namespace MonoDevelopTests
open System
open System.IO
open System.Reflection
open System.Runtime.CompilerServices
open System.Threading.Tasks
open Mono.Addins
open MonoDevelop.Core
open MonoDevelop.Core.Instrumentation
open MonoDevelop.Core.ProgressMonitoring
open MonoDevelop.Ide
open MonoDevelop.Ide.Projects
open MonoDevelop.Ide.Templates
open MonoDevelop.Projects
open NUnit.Framework

[<TestFixture>]
type ``Template tests``() =
    let toTask computation : Task = Async.StartAsTask computation :> _

    let monitor = new ConsoleProgressMonitor()
    do
        FixtureSetup.initialiseMonoDevelop()
        let getField name =
            typeof<IdeApp>.GetField(name, BindingFlags.NonPublic ||| BindingFlags.Static)

        let workspace = getField "workspace"
        workspace.SetValue(null, new RootWorkspace())
        let workbench = getField "workbench"
        workbench.SetValue(null, new MonoDevelop.Ide.Gui.Workbench())
        IdeApp.Preferences.MSBuildVerbosity.Value <- MonoDevelop.Projects.MSBuild.MSBuildVerbosity.Minimal

    let templateService = TemplatingService()
    let templateMatch (template:SolutionTemplate) = 
        template.IsMatch (SolutionTemplateVisibility.All)

    let predicate = new Predicate<SolutionTemplate>(templateMatch)

    let rec flattenCategories (category: TemplateCategory) = 
        printfn "%A" category.Id
        seq {
            yield category
            yield! category.Categories |> Seq.collect flattenCategories
        }

    let solutionTemplates =
        templateService.GetProjectTemplateCategories (predicate)
        |> Seq.collect flattenCategories
        |> Seq.collect(fun c -> c.Templates)
        |> Seq.choose(fun s -> s.GetTemplate("F#") |> Option.ofObj)
        |> Seq.filter(fun t -> t.Id.IndexOf("SharedAssets") = -1) // shared assets projects can't be built standalone
        |> List.ofSeq

    let packageInstallers =
        AddinManager.GetExtensionObjects ("/MonoDevelop/Ide/ProjectTemplatePackageInstallers")
        |> Seq.cast<ProjectTemplatePackageInstaller>

    let profileMap =
        Map.ofList [".NETPortable", "portable-net45+netcore45+wp8"
                    ".NETFramework", "net40"
                    "MonoAndroid" , "portable-net45+netcore45+wp8"]

    member x.Templates =
        solutionTemplates |> Seq.map (fun t -> t.Id)

    [<Test;AsyncStateMachine(typeof<Task>)>]
    [<TestCaseSource ("Templates")>]
    member x.``Build every template`` (tt:string) =
        toTask <| async {
            let template = ProjectTemplate.ProjectTemplates |> Seq.find (fun t -> t.Id == tt)
            let dir = FilePath (FilePath(".").FullPath.ToString() / "buildtemplates" / template.Id)
            dir.Delete()
            Directory.CreateDirectory (dir |> string) |> ignore
            let cinfo = new ProjectCreateInformation (ProjectBasePath = dir, ProjectName = tt, SolutionName = tt, SolutionPath = dir)
            cinfo.Parameters.["CreateSharedAssetsProject"] <- "False"
            cinfo.Parameters.["UseUniversal"] <- "True"
            cinfo.Parameters.["UseIPad"] <- "False"
            cinfo.Parameters.["UseIPhone"] <- "False"
            cinfo.Parameters.["CreateiOSUITest"] <- "False"
            cinfo.Parameters.["CreateAndroidUITest"] <- "False"
            cinfo.Parameters.["MinimumOSVersion"] <- "10.0"
            cinfo.Parameters.["AppIdentifier"] <- tt
            cinfo.Parameters.["AndroidMinSdkVersionAttribute"] <- "android:minSdkVersion=\"10\""
            cinfo.Parameters.["AndroidThemeAttribute"] <- ""

            let sln = template.CreateWorkspaceItem (cinfo) :?> Solution

            let getPackages (template:SolutionTemplate) =
                let config = NewProjectConfiguration()
                config.CreateSolution <- true
                config.ProjectName <- tt
                config.SolutionName <- tt
                config.Location <- dir |> string

                let processedTemplate = templateService.ProcessTemplate(template, config, sln.RootFolder)
                let res = 
                    packageInstallers
                    |> Seq.collect(fun _ -> processedTemplate.PackageReferences)
                    |> Seq.collect(fun ref -> ref.PackageReferences)
                    |> List.ofSeq
                res

            let folder = new SolutionFolder()
            let solutionTemplate =
                solutionTemplates 
                |> Seq.find(fun t -> t.Id = tt)

            let project = sln.FindProjectByName(tt) :?> DotNetProject
            let packagesFolder = (dir |> string)/".."/".."/".."/".."/".."/".."/".."/"packages"

            let assemblyReferencesForPackage (package: ProjectTemplatePackageReference) =
                let profile = profileMap.[project.TargetFramework.Id.Identifier]
                let nameAndVersion = sprintf "%s.%s" package.Id package.Version
                let foldersToScan = [packagesFolder/nameAndVersion/"lib"/"portable-win+net45+wp80+win81+wpa81+MonoAndroid10+MonoTouch10+Xamarin.iOS10" //XF
                                     packagesFolder/nameAndVersion/"lib"/profile
                                     packagesFolder/nameAndVersion/"lib"]

                let baseFolder = foldersToScan |> List.find Directory.Exists

                Directory.GetFiles(baseFolder, "*.dll", SearchOption.AllDirectories)
                |> Array.map(fun file -> ProjectReference.CreateCustomReference(ReferenceType.Assembly, package.Id, file))

            let refs = 
                getPackages solutionTemplate
                |> Seq.collect assemblyReferencesForPackage
                |> Seq.distinct

            for ref in refs do
                project.References.Add ref

            do! sln.SaveAsync(monitor) |> Async.AwaitTask

            let! result = sln.Build(monitor, sln.DefaultConfigurationSelector, null) |> Async.AwaitTask
            match result.HasWarnings, result.HasErrors with
            | _, false -> Assert.Pass()
            | _ -> 
                Assert.Fail(sprintf "Build Failed\n%s\n%A" result.CompilerOutput result.Errors)
        }
