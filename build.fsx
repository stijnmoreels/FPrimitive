#load ".fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open Fake.DotNet.PaketTemplate

type Projet =
    { Name : string
      Summary : string
      Description : string list
      GitHubUrl : string
      NuGetUrl : string
      Authors : string list }

let project = 
    { Name = "FPrimitive" 
      Summary = ".NET project to help developers build a correct and more secure domain model."
      Description = 
        [ ".NET project to help developers build a correct and more secure domain model by providing building blocks, standard types and trust boundaries." ]
      GitHubUrl = "https://github.com/stijnmoreels/FPrimitive"
      NuGetUrl = "http://nuget.org/packages/FPrimitive"
      Authors = [ "Stijn Moreels" ] }

let dotnetExePath = "dotnet"
let releaseNotes = ReleaseNotes.load "RELEASE_NOTES.md"
let solution = project.Name + ".sln"

Target.create "Clean" <| fun _ ->
    !! "src/**/bin"
    ++ "src/**/obj"
    ++ "tests/**/bin"
    ++ "tests/**/obj"
    ++ "docs"
    ++ "bin"
    |> Shell.cleanDirs

Target.create "Build" <| fun _ ->
    AssemblyInfoFile.createFSharp ("src/" + project.Name +  "/AssemblyInfo.fs")
        [ AssemblyInfo.Title project.Name
          AssemblyInfo.Description  project.Summary
          AssemblyInfo.Guid "771121cs-r6e3-38c5-98b1-6cec4c286948"
          AssemblyInfo.Product project.Name
          AssemblyInfo.Version releaseNotes.NugetVersion
          AssemblyInfo.FileVersion releaseNotes.NugetVersion ]

    solution 
    |> DotNet.restore id

    !! "src/**/*.fsproj"
    ++ "tests/**/*.fsproj"
    ++ "tests/**/*.csproj"
    |> Seq.iter (DotNet.build (fun defaults ->
        { defaults with 
            Configuration = DotNet.BuildConfiguration.Release }))

Target.create "Tests" <| fun _ ->
    let runTest project =
        DotNet.exec (DotNet.Options.withDotNetCliPath dotnetExePath)
             ("run --project " + project)
             "--summary"
        |> fun r -> if r.ExitCode <> 0 then project + " failed" |> failwith
    
    runTest "tests/FPrimitive.Tests/FPrimitive.Tests.fsproj"

Target.create "Paket" <| fun _ ->
    let templateFile = "src" @@ project.Name @@ "paket.template"
    let referencesFile = "src" @@ project.Name @@ "paket.references"
    PaketTemplate.create (fun defaults ->
        { defaults with 
            TemplateFilePath = Some templateFile
            TemplateType = File
            Id = Some project.Name
            Version = Some releaseNotes.NugetVersion
            Description = project.Description
            Title = Some project.Name
            Authors = project.Authors
            Owners = project.Authors
            ReleaseNotes = releaseNotes.Notes
            Summary = [ project.Summary ]
            ProjectUrl = Some project.GitHubUrl
            LicenseUrl = Some (project.GitHubUrl + "/blob/master/LICENSE.txt")
            IconUrl = Some "https://raw.githubusercontent.com/stijnmoreels/FPrimitive/master/docs/img/logo.png"
            Copyright = Some "Copyright 2019"
            Tags = [ "domain"; "model"; "secure"; "trust"; "boundaries"; "blocks"; "reusable"; "fsharp" ]
            Files = 
                [ Include ("bin/Release/netstandard2.0/*.dll", "lib/netstandard2.0")
                  Include ("bin/Release/netstandard2.0/*.xml", "lib/netstandard2.0") ]
            Dependencies = 
                Paket.getDependenciesForReferencesFile referencesFile
                |> Array.map (fun (package, version) -> PaketDependency (package, GreaterOrEqual (Version version)))
                |> List.ofArray })

    Paket.pack (fun defaults ->
        { defaults with
            OutputPath = "bin"
            WorkingDir = "."
            TemplateFile = templateFile })

Target.create "Docs" <| fun _ ->
    let content    = __SOURCE_DIRECTORY__ @@ "docsrc/content"
    let docsOutput = __SOURCE_DIRECTORY__ @@ "docs"
    let files      = __SOURCE_DIRECTORY__ @@ "docsrc/files"
    let templates  = __SOURCE_DIRECTORY__ @@ "docsrc/tools/templates"
    let formatting = __SOURCE_DIRECTORY__ @@ "packages/formatting/FSharp.Formatting"
    let docTemplate = formatting @@ "templates/docpage.cshtml"
    let root = "/" + project.Name
    let info =
          [ "project-name", project.Name
            "project-author", project.Authors |> List.head
            "project-summary", project.Summary
            "project-github", project.GitHubUrl
            "project-nuget", project.NuGetUrl ]
            
    let layoutRootsAll = System.Collections.Generic.Dictionary<string, string list>()
    layoutRootsAll.Add("en", [ templates
                               formatting @@ "templates"
                               formatting @@ "templates/reference" ])

    File.delete "docsrc/content/release-notes.md"
    Shell.copyFile "docsrc/content/" "RELEASE_NOTES.md"
    Shell.rename "docsrc/content/release-notes.md" "docsrc/content/RELEASE_NOTES.md"

    File.delete "docsrc/content/license.md"
    Shell.copyFile "docsrc/content/" "LICENSE.txt"
    Shell.rename "docsrc/content/license.md" "docsrc/content/LICENSE.txt"

    DirectoryInfo.getSubDirectories (DirectoryInfo.ofPath templates)
    |> Seq.iter (fun d ->
        let name = d.Name
        if name.Length = 2 || name.Length = 3 
        then layoutRootsAll.Add(name, [ templates @@ name
                                        formatting @@ "templates"
                                        formatting @@ "templates/reference" ]))
    Shell.copyRecursive files docsOutput true
    |> Trace.logItems "Copying file: "
    Directory.ensure (docsOutput @@ "content")
    Shell.copyRecursive (formatting @@ "styles") (docsOutput @@ "content") true
    |> Trace.logItems "Copying styles and scripts: "

    let langSpecificPath (lang, path:string) =
        path.Split([|'/'; '\\'|], System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.exists(fun i -> i = lang)
    
    let layoutRoots =
        let key = layoutRootsAll.Keys |> Seq.tryFind (fun i -> langSpecificPath(i, content))
        match key with
        | Some lang -> layoutRootsAll.[lang]
        | None -> layoutRootsAll.["en"]

    Directory.ensure (docsOutput @@ "reference")
    !! ("src/**/bin/Release/**/*.dll") 
    |> FSFormatting.createDocsForDlls (fun args ->
        { args with
            OutputDirectory = docsOutput @@ "reference"
            LayoutRoots =  layoutRootsAll.["en"]
            ProjectParameters =  ("root", root) :: info
            SourceRepository = project.GitHubUrl @@ "tree/master" })

    FSFormatting.createDocs (fun args ->
        { args with
            Source = content
            OutputDirectory = docsOutput
            LayoutRoots = layoutRoots
            ProjectParameters  = ("root", root) :: info
            Template = docTemplate })

Target.create "All" ignore

"Clean"
  ==> "Build"
  ==> "Tests"
  ==> "Docs"
  ==> "Paket"
  ==> "All"

Target.runOrDefault "All"
