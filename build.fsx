#r "paket:
nuget Fake.Core.Target
nuget Fake.Core.ReleaseNotes
nuget Fake.Dotnet.AssemblyInfoFile
nuget Fake.DotNet.Cli
nuget Fake.DotNet.Paket
nuget Fake.IO.FileSystem //"
#load ".fake/build.fsx/intellisense.fsx"

open System
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
            Copyright = Some $"Copyright {DateTimeOffset.UtcNow.Year}"
            Tags = [ "domain"; "model"; "secure"; "trust"; "boundaries"; "blocks"; "reusable"; "fsharp" ]
            Files = 
                [ Include ("bin/Release/netstandard2.1/*.dll", "lib/netstandard2.1")
                  Include ("bin/Release/netstandard2.1/*.xml", "lib/netstandard2.1")
                  Include ("bin/Release/net6.0/*.dll", "lib/net6.0")
                  Include ("bin/Release/net6.0/*.xml", "lib/net6.0") ]
            Dependencies = 
                Paket.getDependenciesForReferencesFile referencesFile
                |> Array.map (fun (package, version) -> PaketDependency (package, GreaterOrEqual (Version version)))
                |> List.ofArray })

    Paket.pack (fun defaults ->
        { defaults with
            ToolPath = "./.paket/paket.exe"
            OutputPath = "bin"
            WorkingDir = "."
            TemplateFile = templateFile })

Target.create "All" ignore

"Clean"
  ==> "Build"
  ==> "Tests"
  ==> "Paket"
  ==> "All"

Target.runOrDefault "All"
