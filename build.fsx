#r "paket:
nuget BlackFox.Fake.BuildTask
nuget Fake.Core.Target
nuget Fake.Core.Process
nuget Fake.Core.ReleaseNotes
nuget Fake.IO.FileSystem
nuget Fake.DotNet.Cli
nuget Fake.DotNet.MSBuild
nuget Fake.DotNet.AssemblyInfoFile
nuget Fake.DotNet.Paket
nuget Fake.DotNet.FSFormatting
nuget Fake.DotNet.Fsi
nuget Fake.DotNet.NuGet
nuget Fake.Api.Github
nuget Fake.DotNet.Testing.Expecto 
nuget Fake.Tools.Git //"

#if !FAKE
#load "./.fake/build.fsx/intellisense.fsx"
#r "netstandard" // Temp fix for https://github.com/dotnet/fsharp/issues/5216
#endif

open BlackFox.Fake
open System.IO
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Tools

[<AutoOpen>]
/// user interaction prompts for critical build tasks where you may want to interrupt when you see wrong inputs.
module MessagePrompts =

    let prompt (msg:string) =
        System.Console.Write(msg)
        System.Console.ReadLine().Trim()
        |> function | "" -> None | s -> Some s
        |> Option.map (fun s -> s.Replace ("\"","\\\""))

    let rec promptYesNo msg =
        match prompt (sprintf "%s [Yn]: " msg) with
        | Some "Y" | Some "y" -> true
        | Some "N" | Some "n" -> false
        | _ -> System.Console.WriteLine("Sorry, invalid answer"); promptYesNo msg

    let releaseMsg = """This will stage all uncommitted changes, push them to the origin and bump the release version to the latest number in the RELEASE_NOTES.md file. 
        Do you want to continue?"""

    let releaseDocsMsg = """This will push the docs to gh-pages. Remember building the docs prior to this. Do you want to continue?"""

/// Executes a dotnet command in the given working directory
let runDotNet cmd workingDir =
    let result =
        DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd ""
    if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s" cmd workingDir

/// Metadata about the project
module ProjectInfo = 

    let project = "BioFSharp.Vis"

    let summary = "Programmatic access to Biological Databases from F#"

    let configuration = "Release"

    // Git configuration (used for publishing documentation in gh-pages branch)
    // The profile where the project is posted
    let gitOwner = "CSBiology"
    let gitName = "BioFSharp.Vis"

    let solutionFile = "BioFSharp.Vis.sln"

    let gitHome = sprintf "%s/%s" "https://github.com" gitOwner

    let projectRepo = sprintf "%s/%s/%s" "https://github.com" gitOwner gitName

    let website = "/BioFSharp.Vis"

    let testProject = "tests/BioFSharp.Vis.Tests/BioFSharp.Vis.Tests.fsproj"

    let pkgDir = "pkg"

    let release = ReleaseNotes.load "RELEASE_NOTES.md"

    let stableVersion = SemVer.parse release.NugetVersion

    let stableVersionTag = (sprintf "%i.%i.%i" stableVersion.Major stableVersion.Minor stableVersion.Patch )

    let mutable prereleaseSuffix = ""

    let mutable prereleaseTag = ""

    let mutable isPrerelease = false

/// Barebones, minimal build tasks
module BasicTasks = 

    open ProjectInfo

    let setPrereleaseTag = BuildTask.create "SetPrereleaseTag" [] {
        printfn "Please enter pre-release package suffix"
        let suffix = System.Console.ReadLine()
        prereleaseSuffix <- suffix
        prereleaseTag <- (sprintf "%s-%s" release.NugetVersion suffix)
        isPrerelease <- true
    }

    let clean = BuildTask.create "Clean" [] {
        !! "src/**/bin"
        ++ "src/**/obj"
        ++ "tests/**/bin"
        ++ "tests/**/obj"
        ++ "pkg"
        ++ "bin"
        |> Shell.cleanDirs 
    }

    let restore = BuildTask.create "Restore" [clean] {
        !! "src/**/*.*proj"
        ++ "tests/**/*.*proj"
        |> Seq.iter (DotNet.restore id)
    }

    let build = BuildTask.create "Build" [clean; restore] {
        let setParams (defaults:MSBuildParams) =
            { defaults with
                Verbosity = Some(Quiet)
                Targets = ["Build"]
                Properties =
                    [
                        "Optimize", "True"
                        "DebugSymbols", "True"
                        "Configuration", configuration
                        "VersionPrefix", release.NugetVersion
                        if isPrerelease then ("VersionSuffix",prereleaseSuffix)
                    ]
             }
        MSBuild.build setParams solutionFile
    }

    let copyBinaries = BuildTask.create "CopyBinaries" [clean; restore; build] {
        let targets = 
            !! "src/**/*.??proj"
            -- "src/**/*.shproj"
            |>  Seq.map (fun f -> ((Path.getDirectory f) </> "bin" </> configuration, "bin" </> (Path.GetFileNameWithoutExtension f)))
        for i in targets do printfn "%A" i
        targets
        |>  Seq.iter (fun (fromDir, toDir) -> Shell.copyDir toDir fromDir (fun _ -> true))
    }

/// Test executing build tasks
module TestTasks = 

    open ProjectInfo
    open BasicTasks

    let testAssemblies = "tests/**/bin" </> configuration </> "**" </> "*Tests.exe"

    let runTests = BuildTask.create "runTests" [clean; restore; build; copyBinaries] {
        let assemblies = !! testAssemblies

        assemblies
        |> Seq.iter (fun f ->
            Command.RawCommand (
                f,
                Arguments.OfArgs []
            )
            |> CreateProcess.fromCommand
            |> CreateProcess.withFramework
            |> CreateProcess.ensureExitCode
            |> Proc.run
            |> ignore
        )
    }

/// Package creation
module PackageTasks = 

    open ProjectInfo

    open BasicTasks
    open TestTasks

    let pack = BuildTask.create "Pack" [clean; restore; build; runTests; copyBinaries] {
        if promptYesNo (sprintf "creating stable package with version %s OK?" stableVersionTag ) 
            then
                !! "src/**/*.*proj"
                |> Seq.iter (Fake.DotNet.DotNet.pack (fun p ->
                    let msBuildParams =
                        {p.MSBuildParams with 
                            Properties = ([
                                "Version",stableVersionTag
                                "PackageReleaseNotes",  (release.Notes |> String.concat "\r\n")
                                "TargetFrameworks","net47"
                            ] @ p.MSBuildParams.Properties)
                        }
                    {
                        p with 
                            MSBuildParams = msBuildParams
                            OutputPath = Some pkgDir
                    }
                ))
        else failwith "aborted"
    }

    let packPrerelease = BuildTask.create "PackPrerelease" [setPrereleaseTag; clean; restore; build; runTests; copyBinaries] {
        if promptYesNo (sprintf "package tag will be %s OK?" prereleaseTag )
            then 
                !! "src/**/*.*proj"
                //-- "src/**/Plotly.NET.Interactive.fsproj"
                |> Seq.iter (Fake.DotNet.DotNet.pack (fun p ->
                            let msBuildParams =
                                {p.MSBuildParams with 
                                    Properties = ([
                                        "Version", prereleaseTag
                                        "PackageReleaseNotes",  (release.Notes |> String.toLines )
                                        "TargetFrameworks","net47"
                                    ] @ p.MSBuildParams.Properties)
                                }
                            {
                                p with 
                                    VersionSuffix = Some prereleaseSuffix
                                    OutputPath = Some pkgDir
                                    MSBuildParams = msBuildParams
                            }
                ))
        else
            failwith "aborted"
    }

open BasicTasks
open TestTasks
open PackageTasks

BuildTask.runOrDefault pack