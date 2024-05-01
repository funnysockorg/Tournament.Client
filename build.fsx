#r "nuget: Fake.Core.Target, 6.0"
#r "nuget: Fake.DotNet.Cli, 6.0"
#r "nuget: Fake.IO.FileSystem, 6.0"

open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators

// Boilerplate
System.Environment.GetCommandLineArgs()
|> Array.skip 2 // skip fsi.exe; build.fsx
|> Array.toList
|> Fake.Core.Context.FakeExecutionContext.Create false __SOURCE_FILE__
|> Fake.Core.Context.RuntimeContext.Fake
|> Fake.Core.Context.setExecutionContext

Target.initEnvironment ()

let mainPath = Path.getFullName "./src"
let mainFableOutput = mainPath </> "binfable"
let mainDeployDir = Path.getFullName "./deploy"
let testsPath = Path.getFullName "./tests"
let testsFableOutput = testsPath </> "binfable"
let testsDeployDir = testsPath </> "./deploy"

let npm args workingDir =
    let npmPath =
        match ProcessUtils.tryFindFileOnPath "npm" with
        | Some path -> path
        | None ->
            "npm was not found in path. Please install it and make sure it's available from your path. " +
            "See https://safe-stack.github.io/docs/quickstart/#install-pre-requisites for more info"
            |> failwith

    let arguments = args |> String.split ' ' |> Arguments.OfArgs

    Command.RawCommand (npmPath, arguments)
    |> CreateProcess.fromCommand
    |> CreateProcess.withWorkingDirectory workingDir
    |> CreateProcess.ensureExitCode
    |> Proc.run
    |> ignore

let dotnet cmd workingDir =
    let result = DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd ""
    if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s" cmd workingDir

module Fable =
    type BuildOption =
        {
            Output: string
            IsWatch: bool
            NoRestore: bool
        }
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module BuildOption =
        let empty: BuildOption =
            {
                Output = ""
                IsWatch = false
                NoRestore = false
            }

    let run fableArgs =
        dotnet (sprintf "fable %s" (String.concat " " fableArgs)) "."

    let build (fableOption: BuildOption) projectPath =
        [
            yield projectPath

            if fableOption.NoRestore then
                yield "--noRestore"

            yield
                match fableOption.Output with
                | null | "" -> ""
                | output -> sprintf "-o %s" output

            if fableOption.IsWatch then
                yield "--watch"
        ]
        |> run

    let clean outputPath =
        if Shell.testDir outputPath then
            [
                "clean"
                sprintf "-o %s" outputPath
                "--yes"
            ]
            |> run

let removeBinAndObj path =
    Shell.cleanDirs [
        path </> "bin"
        path </> "obj"
    ]

// --------------------------------------------------------------------------------------
// Main

Target.create "MainClean" (fun _ ->
    removeBinAndObj mainPath
)

Target.create "MainDeployClean" (fun _ ->
    Shell.cleanDir mainDeployDir
)

Target.create "MainInstall" (fun _ -> npm "install" ".")

Target.create "MainFemto" (fun _ ->
    dotnet (sprintf "femto %s --resolve" mainPath) "."
)

Target.create "MainFableBuild" (fun _ ->
    mainPath
    |> Fable.build
        { Fable.BuildOption.empty with
            Output = mainFableOutput
        }
)

Target.create "MainFableWatch" (fun _ ->
    mainPath
    |> Fable.build
        { Fable.BuildOption.empty with
            Output = mainFableOutput
            IsWatch = true
        }
)

Target.create "MainFableClean" (fun _ ->
    Fable.clean mainFableOutput
)

Target.create "MainViteWatch" (fun _ ->
    npm "run vite:watch" "."
)

Target.create "MainViteBuild" (fun _ ->
    npm "run vite:build" "."
)

Target.create "MainWebWatchInternal" (fun _ -> ())

Target.create "MainWebWatch" (fun _ ->
    Target.run 2 "MainWebWatchInternal" []
)

Target.create "MainDeploy" (fun _ -> ())

Target.create "MainDotnetBuild" (fun _ ->
    dotnet "build" mainPath
)

// --------------------------------------------------------------------------------------
// Tests
Target.create "TestsClean" (fun _ ->
    removeBinAndObj testsPath
)

Target.create "TestsDeployClean" (fun _ ->
    Shell.cleanDir testsDeployDir
)

Target.create "TestsInstall" (fun _ -> npm "install" ".")

Target.create "TestsDotnetBuild" (fun _ ->
    dotnet "build" testsPath
)

Target.create "TestsDotnetRun" (fun _ ->
    dotnet "run" testsPath
)

Target.create "TestsFableBuild" (fun _ ->
    testsPath
    |> Fable.build
        { Fable.BuildOption.empty with
            Output = testsFableOutput
        }
)

Target.create "TestsFableWatch" (fun _ ->
    testsPath
    |> Fable.build
        { Fable.BuildOption.empty with
            Output = testsFableOutput
            IsWatch = true
        }
)

Target.create "TestsFableClean" (fun _ ->
    Fable.clean testsFableOutput
)

Target.create "TestsViteWatch" (fun _ ->
    npm "run tests:vite:watch" "."
)

Target.create "TestsViteBuild" (fun _ ->
    npm "run tests:vite:build" "."
)

Target.create "TestsWebWatchInternal" (fun _ -> ())

Target.create "TestsWebWatch" (fun _ ->
    Target.run 2 "TestsWebWatchInternal" []
)

Target.create "TestsDeploy" (fun _ -> ())

// --------------------------------------------------------------------------------------
// Overall dotnet build
Target.create "DotnetBuild" (fun _ -> ())

Target.create "DotnetCleanBuild" (fun _ -> ())

// --------------------------------------------------------------------------------------
// Overall clean
Target.create "DotnetClean" (fun _ -> ())

Target.create "Clean" (fun _ -> ())

open Fake.Core.TargetOperators

// --------------------------------------------------------------------------------------
// Main

// Main clean:
"MainClean" ==> "DotnetClean"

// Tests dotnet build:
"MainClean" ?=> "MainDotnetBuild"
"MainDotnetBuild" ==> "DotnetBuild"

// Main web deploy:
"MainDeployClean" ==> "MainDeploy"
"MainInstall" ==> "MainFableBuild"
"MainFableClean" ==> "MainFableBuild"
"MainInstall" ?=> "MainFableClean"
"MainFableBuild"
    ==> "MainViteBuild"
    ==> "MainDeploy"

// Main web run:
"MainFableWatch" ==> "MainWebWatchInternal"
"MainViteWatch" ==> "MainWebWatchInternal"

// --------------------------------------------------------------------------------------
// Tests

// Tests clean:
"TestsClean" ==> "DotnetClean"

// Tests dotnet build:
"TestsClean" ?=> "TestsDotnetBuild"
"TestsDotnetBuild" ==> "DotnetBuild"

// Tests web deploy:
"TestsDeployClean" ==> "TestsDeploy"
"TestsInstall" ==> "TestsFableBuild"
"TestsFableClean" ==> "TestsFableBuild"
"TestsInstall" ?=> "TestsFableClean"
"TestsFableBuild"
    ==> "TestsViteBuild"
    ==> "TestsDeploy"

// Tests web run:
"TestsFableWatch" ==> "TestsWebWatchInternal"
"TestsViteWatch" ==> "TestsWebWatchInternal"

// --------------------------------------------------------------------------------------
// Overall dotnet build
"DotnetClean" ==> "DotnetCleanBuild"
"DotnetBuild" ==> "DotnetCleanBuild"
"DotnetClean" ?=> "DotnetBuild"

// --------------------------------------------------------------------------------------
// Overall clean
"DotnetClean" ==> "Clean"
"MainFableClean" ==> "Clean"
"TestsFableClean" ==> "Clean"

Target.runOrDefaultWithArguments "MainDeploy"
