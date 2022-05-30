open Idasen
open Argu

type MoveArg =
    | [<MainCommand>] TargetHeight of float

    interface IArgParserTemplate with
        member _.Usage = "The target height"

[<DisableHelpFlagsAttribute>]
type NoopArg =
    | [<Hidden>] Noop

    interface IArgParserTemplate with
        member _.Usage = ""

type Argument =
    | [<CliPrefix(CliPrefix.None)>] Move of ParseResults<MoveArg>
    | [<CliPrefix(CliPrefix.None)>] Height of ParseResults<NoopArg>

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Move _ -> "Move the desktop to the desired height"
            | Height _ -> "Read the current height"

[<EntryPoint>]
let main argv =
    let bluetoothAddress = 218660386811270UL

    let parser = ArgumentParser.Create(errorHandler = ProcessExiter())

    let result = parser.ParseCommandLine(argv)

    match result.GetSubCommand() with
    | Move moveArgs ->
        let targetHeight = moveArgs.GetResult <@ TargetHeight @>

        printfn $"Moving to %0.2f{targetHeight}"

        match (moveToTarget targetHeight bluetoothAddress).Result with
        | Ok () ->
            printfn "DONE"
            0
        | Error err ->
            printfn $"%A{err}"
            -1

    | Height _ ->
        let heightTask = getHeight bluetoothAddress
        match heightTask.Result with
        | Ok height ->
            printfn $"The height is: %0.3f{height}m"
            0
        | Error err ->
            eprintfn $"%A{err}"
            -1

