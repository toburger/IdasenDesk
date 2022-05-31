open Argu
open Spectre.Console

type MoveArg =
    | [<MainCommand>] TargetHeight of float

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | TargetHeight _ -> "The target height"

[<DisableHelpFlagsAttribute>]
type NoopArg =
    | [<Hidden>] Noop

    interface IArgParserTemplate with
        member _.Usage = ""

type CatchAllArg =
    | [<Hidden; GatherUnrecognizedAttribute>] Rest of string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Rest _ -> "Provide parameters specific to ASP.NET Core"

type Argument =
    | [<CliPrefix(CliPrefix.None)>] Move of ParseResults<MoveArg>
    | [<CliPrefix(CliPrefix.None)>] Height of ParseResults<NoopArg>
    | [<CliPrefix(CliPrefix.None)>] Discover of ParseResults<NoopArg>
    | [<CliPrefix(CliPrefix.None)>] Serve of ParseResults<CatchAllArg>
    | [<Inherit; MainCommand; First>] BluetoothAddress of uint64

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Move _ -> "Move the desktop to the desired height"
            | Height _ -> "Read the current height"
            | Discover _ -> "Discover your Idåsen Desks"
            | Serve _ -> "Start a web server that listens on a specific port"
            | BluetoothAddress _ -> "The address"

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create(errorHandler = ProcessExiter())
    let result = parser.ParseCommandLine(argv)
    match result.GetSubCommand() with
    | Move moveArgs ->
        let targetHeight = moveArgs.GetResult <@ TargetHeight @>
        let bluetoothAddress = result.GetResult <@ BluetoothAddress @>

        printfn $"Moving to %0.2f{targetHeight}"
        match (Idasen.moveToTargetHeight targetHeight bluetoothAddress).Result with
        | Ok height ->
            printfn $"DONE: %0.2f{height}"
            0
        | Error err ->
            printfn $"%A{err}"
            -1

    | Height _ ->
        let bluetoothAddress = result.GetResult <@ BluetoothAddress @>

        let heightTask = Idasen.getHeight bluetoothAddress
        match heightTask.Result with
        | Ok height ->
            printfn $"The height is: %0.3f{height}m"
            0
        | Error err ->
            eprintfn $"%A{err}"
            -1

    | Discover _ ->
        let watcher = BLE.BLEAdvertisementWatcher()
        watcher.Start()
        printfn "Discovering..."
        let table = Table()
        AnsiConsole.Live(table).Start(fun ctx ->
            table.AddColumn(TableColumn "Address") |> ignore
            table.AddColumn(TableColumn("Name", Width = 20)) |> ignore
            table.AddColumn(TableColumn "Is Idåsen desk") |> ignore

            ctx.Refresh()

            for _ in 1..5 do
                System.Threading.Thread.Sleep 1000

                table.Rows.Clear()
                for device in watcher.Devices do
                    let isDesk =
                        match (Idasen.isDesk device.Address).Result with
                        | Ok true -> ":check_mark_button:"
                        | _ -> ""
                    table.AddRow(
                        Markup(string device.Address),
                        Markup device.Name,
                        Markup(isDesk).Centered()
                    ) |> ignore

                ctx.Refresh()

        )
        watcher.Stop()
        0

    | Serve _ ->
        WebServer.serve argv
        0

    // Case can never happen...
    | BluetoothAddress _ ->
        -100

