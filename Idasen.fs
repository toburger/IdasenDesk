module Idasen

open System
open FsToolkit.ErrorHandling
open BLE

type IdasenError =
    | MaxHeightExceeded of maxHeight: float * currentHeight: float
    | MinHeightExceeded of minHeight: float * currentHeight: float
    | SafetyFeatureKickedIn

type Error =
    | BluetoothError of BLE.BluetoothError
    | IdasenError of IdasenError

type Constants =
    static member UUID_HEIGHT_SVC      = Guid "99fa0020-338a-1024-8a49-009c0215f78a"
    static member UUID_HEIGHT          = Guid "99fa0021-338a-1024-8a49-009c0215f78a"
    static member UUID_COMMAND         = Guid "99fa0002-338a-1024-8a49-009c0215f78a"
    static member UUID_REFERENCE_INPUT = Guid "99fa0031-338a-1024-8a49-009c0215f78a"
    static member UUID_ADV_SVC         = Guid "99fa0001-338a-1024-8a49-009c0215f78a"

    static member COMMAND_REFERENCE_INPUT_STOP = [| 0x01uy; 0x80uy |]
    static member COMMAND_UP = [| 0x47uy; 0x00uy |]
    static member COMMAND_DOWN = [| 0x46uy; 0x00uy |]
    static member COMMAND_STOP = [| 0xFFuy; 0x00uy |]

    static member MIN_HEIGHT = 0.62
    static member MAX_HEIGHT = 1.27
    static member RETRY_COUNT = 3

/// Height calculation offset in meters, assumed to be the same for all desks
/// Converts a value read from the desk in bytes to meters.
let bytesToMeters (raw: byte[]): float =
    let rawLength = raw.Length
    let expectedLength = 4
    assert (rawLength = expectedLength)

    let highByte = int raw[1]
    let lowByte = int raw[0]
    let intRaw = (highByte <<< 8) + lowByte
    (float intRaw / 10_000.0) + Constants.MIN_HEIGHT

let isDesk bluetoothAddress = taskResult {
    let! device = getDevice bluetoothAddress
    return
        listAllServices device
        |> Seq.exists (fun s -> s.Uuid = Constants.UUID_ADV_SVC)
}

let getHeight bluetoothAddress = taskResult {
    let! device = getDevice bluetoothAddress
    let! service = getGattService Constants.UUID_HEIGHT_SVC device
    let! characteristic = getCharacteristic Constants.UUID_HEIGHT service
    let! bytes = readBytes characteristic
    return bytesToMeters bytes
}

let move bytes bluetoothAddress = taskResult {
    let! device = getDevice bluetoothAddress
    let! service = getGattService Constants.UUID_ADV_SVC device
    let! characteristic = getCharacteristic Constants.UUID_COMMAND service
    do! writeBytes bytes characteristic
}

let moveUp bluetoothAddress =
    move Constants.COMMAND_UP bluetoothAddress

let moveDown bluetoothAddress =
    move Constants.COMMAND_DOWN bluetoothAddress

let stop bluetoothAddress =
    move Constants.COMMAND_STOP bluetoothAddress

let moveToTargetHeight targetHeight bluetoothAddress = taskResult {
    let getHeight = getHeight >> TaskResult.mapError BluetoothError
    let moveUp = moveUp >> TaskResult.mapError BluetoothError
    let moveDown = moveDown >> TaskResult.mapError BluetoothError
    let stop = stop >> TaskResult.mapError BluetoothError

    if targetHeight > Constants.MAX_HEIGHT then
        return! Error (IdasenError (MaxHeightExceeded (Constants.MAX_HEIGHT, targetHeight)))
    elif targetHeight < Constants.MIN_HEIGHT then
        return! Error (IdasenError (MinHeightExceeded (Constants.MIN_HEIGHT, targetHeight)))
    else
        let mutable previousHeight = 0.0
        let! height = getHeight bluetoothAddress
        previousHeight <- height
        let willMoveUp = targetHeight > previousHeight
        let mutable continue' = Ok true
        while continue' = Ok true do
            let! height = getHeight bluetoothAddress
            let difference = targetHeight - height
            if (height < previousHeight && willMoveUp) ||
               (height > previousHeight && not willMoveUp) then
                continue' <- Error (IdasenError SafetyFeatureKickedIn)
            elif abs difference < 0.005 then //Tolerance of 0.005 meters
                printfn $"Reached target of %0.2f{targetHeight}"
                do! stop bluetoothAddress
                continue' <- Ok false
            elif difference > 0.0 then
                do! moveUp bluetoothAddress
            elif difference < 0.0 then
                do! moveDown bluetoothAddress
            else ()
            previousHeight <- height
        match continue' with
        | Error error -> return! Error error
        | Ok _ -> return! Ok previousHeight
}

