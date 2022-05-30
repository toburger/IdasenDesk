module BLE

open System
open Windows.Devices.Bluetooth
open Windows.Devices.Bluetooth.GenericAttributeProfile
open Windows.Storage.Streams

type BluetoothError =
    | InvalidDevice of bluetoothAddress: uint64
    | InvalidService of bluetoothAddress: uint64 * serviceUuid: Guid
    | CouldNotOpenService of bluetoothAddress: uint64 * serviceUuid: Guid * status: GattOpenStatus
    | InvalidCharacteristic of bluetoothAddress: uint64 * serviceUuid: Guid * characteristicUuid: Guid
    | CouldNotReadBytes of bluetoothAddress: uint64 * serviceUuid: Guid * characteristicUuid: Guid
    | CouldNotWriteBytes of bluetoothAddress: uint64 * serviceUuid: Guid * characteristicUuid: Guid

let getDevice bluetoothAddress = task {
    match! BluetoothLEDevice.FromBluetoothAddressAsync(bluetoothAddress).AsTask() with
    | null -> return Error (InvalidDevice bluetoothAddress)
    | device -> return Ok device
}

let getGattService uuid (device: BluetoothLEDevice) = task {
    let! result = device.GetGattServicesForUuidAsync(uuid).AsTask()
    if result.Status = GattCommunicationStatus.Success then
        match result.Services |> Seq.tryHead with
        | Some service ->
            let sharingMode = GattSharingMode.SharedReadAndWrite
            let! res = service.OpenAsync(sharingMode).AsTask()
            if res = GattOpenStatus.Success then
                return Ok service
            else
                return Error (CouldNotOpenService (device.BluetoothAddress, uuid, res))
        | None ->
            return Error (InvalidService (device.BluetoothAddress, uuid))
    else
        return Error (InvalidService (device.BluetoothAddress, uuid))
}

let getCharacteristic uuid (service: GattDeviceService) = task {
    let! result = service.GetCharacteristicsForUuidAsync(uuid).AsTask()
    if result.Status = GattCommunicationStatus.Success then
        match result.Characteristics |> Seq.tryHead with
        | Some characteristic ->
            return Ok characteristic
        | None ->
            return Error (InvalidCharacteristic (service.Device.BluetoothAddress, service.Uuid, uuid))
        
    else
        return Error (InvalidCharacteristic (service.Device.BluetoothAddress, service.Uuid, uuid))
}

let readBytes (characteristic: GattCharacteristic) = task {
    let! res = characteristic.ReadValueAsync().AsTask()
    if res.Status = GattCommunicationStatus.Success then
        let reader = DataReader.FromBuffer(res.Value)
        let input = Array.zeroCreate (int reader.UnconsumedBufferLength)
        reader.ReadBytes(input)
        return Ok input
    else
        return Error (CouldNotReadBytes (characteristic.Service.Device.BluetoothAddress, characteristic.Service.Uuid, characteristic.Uuid))
}

let writeBytes bytes (characteristic: GattCharacteristic) = task {
    use buffer = new DataWriter()
    buffer.WriteBytes(bytes)
    let! res = characteristic.WriteValueAsync(buffer.DetachBuffer()).AsTask()
    if res = GattCommunicationStatus.Success then
        return Ok ()
    else
        return Error (CouldNotWriteBytes (characteristic.Service.Device.BluetoothAddress, characteristic.Service.Uuid, characteristic.Uuid))
}

let listAllServices (device: BluetoothLEDevice) =
    device.GattServices

let listAllCharacteristics (service: GattDeviceService) =
    service.GetAllCharacteristics()

