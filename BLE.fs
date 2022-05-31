module BLE

open System
open Windows.Devices.Bluetooth
open Windows.Devices.Bluetooth.Advertisement
open Windows.Devices.Bluetooth.GenericAttributeProfile
open Windows.Storage.Streams

type BluetoothError =
    | InvalidDevice of bluetoothAddress: uint64
    | InvalidService of bluetoothAddress: uint64 * serviceUuid: Guid
    | CouldNotOpenService of bluetoothAddress: uint64 * serviceUuid: Guid * status: GattOpenStatus
    | InvalidCharacteristic of bluetoothAddress: uint64 * serviceUuid: Guid * characteristicUuid: Guid
    | CouldNotReadBytes of bluetoothAddress: uint64 * serviceUuid: Guid * characteristicUuid: Guid
    | CouldNotWriteBytes of bluetoothAddress: uint64 * serviceUuid: Guid * characteristicUuid: Guid * bytes: byte[]

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
        return Error (CouldNotWriteBytes (characteristic.Service.Device.BluetoothAddress, characteristic.Service.Uuid, characteristic.Uuid, bytes))
}

let listAllServices (device: BluetoothLEDevice) =
    device.GattServices

let listAllCharacteristics (service: GattDeviceService) =
    service.GetAllCharacteristics()

type Device =
    { Address: uint64
      Name: string }

type Message =
    | ReceiveDevice of Device
    | RemoveDevice of uint64
    | UpdateDevice of Device
    | UpdateDeviceName of Device
    | GetDevices of AsyncReplyChannel<Device list>

type BLEAdvertisementWatcher () =
    let deviceUpdatedEvent = Event<Device>()
    let deviceNameUpdatedEvent = Event<Device>()
    let readValue (buffer: IBuffer) =
        use reader = DataReader.FromBuffer(buffer)
        let input = Array.zeroCreate<byte> (int reader.UnconsumedBufferLength)
        reader.ReadBytes(input)
        input
    let agent = MailboxProcessor.Start(fun agent ->
        let rec loop devices = async {
            match! agent.Receive() with
            | GetDevices r ->
                r.Reply [ for KeyValue(_, device) in devices -> device ]
                return! loop devices
            | ReceiveDevice updatedDevice ->
                let newDevices =
                    match Map.tryFind updatedDevice.Address devices with
                    | Some existingDevice ->
                        // Device already in map and not the same
                        let updatedDevice =
                            { updatedDevice with
                                Name =
                                    // Set the name to a non empty string if possible
                                    // Some devices send device infos with a name,
                                    // then without a name, then again with a name, and so on...
                                    if updatedDevice.Name <> "" then
                                        updatedDevice.Name
                                    else
                                        existingDevice.Name }
                        // Check if device data is different
                        if existingDevice <> updatedDevice then
                            agent.Post(UpdateDevice updatedDevice)
                            if updatedDevice.Name <> existingDevice.Name then
                                // Send name changed
                                agent.Post(UpdateDeviceName updatedDevice)
                            Map.add existingDevice.Address updatedDevice devices
                        else
                            // Unchanged device data
                            devices
                    | None ->
                        // Device new
                        agent.Post(UpdateDevice updatedDevice)
                        agent.Post(UpdateDeviceName updatedDevice)
                        Map.add updatedDevice.Address updatedDevice devices
                return! loop newDevices
            | RemoveDevice deviceId ->
                let newDevices = Map.remove deviceId devices
                return! loop newDevices
            | UpdateDevice device ->
                deviceUpdatedEvent.Trigger(device)
                return! loop devices
            | UpdateDeviceName device ->
                deviceNameUpdatedEvent.Trigger(device)
                return! loop devices
        }
        loop Map.empty
    )
    let watcher = BluetoothLEAdvertisementWatcher()
    do watcher.add_Received(fun _ args ->
        if args.AdvertisementType = BluetoothLEAdvertisementType.ScanResponse then
            let info =
                { Name = args.Advertisement.LocalName
                  Address = args.BluetoothAddress }
            agent.Post(ReceiveDevice info)
    )
    [<CLIEvent>]
    member _.DeviceChanged = deviceUpdatedEvent.Publish
    /// Does only trigger for devices with names (devices with empty names are ignored).
    [<CLIEvent>]
    member _.DeviceNameChanged = deviceNameUpdatedEvent.Publish
    member _.Devices = agent.PostAndReply(fun r -> GetDevices r)
    member _.Start() = watcher.Start()
    member _.Stop() = watcher.Stop()

