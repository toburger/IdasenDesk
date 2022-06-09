module WebServer

open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Giraffe
open System.Text.Json.Serialization

let errorHandler (ex : Exception) (logger : ILogger) =
    logger.LogError(ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

let moveToTargetPositionHandler targetHeight: HttpHandler =
    fun next ctx -> task {
        let options = ctx.GetService<IConfiguration>()
        let bluetoothAddress = options.GetValue<uint64>("BluetoothAddress")
        let! height = Idasen.moveToTargetHeight targetHeight bluetoothAddress
        return! json height next ctx
    }

let moveToStandingPositionHandler: HttpHandler =
    fun next ctx -> task {
        let options = ctx.GetService<IConfiguration>()
        let targetHeight = options.GetSection("Positions").GetValue<float>("Stand")
        return! moveToTargetPositionHandler targetHeight next ctx
    }

let moveToSittingPositionHandler: HttpHandler =
    fun next ctx -> task {
        let options = ctx.GetService<IConfiguration>()
        let targetHeight = options.GetSection("Positions").GetValue<float>("Sit")
        return! moveToTargetPositionHandler targetHeight next ctx
    }

let webApp =
    choose [
        GET >=> choose [
            route "/" >=> text "Welkome to the IKEA Idåsen Desk control web service API"
            route "/height" >=> fun next ctx -> task {
                let options = ctx.GetService<IConfiguration>()
                let bluetoothAddress = options.GetValue<uint64>("BluetoothAddress")
                let! height = Idasen.getHeight bluetoothAddress
                return! json height next ctx
            }
        ]
        POST >=> choose [
            route "/stand" >=> moveToStandingPositionHandler
            route "/sit" >=> moveToSittingPositionHandler
            routef "/set/%f" moveToTargetPositionHandler
        ]
    ]

let configureApp (app: IApplicationBuilder) =
    let env = app.ApplicationServices.GetService<IWebHostEnvironment>()
    if env.IsDevelopment() then
        app.UseDeveloperExceptionPage() |> ignore
    app.UseGiraffeErrorHandler(errorHandler) |> ignore
    app.UseGiraffe(webApp) |> ignore

let configureServices (services: IServiceCollection) =
    services.AddGiraffe() |> ignore
    let serializationOptions = SystemTextJson.Serializer.DefaultOptions
    serializationOptions.Converters.Add(
        JsonFSharpConverter(
            JsonUnionEncoding.FSharpLuLike,
            unionTagNamingPolicy = Text.Json.JsonNamingPolicy.CamelCase))
    services.AddSingleton<Json.ISerializer>(SystemTextJson.Serializer(serializationOptions)) |> ignore

let serve argv =
        Host.CreateDefaultBuilder(argv)
            .ConfigureWebHostDefaults(fun webHostBuilder ->
                webHostBuilder
                    .Configure(Action<IApplicationBuilder> configureApp)
                    .ConfigureServices(configureServices)
                |> ignore)
            .UseWindowsService()
            .Build()
            .Run()
