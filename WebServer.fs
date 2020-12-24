open System
open System.Threading
open Suave
open Suave.Filters 
open Suave.Operators 
open Suave.Successful
open AmigoSecreto.Sorteing


[<EntryPoint>]
let main argv = 
  let cts = new CancellationTokenSource()
       

  let conf = { defaultConfig with cancellationToken = cts.Token}

  let app = choose [ 
      GET >=> choose [ path "/" >=> OK testApi
                       path "/hello" >=> OK "Hello GET" 
                       path "/goodbye" >=> OK "Good bye GET" ] 
      POST >=> choose [ path "/hello" >=> OK "Hello POST" 
                        path "/goodbye" >=> OK "Good bye POST" ] 
    ] 

  let listening, server = startWebServerAsync conf app
    
  Async.Start(server, cts.Token)
  printfn "Make requests now"
  Console.ReadKey true |> ignore
    
  cts.Cancel()

  0 // return an integer exit code