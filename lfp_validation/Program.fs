open ResultDemo
open Validator

[<EntryPoint>]
let main argv = 
    // printfn "%A" (messages Validator.validatedCmdLineOptions)
    ResultDemo.mapResult (fun vr -> printfn "%A" vr) (Validator.validatedCmdLineOptions) |> ignore 
    0 // return an integer exit code

