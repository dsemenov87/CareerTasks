open System
open ArrayAndStrings

[<EntryPoint>]
let main argv =
    let success = areEachSymbolMeetsOnce argv.[0]
    printfn "Result: %A" success
    0
