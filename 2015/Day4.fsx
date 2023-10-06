
let md5 (input:string) : string =
    use md5 = System.Security.Cryptography.MD5.Create()
    let stringBuilder = System.Text.StringBuilder()
    for byte in md5.ComputeHash(System.Text.Encoding.UTF8.GetBytes(input)) do
        stringBuilder.Append(byte.ToString("x2")) |> ignore
    stringBuilder.ToString()

let part1 (input:string) : int =
    Seq.initInfinite(fun i ->
        i
    )
    |> Seq.map (sprintf "%s%i" input) 
    |> Seq.map(fun s ->
        md5(s)
        |> Seq.take(5)
        |> Seq.forall(fun c ->
            c = '0'
        )
    )
    |> Seq.indexed
    |> Seq.find snd
    |> fst

if fsi.CommandLineArgs.Length = 2 then
    fsi.CommandLineArgs[1]
    |> part1
    |> printfn "%i"
else
    [
        "abcdef", 609043
        "pqrstuv", 1048970
    ]
    |> Seq.map (fun (input, output) -> input, output)
    |> Seq.iter (fun (input, output) ->
        if part1 input = output then
            printfn "yay"
        else
            printfn "nay"
    )
