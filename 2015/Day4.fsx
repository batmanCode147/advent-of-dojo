
let md5 (input:string) : string =
    use md5 = System.Security.Cryptography.MD5.Create()
    let stringBuilder = System.Text.StringBuilder()
    for byte in md5.ComputeHash(System.Text.Encoding.UTF8.GetBytes(input)) do
        stringBuilder.Append(byte.ToString("x2")) |> ignore
    stringBuilder.ToString()

let findHashWithNLeadingZeroes (input:string, n:int) : int =
    Seq.initInfinite id
    |> Seq.find (fun i ->
        i
        |> sprintf "%s%i" input
        |> md5
        |> Seq.take n
        |> Seq.forall ((=)'0')
    )

let part1 (input:string) : int =
    findHashWithNLeadingZeroes(input, 5)

let part2 (input:string) : int =
    findHashWithNLeadingZeroes(input, 6)

if fsi.CommandLineArgs.Length = 2 then
    fsi.CommandLineArgs[1]
    |> part2
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
