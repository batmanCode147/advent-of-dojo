
type MoveInstruction =
    | Up
    | Down
 
    static member ToInt = function
        | Up -> 1
        | Down -> -1

let parse (input:string) : list<MoveInstruction> =
    input
    |> Seq.map (function
        | '(' -> Up
        | ')' -> Down
        | _ -> failwith "wrong input"
    )
    |> Seq.toList

let part1 (input:list<MoveInstruction>) : int =
    input
    |> Seq.map MoveInstruction.ToInt
    |> Seq.sum

let part2 (input:list<MoveInstruction>) : int =
    input
    |> Seq.map MoveInstruction.ToInt
    |> Seq.scan (+) 0
    |> Seq.takeWhile (fun x -> 0 <= x)
    |> Seq.length

if fsi.CommandLineArgs.Length = 2 then
    let file = fsi.CommandLineArgs[1]
    System.IO.File.ReadAllText(file)
    |> parse
    |> part2
    |> printfn "%i"
else
    [
        "(())", 0
        "()()", 0
        "(((", 3
        "))(((((",3
        ")())())",-3
    ]
    |> Seq.map (fun (input, output) -> parse input, output)
    |> Seq.iter (fun (input, output) ->
        if part1 input = output then
            printfn "yay"
        else
            printfn "nay"
    )
