type Move =
    | North
    | South
    | East
    | West 

type Position =
    { x : int
      y : int }
    static member Zero = { x = 0; y = 0 }

let updatePosition (current : Position) (delta : Move) : Position =
    match delta with
    | North -> { current with y = current.y + 1 }
    | South -> { current with y = current.y - 1 }
    | East ->  { current with x = current.x + 1 }
    | West  -> { current with x = current.x - 1 }

let parse (input:string) : list<Move> =
    input
    |> Seq.map (function
        | '^' -> North
        | 'v' -> South
        | '>' -> East
        | '<' -> West
        | _ -> failwith "wrong input"
    )
    |> Seq.toList

let part1 (input:list<Move>) : int =
    (Position.Zero, input)
    ||> Seq.scan updatePosition
    |> Set.ofSeq
    |> Set.add Position.Zero
    |> Set.count

let part2 (input : seq<_>) : int =
    ((Position.Zero, Position.Zero), input)
    ||> Seq.scan (fun (first, second) delta ->
        (second, updatePosition first delta)
    )
    |> Seq.map snd
    |> Set.ofSeq 
    |> Set.add Position.Zero
    |> Set.count

if fsi.CommandLineArgs.Length = 2 then
    let file = fsi.CommandLineArgs[1]
    System.IO.File.ReadAllText(file)
    |> parse
    |> part2
    |> printfn "%i"
else
    [
        "^v", 3
        "^>v<", 3
        "^v^v^v^v^v", 11
    ]
    |> Seq.map (fun (input, output) -> parse input, output)
    |> Seq.iter (fun (input, output) ->
        if part2 input = output then
            printfn "yay"
        else
            printfn "nay"
    )
