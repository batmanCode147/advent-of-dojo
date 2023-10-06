type Move =
    | North
    | South
    | East
    | West 

type Position = { 
    x : int
    y : int
} with
    static member (+)(left: Position, right: Position) =
        { x = (left.x + right.x); y = (left.y + right.y)} 

let makeMove(move: Move) : Position =
   match move with
    | North -> { x = 0; y = 1}
    | South -> { x = 0; y = -1}
    | East -> { x = 1; y = 0}
    | West  -> { x = -1; y = 0}

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
    ({x=0;y=0}, input)
    ||> Seq.scan(fun current move -> 
            current + makeMove move
        ) 
    |> Set.ofSeq
    |> Set.add {x=0;y=0}
    |> Set.count

let deliver (input : seq<Move>) : seq<Position> =
    ({x=0;y=0}, input)
    ||> Seq.scan(fun current move -> 
            current + makeMove move
    )

let nthElement (input : seq<_>) (n : int) : seq<_> =
    input
    |> Seq.indexed
    |> Seq.filter(fun (idx, _) -> idx % n = 0 )
    |> Seq.map snd

let part2v5 (input : seq<_>) : int =
    (({x = 0; y = 0}, {x = 0 ; y = 0}), input
    |> Seq.indexed)
    ||> Seq.scan(fun (santa, robot) (idx, move) -> 
        if idx % 2 = 0 then
            (santa + makeMove move, robot)
        else 
            (santa, robot + makeMove move)
    )
    |> Seq.collect(fun (santa, robot) -> [santa; robot])
    |> Set.ofSeq 
    |> Set.add {x=0; y=0}
    |> Set.count

let part2alt (input : list<Move>) : int =
    let santa = 
        nthElement input 2
    let robot =
        nthElement (Seq.skip 1 input) 2

    Seq.append (deliver santa) (deliver robot)
    |> Set.ofSeq
    |> Set.add {x=0; y=0}
    |> Set.count

let part2 (input : list<Move>) : int =
    let santa = 
        input
        |> List.indexed
        |> List.filter(fun (idx, move) -> idx % 2 = 0)
        |> List.map snd
    let robot =
        input
        |> List.indexed
        |> List.filter(fun (idx, move) -> idx % 2 = 1)
        |> List.map snd

    Seq.append (deliver santa) (deliver robot)
    |> Set.ofSeq
    |> Set.add {x=0; y=0}
    |> Set.count
    
if fsi.CommandLineArgs.Length = 2 then
    let file = fsi.CommandLineArgs[1]
    System.IO.File.ReadAllText(file)
    |> parse
    |> part2v5
    |> printfn "%i"
else
    [
        "^v", 3
        "^>v<", 3
        "^v^v^v^v^v", 11
    ]
    |> Seq.map (fun (input, output) -> parse input, output)
    |> Seq.iter (fun (input, output) ->
        if part2v5 input = output then
            printfn "yay"
        else
            printfn "nay"
    )
