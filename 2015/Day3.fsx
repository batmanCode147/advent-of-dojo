
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

let part2 (input:list<Move>) : int =
    0

if fsi.CommandLineArgs.Length = 2 then
    let file = fsi.CommandLineArgs[1]
    System.IO.File.ReadAllText(file)
    |> parse
    |> part1
    |> printfn "%i"
else
    [
        ">", 2
        "^>v<", 4
        "^v^v^v^v^v", 2
    ]
    |> Seq.map (fun (input, output) -> parse input, output)
    |> Seq.iter (fun (input, output) ->
        if part1 input = output then
            printfn "yay"
        else
            printfn "nay"
    )
