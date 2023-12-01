#r "nuget: Raylib-cs"

open Raylib_cs
open type Raylib

[<Struct>]
type Position = { X : int; Y : int }

type CountDown = | CountDown of int

type PlayerId = | PlayerId of int

type PlayerData =
    {
        Position : Position
    }

type Bomb =
    {
        CountDown : CountDown
        Owner: PlayerId
        BlastRadius : int
    }

type BombBlast =
    {
        CountDown : CountDown
    }

type CellId = {X:int; Y:int}

type CellData =
    | Empty
    | Brick
    | Bomb of Bomb
    | BombBlast of BombBlast

type Board = Map<CellId, CellData>
type Players = Map<PlayerId, PlayerData>

type Bomberman =
    {
        Board : Board
        Players : Players
    }

type PlayerAction =
    | PlaceBomb

type PlayerMove =
    | MoveUp
    | MoveDown
    | MoveLeft
    | MoveRight

let CellSize = 20

let PositionToCellId : Position -> CellId = fun pos ->
    { X = (pos.X + CellSize / 2) / CellSize
      Y = (pos.Y + CellSize / 2) / CellSize }

let MovePlayer : PlayerMove -> Board -> PlayerData -> PlayerData = fun move board player ->
    let newPosition = 
        match move with
        | MoveDown  -> {player.Position with Y = player.Position.Y - 1}
        | MoveUp    -> {player.Position with Y = player.Position.Y + 1}
        | MoveLeft  -> {player.Position with X = player.Position.X - 1}
        | MoveRight -> {player.Position with X = player.Position.X + 1}

    let isValidMove =
        let oldCellId = PositionToCellId player.Position
        let newCellId = PositionToCellId newPosition
        if oldCellId <> newCellId then
            match board |> Map.tryFind newCellId with
            | Some Brick
            | Some (Bomb _)
            | None -> false
            | _ -> true
        else
            true

    if isValidMove then
        {player with
            Position = newPosition
        }
    else
        player

let DoPlayerAction : PlayerId -> Board * PlayerData -> PlayerAction -> Board * PlayerData = fun playerId (board, player) action ->
    match action with
    | PlaceBomb ->
        let cellId = PositionToCellId player.Position
        let bomb = {
            CountDown = CountDown 120
            Owner = playerId
            BlastRadius = 3
        }
        let newBoard = board |> Map.add cellId (Bomb bomb)
        (newBoard, player)

module CountDown =
    let willFinish (CountDown value) = value <= 1
    let tryDecrement (CountDown value) : option<CountDown> =
        if value <= 0 then
            None
        else
            CountDown (value - 1)
            |> Some

module Board =
    let isValidCell (board:Board) (cellId:CellId) = Map.containsKey cellId board

    let createEmpty (sizeX:int) (sizeY:int) : Board =
        Seq.init (sizeX * 2 + 1) id
        |> Seq.collect (fun x ->
            Seq.init (sizeY * 2 + 1) (fun y -> (x, y))
            |> Seq.filter (fun (x, y) -> (x % 2 = 0) || (y % 2 = 0))
        )
        |> Seq.map (fun (x, y) -> ({X=x;Y=y}, Empty))
        |> Map.ofSeq

    let addRandomBricks (rndBrick) (board:Board) =
        board
        |> Map.map (fun _ cellData ->
            match cellData with
            | Empty when rndBrick () ->
                Brick
            | _ -> cellData
        )

let ProgressBoard : Board -> Board = fun board ->
    let bombBlast (cellId) (bomb:Bomb) =
        seq {
        let get (offset) =
            Seq.initInfinite offset
            |> Seq.pairwise
            |> Seq.takeWhile (fun (last, _) -> board.TryFind(last) <> Some Brick)
            |> Seq.map snd
            |> Seq.takeWhile (Board.isValidCell board)
            |> Seq.truncate bomb.BlastRadius
        yield cellId
        yield! get (fun d -> {cellId with X = cellId.X + d})
        yield! get (fun d -> {cellId with X = cellId.X - d})
        yield! get (fun d -> {cellId with Y = cellId.Y + d})
        yield! get (fun d -> {cellId with Y = cellId.Y - d})
        }

    let rec ExplodeCascade (newBlasts:Set<CellId>) (bombBlasts:Set<CellId>) : Set<CellId> =
        if newBlasts.IsEmpty then bombBlasts
        else
            let newBlasts =
                newBlasts
                |> Set.toSeq
                |> Seq.collect (fun cellId ->
                    match board.TryFind(cellId) with
                    | Some (Bomb bomb) -> bombBlast cellId bomb
                    | _ -> Seq.empty
                )
                |> Set.ofSeq

            let newDiff = Set.difference newBlasts bombBlasts
            let bombBlasts = Set.union newBlasts bombBlasts
            ExplodeCascade newDiff bombBlasts

    let newBombBlasts =
        let blasts =
            seq {
            for (cellId, cellData) in Map.toSeq board do
                match cellData with
                | Bomb bomb ->
                    if CountDown.willFinish bomb.CountDown then
                        yield cellId
                | _ -> ()
            }
            |> Set.ofSeq
        ExplodeCascade blasts blasts

    board
    |> Map.map (fun cellId cellData ->
        if newBombBlasts |> Set.contains cellId then
            BombBlast {CountDown = CountDown 10}
        else
            match cellData with
            | BombBlast bombBlast ->
                match CountDown.tryDecrement bombBlast.CountDown with
                | Some countdown -> BombBlast {bombBlast with CountDown = countdown}
                | None -> Empty
            | Bomb bomb ->
                match CountDown.tryDecrement bomb.CountDown with
                | Some countdown -> Bomb {bomb with CountDown = countdown}
                | None -> failwith "unreachable"
            | _ -> cellData
    )

let Tick : Map<PlayerId, option<PlayerMove> * list<PlayerAction>> -> Bomberman -> Bomberman = fun playerInputs game ->
    let game = {game with Board = ProgressBoard game.Board}

    // TODO check if player dead
    let game =
        {game with
            Players =
                game.Players
                |> Map.filter (fun playerId playerData ->
                    let cell = game.Board |> Map.tryFind (PositionToCellId playerData.Position) 
                    match cell with
                    | Some (BombBlast _) -> false
                    | None -> false
                    | _ -> true
                )
        }

    let game =
        (game, Map.toSeq playerInputs)
        ||> Seq.fold (fun game (playerId, (move, actions)) ->
            match game.Players.TryFind(playerId) with
            | None -> game
            | Some playerData ->
                let playerData =
                    match move with
                    | Some move -> MovePlayer move game.Board playerData
                    | None -> playerData

                let (board, playerData) =
                    ((game.Board, playerData), actions)
                    ||> Seq.fold (DoPlayerAction playerId)
                {
                    Board = board
                    Players = game.Players |> Map.add playerId playerData
                }
        )

    game

type RenderTileType =
    | TileFloor
    | TileBrick
    | TileBomb
    | TileBombBlast
    | TilePlayer of PlayerId

type RenderTile = {
    Position : float * float
    Type : RenderTileType
}

let GetRenderRepresentation : Bomberman -> seq<RenderTile> = fun game ->
    let PositionFromCellId (cellId:CellId) = (float cellId.X, float cellId.Y)
    let PositionFromPosition (pos:Position) = (float pos.X / float CellSize, float pos.Y / float CellSize)
    seq {
        for (cellId, cellData) in Map.toSeq game.Board do
            yield {Position = PositionFromCellId cellId; Type = TileFloor}
            match cellData with
            | Bomb _ -> 
                yield {
                    Position = PositionFromCellId cellId
                    Type = TileBomb
                }
            | Brick -> yield {Position = PositionFromCellId cellId; Type = TileBrick}
            | BombBlast _ -> yield {Position = PositionFromCellId cellId; Type = TileBombBlast}
            | Empty -> ()

        for (playerId, playerData) in Map.toSeq game.Players do
            yield {Position = PositionFromPosition playerData.Position; Type = TilePlayer playerId}
    }

/////////////////////////////////////////////////////////////

let BoardSizeX = 10
let BoardSizeY = 10

let startGame = {
    Board = Board.createEmpty BoardSizeX BoardSizeY
    Players = [
        (PlayerId 0, {
            Position = {X=0;Y=0}
        })
        (PlayerId 1, {
            Position = {X=(BoardSizeX*2) * CellSize;Y=(BoardSizeX*2) * CellSize}
        })
    ] |> Map.ofList
}

let MoveBindings = [
    KeyboardKey.KEY_W, MoveDown
    KeyboardKey.KEY_S, MoveUp
    KeyboardKey.KEY_A, MoveLeft
    KeyboardKey.KEY_D, MoveRight
]

let ActionBindings = [
    KeyboardKey.KEY_SPACE, PlaceBomb
]

let MoveBindings2 = [
    KeyboardKey.KEY_UP, MoveDown
    KeyboardKey.KEY_DOWN, MoveUp
    KeyboardKey.KEY_LEFT, MoveLeft
    KeyboardKey.KEY_RIGHT, MoveRight
]

let ActionBindings2 = [
    KeyboardKey.KEY_ENTER, PlaceBomb
]

let RenderCellSize = 20.0

type CBool with member this.AsBool = this = CBool(true)

InitWindow(int (float (BoardSizeX * 2 + 1) * RenderCellSize), int (float (BoardSizeY * 2 + 1) * RenderCellSize), "Bomberman")
SetTargetFPS(CellSize * 2)
//DisableEventWaiting()
SetExitKey(LanguagePrimitives.EnumOfValue 0)

let addRandomBricks (game) =
    {game with Board = game.Board |> Board.addRandomBricks (fun () -> GetRandomValue(0, 99) < 30)}

let mutable game = addRandomBricks startGame

while not (WindowShouldClose().AsBool) do
    //PollInputEvents()

    if (IsKeyPressed KeyboardKey.KEY_ESCAPE).AsBool then
        game <- addRandomBricks startGame

    let playerInputs =
        [
            (PlayerId 0, (
                (   MoveBindings
                    |> Seq.choose (fun (key, move) ->
                        if (IsKeyDown key).AsBool then Some move
                        else None
                    )
                    |> Seq.tryHead
                ,   ActionBindings
                    |> List.choose (fun (key, action) ->
                        if (IsKeyPressed key).AsBool then
                            Some action
                        else None
                    )
                )
            ))
            (PlayerId 1, (
                (   MoveBindings2
                    |> Seq.choose (fun (key, move) ->
                        if (IsKeyDown key).AsBool then Some move
                        else None
                    )
                    |> Seq.tryHead
                ,   ActionBindings2
                    |> List.choose (fun (key, action) ->
                        if (IsKeyPressed key).AsBool then
                            Some action
                        else None
                    )
                )
            ))
        ]
        |> Map.ofList

    game <- Tick playerInputs game

    BeginDrawing()
    ClearBackground(Color.GRAY)

    let drawRect (x:float, y:float) (color:Color) =
        DrawRectangle(int (x * RenderCellSize), int (y * RenderCellSize), int RenderCellSize, int RenderCellSize, color)

    let drawCircle (x:float, y:float) (color:Color) =
        DrawCircle(int ((x + 0.5) * RenderCellSize), int ((y + 0.5) * RenderCellSize), float32 (RenderCellSize * 0.5), color)

    for tile in GetRenderRepresentation game do
        match tile.Type with
        | TileFloor -> drawRect tile.Position Color.RAYWHITE
        | TileBrick -> drawRect tile.Position Color.LIGHTGRAY
        | TileBomb -> drawCircle tile.Position Color.BLACK
        | TileBombBlast ->
            let c1 = Color.YELLOW
            let c2 = Color.ORANGE
            let r = GetRandomValue(0, 255)
            let color =
                let lerp (b1:byte) (b2:byte) = (int b1 * r + int b2 * (255 - r)) / 255
                Color(lerp c1.R c2.R, lerp c1.G c2.G, lerp c1.B c2.B, 255)
            drawCircle tile.Position color
        | TilePlayer playerId ->
            drawCircle tile.Position (if playerId = PlayerId(0) then Color.BLUE else Color.GREEN)


    EndDrawing()

CloseWindow()
