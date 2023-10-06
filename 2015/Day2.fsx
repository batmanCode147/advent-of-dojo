type Parcel = {
    Width : int
    Height : int
    Length : int
}

let parse (input:seq<string>) : seq<Parcel> =
    input
    |> Seq.map (fun line ->
        line.Split('x')
        |> Array.map System.Int32.Parse
        |> function 
            | [|w;h;l|] -> { Width = w; Height = h; Length = l } 
            | _ -> failwith "Invalid input format. Needs to be wxhxl."
    )

let part1 (input:seq<Parcel>) : int =
    let area (parcel : Parcel) : int =
        2*(parcel.Height*parcel.Width + parcel.Height * parcel.Length + parcel.Width * parcel.Length)
    let smallest_side(parcel : Parcel) : int =
        [ parcel.Height*parcel.Width; parcel.Height * parcel.Length; parcel.Width * parcel.Length]
        |> List.min
    input
    |> Seq.map (fun x -> area x + smallest_side x)
    |> Seq.sum

let part2 (input:seq<Parcel>) : int =
    let ribbon_length (parcel : Parcel) : int =
        let l = [parcel.Height; parcel.Width; parcel.Length]
        let perimeter = 2*(List.sum l - List.max l)
        let volume = List.reduce (*) l
        volume + perimeter
    input
    |> Seq.map ribbon_length
    |> Seq.sum

if fsi.CommandLineArgs.Length = 2 then
    let file = fsi.CommandLineArgs[1]
    System.IO.File.ReadAllLines(file)
    |> parse
    |> part2
    |> printfn "%i"
else
    [
        ["2x3x4"], 58
        ["1x1x10"], 43
    ]
    |> Seq.map (fun (input, output) -> parse input, output)
    |> Seq.iter (fun (input, output) ->
        if part1 input = output then
            printfn "yay"
        else
            printfn "nay"
    )
