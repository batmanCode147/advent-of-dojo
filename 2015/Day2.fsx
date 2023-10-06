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
    let wrapping_paper(parcel : Parcel) : int =
        let sides = [ parcel.Height*parcel.Width; parcel.Height * parcel.Length; parcel.Width * parcel.Length]
        let area = 2 * Seq.sum sides
        let smallest_side = Seq.min sides
        area + smallest_side
    input
    |> Seq.map wrapping_paper
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
    |> part1
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
