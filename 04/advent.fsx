open System.IO

let root = __SOURCE_DIRECTORY__
let arr = File.ReadAllLines(Path.Join(root, "/test"))
let lines = Array.toList(arr)


let toMinMax = fun (str:string) ->
    let split = str.Split("-")
    (int(split[0]), int(split[1]))

let toPair = fun (line:string) ->
    let split = line.Split(',')
    (toMinMax split[0], toMinMax split[1])

let isContained = fun ((min1, max1),(min2,max2)) ->
    (min1 >= min2 && max1 <= max2)
    || (min2 >= min1) && (max2 <= max1)

let resultOne =
    lines
    |> Seq.map toPair
    |> Seq.filter isContained
    |> Seq.length

// PART TWO
let isOverlap = fun ((min1, max1),(min2,max2)) ->
    not (max2 < min1 || max1 < min2)

let resultTwo =
    lines
    |> Seq.map toPair
    |> Seq.filter isOverlap
    |> Seq.length    