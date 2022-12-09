open System.IO

let root = __SOURCE_DIRECTORY__
let arr = File.ReadAllLines(Path.Join(root, "/input"))
let lines = Array.toList(arr)

let rec group list =
    match list with
    | [] -> []
    | "" :: tail -> []::group(tail)
    | head :: tail ->
        let v = int(head)
        match group tail with
        | [] -> [[v]]
        | h::t -> (v::h)::t

let sums =
    group lines
    |> Seq.map (Seq.sum)

let mostCalories =
    sums
    |> Seq.max

let topCalories =
    sums
    |> Seq.sort
    |> Seq.rev
    |> Seq.take 3
    |> Seq.sum

