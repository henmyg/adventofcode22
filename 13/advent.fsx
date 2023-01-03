open System.IO
open System.Text.Json
open System.Data

let root = __SOURCE_DIRECTORY__
let arr = File.ReadAllLines(Path.Join(root, "/test"))
let lines = Array.toList(arr)

let rec compareArrs (arr1:int list) (arr2:int list) : bool =
    match (arr1, arr2) with
    | [], [] -> true
    | [], _ -> true
    | _, [] -> false
    | h1::t1, h2::t2 ->
        if h1 < h2 then true
        elif h1 = h2 then compareArrs t1 t2
        else false


type TType =
| TList of TType list
| TValue of int

// let parseLine (line:string) : 'TType =
//     parser.Parse line
//     JsonSerializer.Deserialize(line)


let allPairs =
    lines
    |> List.chunkBySize 3

// parseLine lines[0]

// let obj = JsonSerializer.Deserialize(lines[0]) :?> Any

let resultOne =
    allPairs
