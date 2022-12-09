open System.IO

let root = __SOURCE_DIRECTORY__
let arr = File.ReadAllLines(Path.Join(root, "/input"))
let lines = Array.toList(arr)

// PARSE AS INTS
let charToInt c = int c - int '0'
let toInts line = line |> Seq.map charToInt |> Seq.toList

let treeMap = lines |> Seq.map toInts |> Seq.toList


let rec checkLeft (tallest:int) (line:list<int>)  =
    match line with
    | height::tail -> (tallest < height)::checkLeft (max tallest height) tail 
    | _ -> []

let checkTrees list =
    list |> Seq.map (checkLeft -1) |> Seq.toList

let getCol (arr:list<list<'a>>) col : list<'a> =
    arr |> Seq.map (fun l -> l[col]) |> Seq.toList
let rotate (arr:list<list<'a>>) =
    [0..arr.Length-1] |> Seq.map (getCol arr) |> Seq.toList

let reverse list =
    list
        |> Seq.map (fun x -> x |> Seq.rev |> Seq.toList)
        |> Seq.toList

let checkAll (trees:list<list<int>>) =
    let left = trees |> checkTrees |> List.concat
    let right = trees |> reverse |> checkTrees |> reverse |> List.concat
    let top = trees |> rotate |> checkTrees |> rotate |> List.concat
    let bottom = trees |> rotate |> reverse |> checkTrees |> reverse |> rotate |> List.concat

    [0..left.Length-1]
        |> Seq.map (fun i -> left[i] || right[i] || top[i] || bottom[i])
        |> Seq.map (fun b -> if b then 1 else 0)
        |> Seq.sum


let resultOne = checkAll treeMap

// PART TWO

let rec counting (trees:list<int>) (height:int) =
    match trees with
    | head::tail when head < height -> 1 + (counting tail height)
    | head::tail when head >= height -> 1
    | _ -> 0

let rec countLeft (line:list<int>) =
    match line with
    | head::tail -> (counting tail head)::countLeft tail
    | _ -> []

let countTrees (list:list<list<int>>) =
    list |> Seq.map countLeft |> Seq.toList

let countAll (trees:list<list<int>>) =
    let left = trees |> countTrees |> List.concat
    let right = trees |> reverse |> countTrees |> reverse |> List.concat
    let top = trees |> rotate |> countTrees |> rotate |> List.concat
    let bottom = trees |> rotate |> reverse |> countTrees |> reverse |> rotate |> List.concat

    [0..left.Length-1]
        |> Seq.map (fun i -> left[i] * right[i] * top[i] * bottom[i])
        |> Seq.max

let resultTwo = countAll treeMap

