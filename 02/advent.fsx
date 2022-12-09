open System.IO

let root = __SOURCE_DIRECTORY__
let arr = File.ReadAllLines(Path.Join(root, "/input"))
let lines = Array.toList(arr)


let winScore (opp:string , self:string) = 
    match (opp, self) with
    | "A", "X" -> 3
    | "A", "Y" -> 6
    | "B", "Y" -> 3
    | "B", "Z" -> 6
    | "C", "Z" -> 3
    | "C", "X" -> 6
    | _ -> 0

let selfScore self =
    match self with
    | "X" -> 1
    | "Y" -> 2
    | "Z" -> 3
    | _ -> 0

let scoreLine (line:string) = 
    line.Split(" ")
    |> fun l -> winScore (l[0], l[1]) + selfScore l[1]

let scorePartOne =
    lines
    |> Seq.map scoreLine
    |> Seq.sum

//rock, paper, scissor
//abc
// xyz

let chooseWin opp =
    match opp with
    | "A" -> "Y"
    | "B" -> "Z"
    | "C" -> "X"
    | _ -> ""

let chooseDraw opp =
    match opp with
    | "A" -> "X"
    | "B" -> "Y"
    | "C" -> "Z"
    | _ -> ""

let chooseLose opp =
    match opp with
    | "A" -> "Z"
    | "B" -> "X"
    | "C" -> "Y"
    | _ -> ""

let myChoice (opp:string , result:string) =
    match result with
    | "X" -> chooseLose opp
    | "Y" -> chooseDraw opp
    | "Z" -> chooseWin opp
    | _ -> chooseLose opp

let scoreLineTwo (line:string) = 
    line.Split(" ")
    |> fun l -> (l[0], l[1])
    |> fun (opp, res) -> (opp, myChoice (opp, res))
    |> fun (opp, self) -> winScore (opp, self)  + selfScore self

// let debugLine (line:string) = 
//     line.Split(" ")
//     |> fun l -> (l[0], l[1])
//     |> fun (opp, res) -> (opp, myChoice (opp, res))
//     |> fun (opp, self) -> winScore (opp, self)  + selfScore self

// let debug =
//     lines
//     |> Seq.map debugLine

// Seq.toArray(debug)
let scorePartTwo =
    lines
    |> Seq.map scoreLineTwo
    |> Seq.sum
