open System.IO
open System.Text.RegularExpressions

let root = __SOURCE_DIRECTORY__
let arr = File.ReadAllLines(Path.Join(root, "/input"))
let lines = Array.toList(arr)

let stackInput = lines[..7] // lines[..2]
let moveInput = lines[10..] // lines[5..]

// CREATE STACKS
let rec getStack (line:string) (i:int) :list<char> =
    let index = i * 4
    let potential = line[index..index+2] |> Seq.toList
    match potential with
    | '['::letter::']'::tail -> [letter]
    | _ -> []

let rec getStacks (lines:list<string>) (i:int) =
    match lines with
    | line::tail -> getStack line i @  getStacks tail i
    | _ -> []
        

let rec createStacks indexes =
    match indexes with
    | index::tail -> getStacks lines index :: createStacks tail
    | _ -> []

// MOVE STACKS

type Stacks = list<list<char>>

// let indexes = [0..2]
let indexes = [0..8]
let startState = createStacks indexes

let updateStacks (stacks:Stacks) fromIdx (newFrom:list<char>) toIdx newTo : Stacks =
    stacks
        |> List.mapi (fun i list ->
            if i = fromIdx then newFrom
            elif i = toIdx then newTo
            else list
        )
        |> Seq.toList

let printStacks stacks =
    ignore ""
    // let debug =
    //     stacks
    //     |> Seq.map (fun (l:list<char>) -> 
    //         l |> Seq.map (fun c -> string(c)) |> String.concat ",")
    //     |> String.concat "];["
    // printfn "[%s]" debug

let moveOne (stacks:Stacks) (fromIdx:int, toIdx:int) : Stacks =
    match stacks[fromIdx] with
    | crate::newFrom -> 
        printStacks stacks
        printfn "%d --> %d" fromIdx toIdx
        let result = updateStacks stacks fromIdx newFrom toIdx (crate::stacks[toIdx])
        printStacks result
        result
            // (l |> Seq.map sprintf |> String.concat ",")))
    | _ -> stacks

let rec moveMany stacks fromTo (count:int) : Stacks =
    match count with
    | 0 -> stacks
    | _ -> 
        let newStacks = moveOne stacks fromTo
        moveMany newStacks fromTo (count-1)




let doLine line stacks : Stacks =
    let regex = new Regex(@"\d+")
    let matches = regex.Matches(line) |> Seq.map (fun m -> int(m.Value)) |> Seq.toList

    match matches with
    | move::fromStack::toStack::tail -> moveMany stacks (fromStack-1,toStack-1) move
    | _ -> stacks

let rec doLines lines stacks: Stacks =
    match lines with
    | line::tail ->
        let newStacks = doLine line stacks
        doLines tail newStacks
    | _ -> stacks

let finalStacks = doLines moveInput startState
let resultOne = finalStacks |> Seq.map (fun l -> string(l[0])) |> String.concat ""

// PART TWO

let moveTwo (stacks: Stacks) (fromIdx,toIdx) (count: int) : Stacks =
    let (crates, rest) = stacks[fromIdx] |> List.splitAt count
    updateStacks stacks fromIdx rest toIdx (crates@(stacks[toIdx]))

let doLineTwo line stacks : Stacks =
    let regex = new Regex(@"\d+")
    let matches = regex.Matches(line) |> Seq.map (fun m -> int(m.Value)) |> Seq.toList

    match matches with
    | move::fromStack::toStack::tail -> moveTwo stacks (fromStack-1,toStack-1) move
    | _ -> stacks

let rec doLinesTwo lines stacks: Stacks =
    match lines with
    | line::tail ->
        let newStacks = doLineTwo line stacks
        doLinesTwo tail newStacks
    | _ -> stacks

let finalStacksTwo = doLinesTwo moveInput startState
let resultTwo = finalStacksTwo |> Seq.map (fun l -> string(l[0])) |> String.concat ""