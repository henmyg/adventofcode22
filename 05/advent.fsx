open System.IO
open System.Text.RegularExpressions

let root = __SOURCE_DIRECTORY__
let arr = File.ReadAllLines(Path.Join(root, "/test"))
let lines = Array.toList(arr)

let stackInput = lines[..2]
let moveInput = lines[5..]

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

let indexes = [0..2]
//let indexes = [0..8]
let startState = createStacks indexes

let updateStacks (stacks:Stacks) fromIdx (newFrom:list<char>) toIdx (crate:char) : Stacks =
    stacks
        |> List.mapi (fun i list ->
            if i = fromIdx then newFrom
            elif i = toIdx then crate::list
            else list
        )
        |> Seq.toList

let printStacks stacks =
    let debug =
        stacks
        |> Seq.map (fun (l:list<char>) -> 
            l |> Seq.map (fun c -> string(c)) |> String.concat ",")
        |> String.concat "];["
    printfn "[%s]" debug

let moveOne (stacks:Stacks) (fromIdx:int, toIdx:int) : Stacks =
    match stacks[fromIdx] with
    | crate::newFrom -> 
        printStacks stacks
        printfn "-->"
        let result = updateStacks stacks fromIdx newFrom toIdx crate
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
    | move::fromStack::toStack::tail -> moveMany stacks (fromStack,toStack) move
    | _ -> stacks

let rec doLines lines stacks: Stacks =
    match lines with
    | line::tail ->
        let newStacks = doLine line stacks
        doLines tail newStacks
    | _ -> stacks

let result = doLines moveInput startState
