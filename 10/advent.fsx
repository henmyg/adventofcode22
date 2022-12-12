open System.IO

let root = __SOURCE_DIRECTORY__
let arr = File.ReadAllLines(Path.Join(root, "/input"))
let lines = Array.toList(arr)

type Element = {
    x: int
    cycle: int
}

let parseLine (line:string) state : Element =
    match line[..3] with
    | "addx" ->
        let dx = int(line[4..])
        { state with cycle = state.cycle + 2; x = state.x + dx }
    | "noop" -> { state with cycle = state.cycle + 1 }
    | _ -> state

let rec parseLines lines state : Element list =
    match lines with
    | line::tail ->
        let newState = parseLine line state
        newState::parseLines tail newState
    | _ -> []

let startState = { cycle = 0; x = 1 }

let states = startState:: parseLines lines startState

let checkpoints = [20;60;100;140;180;220]

let getState states checkpoint : Element =
    states |> Seq.filter (fun s -> s.cycle < checkpoint) |> Seq.maxBy (fun s -> s.cycle)

let getStrength (states:Element list) (checkpoint:int) : int =
    let state = getState states checkpoint
    state.x * checkpoint

let resultOne =
    checkpoints
    |> Seq.map (getStrength states)
    |> Seq.sum


// PART TWO
let positions = [1..240]

let getPixel (states:Element list) (pos:int) : char =
    let state = getState states pos

    let pos = (pos % 40) + 0

    if pos = state.x || pos = state.x+1 || pos = state.x+2 then
        '#'
    else
        '.'

getPixel states 1

let resultTwo =
    positions
    |> Seq.map (getPixel states)
    |> Seq.toArray
    |> Array.chunkBySize 40

let toString (chars:char seq) : string =
    chars
        |> Seq.map string
        |> String.concat ""

resultTwo |> Seq.iter (fun l -> printfn "%s" (toString l))
