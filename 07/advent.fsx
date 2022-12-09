open System.IO
open System.Text.RegularExpressions

let root = __SOURCE_DIRECTORY__
let arr = File.ReadAllLines(Path.Join(root, "/input"))
let lines = Array.toList(arr)

type State = {
    dir: string list

    sizes: Map<string, int>
}

let getSize (line:string) =
    int(line.Split(" ")[0])

let popDir (dir:list<string>) : string list =
    match dir with
    | head::tail -> tail
    | _ -> dir

let updateSize state (dir:string list) size : State =
    let key = dir |> List.rev |> String.concat "/"

    let sizes = state.sizes
    let oldSize = if sizes.ContainsKey(key) then sizes.[key] else 0
    let newSizes = sizes |> Map.add key (oldSize + size)
    { state with sizes = newSizes }

let rec updateSizes state dir size : State =
    match dir with
    | head::tail ->
        let newState = updateSize state dir size
        updateSizes newState tail size
    | _ -> state


let parseLine (state:State) (line:string) : State =
    match line with
    | s when s[..5] = "$ cd \/" -> state
    | s when s[..3] = "$ ls" -> state
    | s when s[..2] = "dir" -> state
    | s when s[..6] = "$ cd .." -> 
        { state with dir = popDir state.dir }
    | s when s[..3] = "$ cd" ->
        let dir = s[5..]
        { state with dir = dir::state.dir }
    | s when Regex.IsMatch(s, "\d.* ") ->
        let size = getSize s
        updateSizes state state.dir size

    | _ -> state

let rec parseLines state lines : State =
    match lines with
    | line::tail ->
        let newState = parseLine state line
        parseLines newState tail
    | _ -> state

let startState = { 
    dir = []
    sizes = Map([])
}

let finalStateOne = parseLines startState lines
let resultOne = finalStateOne.sizes.Values |> Seq.filter (fun v -> v <= 100000) |> Seq.sum

// PART TWO
let totalAvail = 70000000
let needed = 30000000
let totalUse = finalStateOne.sizes["/"]
let freeSpace = totalAvail - totalUse
let missing = needed - freeSpace

let resultTwo = finalStateOne.sizes.Values |> Seq.filter (fun v -> v >= missing) |> Seq.min