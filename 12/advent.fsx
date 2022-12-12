open System.IO

let root = __SOURCE_DIRECTORY__
let arr = File.ReadAllLines(Path.Join(root, "/input"))
let lines = Array.toList(arr)

type Map = char[,]
type Pos = int * int
type State = {
    positions: Pos list
    steps:int
}

let stepTWO = true

let map = lines |> List.map (fun line -> line |> Seq.toArray ) |> List.toArray |> array2D

let nextIndex ((y,x):Pos) : Pos option =
    let maxY = map |> Array2D.length1
    let maxX = map |> Array2D.length2
    match (y+1, x+1) with
    | (a,b) when a = maxY && b = maxX ->
        None
    | (_,b) when b = maxX -> Some(y+1, 0)
    | (_, _) -> Some(y, x+1)

let rec findIndex (map:Map) (needle:char) ((y,x):Pos)  : Pos option =
    if map[y,x] = needle then Some((y,x))
    else
        let next = nextIndex (y,x)
        match next with
        | Some(pos) -> findIndex map needle pos
        | None -> None

let start = (findIndex map 'S' (0,0)).Value
let goal = (findIndex map 'E' (0,0)).Value

let isValid (map:Map) (height:char) ((y,x):Pos) : bool =
    if y < 0 || x < 0 || y >= (map |> Array2D.length1) || x >= (map |> Array2D.length2) then
        false
    else
        let other = int(map[y,x])
        if stepTWO then
            other >= int(height) - 1
        else
            other <= int(height) + 1

let getValidMoves (map:Map) ((y,x):Pos) : Pos list =
    let current = map[y,x]
    let nbs = [(y+1, x);(y,x+1);(y-1,x);(y,x-1)]
    let validNbs =
        nbs
        |> List.filter (isValid map current)
    validNbs

let rec getPositions map (positions:Pos list) : Pos list =
    match positions with
    | pos::tail -> 
        let valid = getValidMoves map pos
        valid@getPositions map tail
    | _ -> []

let rec iteration (map:Map) (state:State) : State =
    let moves = getPositions map state.positions
    let unique = moves |> List.distinct
    let newState =
        { state with
            steps = state.steps + 1;
            positions = unique
        }

    if stepTWO then
        if unique |> List.filter (fun (y,x) -> map[y,x] = 'a') |> List.isEmpty
        then iteration map newState
        else newState
    else
        if unique |> List.contains goal then
            newState
        else
            iteration map newState

let initialState = { 
    steps = 0
    positions = [start]
}

map.[start |> fst , start |> snd] <- 'a'
map.[goal |> fst, goal |> snd] <- 'z'

let resultingState = iteration map initialState
let resultOne = resultingState.steps

// PART TWO
// Oh why didnt i go backwards!?
// I am..
let newIntial = { initialState with positions = [goal] }



let rec findIndexes (map) (needle) (y,x) =
    let pos = findIndex map needle (y,x)
    match pos with
    | Some(p) ->
        printfn "%s" (p.ToString())
        let next = nextIndex (y,x)
        match next with
        | Some(n) -> p::findIndexes map needle n
        | None -> [p]
    | None -> []

let starts = findIndexes map 'a' (0,0) |> List.distinct

let resultTwo =
    starts
    |> List.map (fun s -> iteration map { initialState with positions = [s] })
    |> List.map (fun s -> s.steps)
    |> List.min