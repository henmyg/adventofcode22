open System.IO

let root = __SOURCE_DIRECTORY__
let arr = File.ReadAllLines(Path.Join(root, "/input"))
let lines = Array.toList(arr)

let start = 250,0
let xmin = 250
let xmax = 750
let ymax = 200

let partTwo = true

let emptyMap = Array2D.init (xmax - xmin) ymax (fun x y -> '.')

let normalize c1 c2 =
    if c2=c1 then 0 else (c2-c1) / abs(c2-c1)

let rec insertRocks (state:char[,]) (x1,y1) (x2,y2) = // : char[,] =
    state.[x1,y1] <- '#'
    if x1=x2 && y1=y2 then state
    else insertRocks state (x1 + normalize x1 x2, y1 + normalize y1 y2) (x2, y2)

let rec rockPoint state (pieces:(int*int) list) i =
    let newState = insertRocks state pieces[i-1] pieces[i]
    match i+1 with
    | ii when ii = pieces.Length -> newState
    | ii -> rockPoint newState pieces ii

let parseLine state (line:string) = //: char[,] =
    let points = line.Split(" -> ") |> Seq.toList
    let pieces = points |> List.map (fun s -> 
        let xy = s.Split(",")
        (int(xy[0])-xmin,int(xy[1])))
    rockPoint state pieces 1

let rec parseLines state lines =
    match lines with
    | line::tail -> 
        let newState = parseLine state line
        parseLines newState tail
    | _ -> state

let rockyMap = parseLines emptyMap lines

let bottom =
    let mutable maxY = 0
    rockyMap
        |> Array2D.iteri  (fun x y i -> if i <> '.' && y > maxY then maxY <- y else () )
    maxY + 1


let prettyPrint (map:char[,]) =
    let strings = [
        for row in 0..map.GetLength 1 - 1 do
            yield map.[*,row] |> Seq.map string |> String.concat ""
        ]
    for s in strings do
        printfn "%s" s

// rockyMap |> prettyPrint

// SIMULATE


let rec simSand (state:char[,]) (x,y) =
    if partTwo && y = bottom then
        state.[x,y] <- 'o'
        Some(state)

    elif partTwo = false && y >= ymax-1 then None
    else
        match (x,y) with
        | _ when state.[x,y+1] = '.' -> simSand state (x,y+1)
        | _ when state.[x-1,y+1] = '.' -> simSand state (x-1,y+1)
        | _ when state.[x+1,y+1] = '.' -> simSand state (x+1,y+1)
        | _ ->
            state.[x,y] <- 'o'
            Some(state)

let rec simulate (state:char[,]) (cycles:int) =
    let newState = simSand state start

    match newState with
    | None -> (cycles, state)
    | Some(state) -> 
        let (sx, sy) = start
        if state.[sx,sy] = 'o' then (cycles+1, state)
        else simulate state (cycles+1)

    // pieces |>  |> Seq.reduce (fun a b -> (insertRocks state a b))

let (cycles, restingState) = simulate rockyMap 0

prettyPrint restingState
printfn "Result: %d" cycles

// PART TWO


