open System.IO

let root = __SOURCE_DIRECTORY__
let arr = File.ReadAllLines(Path.Join(root, "/input"))
let lines = Array.toList(arr)

type Point = {
    x: int
    y: int
}

type Pair = {
    sensor: Point
    beacon: Point
    dist: int
}
//Sensor at x=2, y=18: closest beacon is at x=-2, y=15

let getDist one other =
    abs(one.x-other.x)+abs(one.y-other.y)

let parseLine (line:string) =
    let sb =
        line
            .Replace("Sensor at x=", "")
            .Replace(" y=", "")
            .Split(": closest beacon is at x=")
    let ss = sb[0].Split(",") |> Array.map int
    let sensor = { x=ss[0]; y=ss[1] }
    let bb = sb[1].Split(",") |> Array.map int
    let beacon = { x=bb[0]; y=bb[1] }
    let dist = getDist sensor beacon
    {
        sensor = sensor
        beacon = beacon
        dist = dist
    }

let getRange (y:int) (pair:Pair) =
    let yDist = abs(y-pair.sensor.y)
    if pair.dist > yDist then
        let xDist = pair.dist - yDist
        Some((pair.sensor.x-xDist),(pair.sensor.x+xDist))
    else None

let ranges =
    lines
    |> List.map parseLine
    |> List.map (getRange 2000000)
    |> List.choose id

let overlap one other =
    (fst one > snd other || fst other > snd one) = false

let rec colRanges (ranges:(int*int) list) =
    match ranges with
    | range::rest ->
        let overlaps = range:: (rest |> List.filter (overlap range))
        let newRest = rest |> List.except overlaps
        let newRange = (
            overlaps |> List.map fst |> List.min,
            overlaps |> List.map snd |> List.max)
        newRange::colRanges newRest
    | _ -> []

let rec colCol ranges =
    let newRanges = colRanges ranges
    if newRanges.Length = ranges.Length then newRanges
    else colCol newRanges

let finalRanges = colCol ranges
let resultOne =
    finalRanges
    |> List.map (fun r -> snd r - fst r)
    |> List.sum

// let extra = 20

// let minX = (sensors |> List.map (fun ((sx,sy),(bx,by)) -> min sx bx) |> List.min) - extra
// let maxX = (sensors |> List.map (fun ((sx,sy),(bx,by)) -> max sx bx) |> List.max) + extra
// let minY = (sensors |> List.map (fun ((sx,sy),(bx,by)) -> min sy by) |> List.min) - extra
// let maxY = (sensors |> List.map (fun ((sx,sy),(bx,by)) -> max sy by) |> List.max) + extra

// let correctedSensors =
//     sensors
//     |> List.map (fun ((sx,sy),(bx,by)) -> ((sx-minX,sy-minY),(bx-minX,by-minY)))

// let world = Array2D.init (maxX-minX) (maxY-minY) (fun x y -> '.')

// let getDist (sx:int,sy:int) (bx:int,by:int) =
//     abs(bx-sx)+abs(by-sy)

// let drawSensor (world:char[,]) ((sx:int,sy:int),(bx:int,by:int)) =
//     let dist = getDist (sx,sy) (bx,by)
//     printfn "(%d,%d)-(%d,%d) == %d" sx sy bx by dist
//     for x in (sx-dist)..(sx+dist) do
//         for y in (sy-dist)..(sy+dist) do
//             if (getDist (sx,sy) (x,y)) <= dist && world[x,y]='.' then
//                 world.[x,y] <- '|'
//     world.[sx,sy] <- 'S'
//     world.[bx,by] <- 'B'
//     world

// let rec drawSensors world sensors =
//     match sensors with
//     | sensor::tail ->
//         let newWorld = drawSensor world sensor
//         drawSensors newWorld tail
//     | _ -> world

// let filledWorld = drawSensors world correctedSensors

// let prettyPrint (map:char[,]) =
//     let strings = [
//         for row in 0..map.GetLength 1 - 1 do
//             yield (map.[*,row] |> Seq.map string |> String.concat "")
//         ]
//     for s in strings do
//         printfn "%s" s

// // prettyPrint filledWorld

// let checkY = 10 - minY
// let result =
//     (filledWorld[*, checkY]
//     |> Array.filter (fun c -> c <> '.')
//     |> Array.length
//     )- 1
