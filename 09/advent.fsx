open System.IO

let root = __SOURCE_DIRECTORY__
let arr = File.ReadAllLines(Path.Join(root, "/input"))
let lines = Array.toList(arr)

let extractMove (line:string) =
    let parts = line.Split(" ")
    parts[0], int(parts[1])

let moves = lines |> Seq.map extractMove |> Seq.toList

let start = (0,0)

let moveHead ((x,y):int*int) dir =
    match dir with
    | "R" -> (x+1,y)
    | "U" -> (x,y+1)
    | "L" -> (x-1,y)
    | "D" -> (x,y-1)
    | _ -> (x,y)

let moveTail (tx, ty) (hx, hy) =
    let dx = hx - tx
    let dy = hy - ty
    let mx = if dx = 0 then 0 else  dx / abs(dx)
    let my = if dy = 0 then 0 else dy / abs(dy)
    match (dx, dy) with
    | (_, 0) when abs(dx) > 1 -> (tx + mx, ty)
    | (0, _) when abs(dy) > 1 -> (tx, ty + my)
    | (_, _) when abs(dx) > 1 || abs(dy) > 1 -> (tx + mx, ty + my)
    | _ -> (tx, ty)

let moveOne (head, tail) dir =
    let newHead = moveHead head dir
    let newTail = moveTail tail newHead
    (newHead,newTail)

let rec move (head, tail) dir moves =
    match moves with
    | 0 -> (head, tail),[tail]
    | _ ->
        let (h,t) = (moveOne (head, tail) dir)
        let (nh,nt),ts = move  (h,t) dir (moves-1)
        (nh, nt), tail::ts

let rec partOne (head,tail) lines =
    match lines with
    | (dir, moves)::rest -> 
        let mv,tails = (move (head, tail) dir moves)
        let po, ts = partOne mv rest
        po, tails::ts
    | _ -> (head, tail),[[tail]]

// let resultOne, tailsOne = partOne (start, start) moves
// let unique = tailsOne |> List.concat |> Set |> Seq.length


// PART TWO
type Pos = (int*int)

type State = {
    head: (int*int)
    knots: (int*int) list
    tails: (int*int) list
}

let startState = { 
    head = (0,0);
    knots = List.replicate 9 (0,0)
    tails = []
}

let rec moveKnots (knots:list<Pos>) (head:Pos) : list<Pos> =
    match knots with
    | knot::tail -> 
        let newKnot = moveTail knot head
        newKnot::moveKnots tail newKnot
    | _ -> []

let updateOne state dir =
    let afterHead = { state with head = moveHead state.head dir }
    let afterKnots = { afterHead with knots = moveKnots afterHead.knots afterHead.head }
    let last = afterKnots.knots |> List.last
    { afterKnots with tails = last::afterKnots.tails }

let rec update state dir moves =
    match moves with
    | 0 -> state
    | _ -> 
        let newState = updateOne state dir
        update newState dir (moves-1)

let rec partTwo state lines =
    match lines with
    | (dir, moves)::rest -> 
        let newState = update state dir moves
        partTwo newState rest
    | _ -> state

let finalState = partTwo startState moves

let resultTwo = finalState.tails |> Set |> Seq.length