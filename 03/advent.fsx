open System.IO

let root = __SOURCE_DIRECTORY__
let arr = File.ReadAllLines(Path.Join(root, "/input"))
let lines = Array.toList(arr)

let splitLine = fun (line:string) ->
    let mid = line.Length / 2
    let left = line[..mid-1]
    let right = line[mid..]
    (left.ToCharArray(), right.ToCharArray())

let leftOver = fun splits ->
    splits
    |> fun (l, r) -> (Set(l), Set(r) )
    |> fun (l,r) -> Set.intersect l r

let points = fun (c:char) ->
    match c with
    | x when System.Char.IsLower x -> 1 + int(x) - int('a')
    | x when System.Char.IsUpper x -> 27 + int(x) - int('A')
    | _ -> 0

let scoreSet = fun set ->
    set
    |> fun s -> (Set.toArray s)[0]
    |> points

let scoreLine = fun line ->
    splitLine line
    |> leftOver
    |> scoreSet
    

let resultOne =
    lines
    |> Seq.map scoreLine
    |> Seq.sum

// PART TWO
let rec splitThree (lines:list<string>):list<list<string>> =
    match lines with
    | one::two::three::tail -> [one;two;three]::splitThree tail
    | _ -> []

let leftOverGroup = fun (grp:list<string>) ->
    grp
    |> Seq.map Set
    |> Set.intersectMany

let groups = splitThree lines

leftOverGroup groups[0]
    
lines
    |> splitThree
    |> Seq.map leftOverGroup
    |> Seq.map scoreSet
    |> Seq.sum