open System.IO

let root = __SOURCE_DIRECTORY__
let arr = File.ReadAllLines(Path.Join(root, "/test"))
let lines = Array.toList(arr)

let updateAti (index:int) (updFun:'a->'a) (list:'a list) : 'a list =
    list
        |> List.mapi (fun i item ->
            if i = index then
                updFun item
            else
                item
        )

type Monkey = {
    id: int
    items: int list
    operation: int -> int
    testDiv: int
    trueMonkey: int
    falseMonkey: int 
    inspects: int
}

let parseOp (line:string) : int->int =
    match line with
    | s when s.StartsWith("old + ") ->
        let toAdd = s.Split(" + ")[1] |> int
        fun i -> i + toAdd
    | s when s.StartsWith("old * old") ->
        fun i -> i * i
    | s when s.StartsWith("old *") -> 
        let x = s.Split(" * ")[1]
        let toMulti = x |> int
        fun i -> i * toMulti
    | _ -> fun i -> i


let parseMonkey (lines:string list) =
    let id = (lines[0].Split(" ")[1]).Split(":")[0] |> int
    let items = (lines[1].Split(": ")[1]).Split(", ") |> Seq.map int |> Seq.toList
    let op = parseOp (lines[2].Split("= ")[1])
    let testDiv = int(lines[3].Split("by ")[1])
    let trueMonkey = int(lines[4].Split("monkey ")[1])
    let falseMonkey = int(lines[5].Split("monkey ")[1])
    {
        id = id
        items = items
        operation = op
        testDiv = testDiv
        trueMonkey = trueMonkey
        falseMonkey = falseMonkey
        inspects = 0
    }

let monkeys =
    lines
    |> List.chunkBySize 7
    |> Seq.map parseMonkey 
    |> Seq.toList

let inspectItem (monkey:Monkey) (item:int) : int =
    let afterOp = monkey.operation item
    let res = (afterOp % 96577) // % 9699690 //% 96577 // / 3
    res

let addItem (monkeys:Monkey list) (id:int) (item:int) : Monkey list =
    monkeys
        |> updateAti id (fun monkey ->
            { monkey with items = monkey.items@[item] })

let rec performMonkey (monkeys: Monkey list) (i:int) : Monkey list =
    let monkey = monkeys[i]
    match monkey.items with
    | item::rest ->
        let newItem = inspectItem monkey item
        let afterInspect = monkeys |> updateAti i (fun m -> { m with inspects = m.inspects + 1})

        let toMonkey = if (newItem % monkey.testDiv) = 0 then monkey.trueMonkey else monkey.falseMonkey
        let afterAdd = addItem afterInspect toMonkey newItem

        let afterRem = afterAdd |> updateAti i (fun monkey -> { monkey with items = rest })
        performMonkey afterRem i        
    | _ -> monkeys



let rec performRound monkeys (ids:int list) : Monkey list  =
    match ids with
    | id::rest ->
        let newMonkeys = performMonkey monkeys id
        performRound newMonkeys rest
    | _ -> monkeys

performRound monkeys (monkeys |> List.map (fun m -> m.id ))

let rec performRounds monkeys (rounds:int list) : Monkey list =
    match rounds with
    | round::rest ->
        let newMonkeys = performRound monkeys (monkeys |> List.map (fun m -> m.id ))
        performRounds newMonkeys rest
    | _ -> monkeys

let after20 = performRounds monkeys [1..20]
let inspects = after20 |> List.map (fun m -> m.inspects)
let most = inspects |> List.max
let second = inspects  |> List.filter (fun r -> r <> most) |> List.max
let resultOne = most * second

// PART TWO

let divs = monkeys |> List.map (fun m -> m.testDiv)
let divSum = divs |> Seq.reduce (fun a b -> a * b)

let after10000 = performRounds monkeys [1..20]
let inspects2 = after10000 |> List.map (fun m -> m.inspects)
let most2 = inspects2 |> List.max
let second2 = inspects2  |> List.filter (fun r -> r <> most2) |> List.max
let resultTwo = most2 * second2
