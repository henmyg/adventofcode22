open System.IO

let root = __SOURCE_DIRECTORY__
let arr = File.ReadAllLines(Path.Join(root, "/input"))
let lines = Array.toList(arr)


let test1 = "bvwbjplbgvbhsrlpgdmjqwftvncz"
let test2 = "nppdvjthqldpwncqszvftbrmjlhg"
let test3 = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
let test4 = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
let input = lines[0]

let isMarker = fun (chars:list<char>) ->
    chars.Length = 4 && Set(chars).Count = 4

let rec findMarker (str:string) (index:int) =
    let potential = str[0..3] |> Seq.toList
    match potential with
    | M when isMarker M -> index
    | _ -> findMarker str[1..] (index+1)
    
let resultOne = findMarker input 0 + 4

// PART TWO
let test5 = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
let test6 = "bvwbjplbgvbhsrlpgdmjqwftvncz"
let test7 = "nppdvjthqldpwncqszvftbrmjlhg"
let test8 = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
let test9 = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"

let isMarker2 = fun (chars:list<char>) ->
    chars.Length = 14 && Set(chars).Count = 14

let rec findMarker2 (str:string) (index:int) =
    let potential = str[0..13] |> Seq.toList
    //printf "%s " (potential|>string)
    match potential with
    | M when isMarker2 M -> index
    | _ -> findMarker2 str[1..] (index+1)

let resultTwo = findMarker2 input 0 //+ 14
let answer = resultTwo + 14