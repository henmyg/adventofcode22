open System.IO
open System.Text.RegularExpressions

let root = __SOURCE_DIRECTORY__
let arr = File.ReadAllLines(Path.Join(root, "/test"))
let lines = Array.toList(arr)

let getSize (line:string) =
    int(line.Split(" ")[0])

let rec parse (lines:list<string>) (dir:list<string>):int =
    match lines with
    | line::tail ->
        match line with
        | s when s[..5] = "$ cd \/" -> parse tail ["/"]
        | s when s[..3] = "$ ls" -> parse tail dir
        | s when s[..2] = "dir" -> parse tail dir
        | s when s[..6] = "$ cd.." -> 
            match dir with
            | head::rest -> parse tail rest
            | _ -> parse tail dir
        | s when s[..4] = "$ cd" -> parse tail (s[5..]::dir)
        | s when Regex.IsMatch(s, "\d.* ") -> getSize s + parse tail dir
        | _ -> parse tail dir

    | _ -> 0

parse lines

    
