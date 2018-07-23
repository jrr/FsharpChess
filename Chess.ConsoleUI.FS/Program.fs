// Learn more about F# at http://fsharp.org

open System
open Chess.Domain.Api

type ParsedMove = {
    IsValid : bool
    From : string
    To : string
}

let parseMove (move:string) =
    let parts = move.ToUpper().Split([|',';' ';'-'|])
    match parts.Length with
    | 2 -> {IsValid = true;From = parts.[0];To = parts.[1]}
    | _ -> {IsValid = false;From = "";To = ""}
    
let ranks = ["pawn";"rook";"knight";"bishop";"queen";"king"]

let validRank rank =
    ranks |> List.contains rank

let blackGlyphs = [
            "pawn","♟"
            "rook","♜"
            "knight","♞"
            "bishop","♝"
            "queen","♛"
            "king","♚" ] |> Map.ofList
            
let whiteGlyphs = [
            "pawn","♙"
            "rook","♖"
            "knight","♘"
            "bishop","♗"
            "queen","♕"
            "king","♔" ] |> Map.ofList
            
let RenderCell (cell:CellDTO) =
    match cell with
    | c when not c.isOccupied -> "."
    | c when c.color = "black" && validRank c.rank -> blackGlyphs.[c.rank]
    | c when c.color = "white" && validRank c.rank -> whiteGlyphs.[c.rank]
    | _ -> failwith "Invalid piece."
    
let rec RenderGame(api: ChessApi) =
    printfn ""
    printfn "    A B C D E F G H"
    printfn "    ─ ─ ─ ─ ─ ─ ─ ─"
    
    let rows = ["8"; "7"; "6"; "5"; "4"; "3"; "2"; "1"]
    let cols = ["A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"]
    let cells = api.Cells |> Array.map (fun c -> c.coord,c) |> Map.ofArray
    rows |> List.iter (fun row ->
        printf "%s |" row
        cols |> List.iter (fun col ->
            let cell = cells.[col+row]
            printf " %s" (RenderCell cell)
        )
        printfn ""
    )
    printfn ""
    printfn "%s" api.Message
    
    printfn "Enter a move in the following format: A2 A3 (case insensitive)"
    printfn "> "
    
    let moveText = Console.ReadLine()
    let move = parseMove(moveText)
    if move.IsValid then
        api.Move(move.From, move.To)
    else 
        ()
    Console.Clear()
    RenderGame(api)
    ()

[<EntryPoint>]
let main argv =
    let c = ChessApi()
    RenderGame(c)
    0 // return an integer exit code
