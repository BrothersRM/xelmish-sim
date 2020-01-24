﻿// Learn more about F# at http://fsharp.org

open Elmish
open Xelmish.Model
open Xelmish.Viewables

type Model = {
    counter: int
    tickCount: int64
}

let init () = {
    counter = 0
    tickCount = 0L
}

type Message =
    | Increment of amount: int
    | Decrement of amount: int
    | Tick
      
let update message model =
    match message with
    | Increment x -> {model with counter = model.counter + x;}  
    | Decrement x -> {model with counter = model.counter - x;}
    | Tick -> model 
    
let view model dispatch =
    [
        yield text "defaultFont" 20. Colour.White (0., 0.) (sprintf "%i" model.counter) (300, 300)
        yield onupdate (fun x -> dispatch (Increment 1))
    ]

[<EntryPoint>]
let main _ =
    let config = {
        resolution = Windowed (600, 600)
        clearColour = Some Colour.Black
        mouseVisible = true
        assetsToLoad = [
            PipelineFont ("defaultFont", "./content/SourceCodePro")
        ]
    }
    Program.mkSimple init update view
    |> Xelmish.Program.runGameLoop config
    0
