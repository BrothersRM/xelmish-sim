open Elmish
open Xelmish.Model
open Xelmish.Viewables
open xelmish_template.Controls

type Model = {
    boxes: (int*int) list
    tickCount: int64
}


let init () = {
    tickCount = 0L
    boxes = [(10, 10); (100, 100)]
}

type Message =
    | Tick of int64
    | AddBox of int*int
    
let tryMoveDown (position: int*int) (stepSize: int) =
    match position with
    | (x, y) when y < 600 - stepSize -> (x, y+stepSize)
    | _ -> position
    
let update message model =
    match message with
    | Tick x -> {model with tickCount = x; boxes = model.boxes |> List.map (fun x -> tryMoveDown x 10)} 
    | AddBox (x, y) -> {model with boxes = (List.append model.boxes [(x, y)]) }    

let view model dispatch =
    [
        yield! [0 .. 10 .. 600] |> Seq.map (fun x -> colour Colour.DarkGray (1, 600) (x, 0))
        yield! [0 .. 10 .. 600] |> Seq.map (fun y -> colour Colour.DarkGray (600, 1) (0, y))
        yield onclickpoint (fun inputs ->
            let x, y = inputs
            dispatch (AddBox (x - x%10, y - y%10))            
        )
        yield onupdate (fun inputs ->
            let interval = 100L
            if(inputs.totalGameTime - model.tickCount) >= interval then
                dispatch (Tick inputs.totalGameTime)
            )
        yield! model.boxes |> Seq.map (fun x -> colour Colour.Black (10, 10) x )
    ]

[<EntryPoint>]
let main _ =
    let config = {
        resolution = Windowed (600, 600)
        clearColour = Some Colour.SlateGray
        mouseVisible = true
        assetsToLoad = [
            PipelineFont ("defaultFont", "./content/SourceCodePro")
        ]
    }
    Program.mkSimple init update view
    |> Program.withConsoleTrace
    |> Xelmish.Program.runGameLoop config
    0
