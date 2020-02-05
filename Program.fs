open System
open Elmish
open Xelmish.Model
open Xelmish.Viewables
open xelmish_template.Controls

type cfg = {
    TileWidth: int
    ScreenWidth: int
    ScreenHeight: int
}

let conf = {
    TileWidth = 100
    ScreenWidth = 500
    ScreenHeight = 500
}

type Tile = {
    colour: Colour
}

//type Tile =
//    | Grain of Grain
//    | None


type Model = {
    tickCount: int64
    boxes: Tile option [,]
}

 

let initboxes =
    Array2D.init (conf.ScreenWidth / conf.TileWidth) (conf.ScreenHeight / conf.TileWidth) (fun x y  ->
        None
    )



let printBoxes (bxs: Tile option [,]) =
    [0 .. bxs.GetLength 0 - 1] |> Seq.iter (fun y ->
        [0 .. bxs.GetLength 1 - 1] |> Seq.iter (fun x ->
            match bxs.[x, y] with
            | Some g -> printf " X "
            | None -> printf " O "
            )
        printfn "" 
    )
    printfn "-----------------"

let init () =
    let newModel = {
        tickCount = 0L
        boxes = initboxes
    }
    printfn "%A" newModel
    newModel

type Message =
    | Tick of int64
    | AddBox of int*int
    
    
let tryMoveDown (board: Tile option [,]) (position: int*int) =
    let x, y = position
    if y < board.GetLength 0 then
        if board.[x, y + 1].IsNone then
            (x, y + 1)
        else if x < board.Length && board.[x + 1, y + 1].IsNone then
            (x + 1, y + 1)    
        else if x - 1 >= 0 &&  board.[x - 1, y + 1].IsNone then
            (x - y, y + 1)
        else
            position
    else
        position 
  
let updatePositions (bxs: Tile option [,]) =
    for y in 0 .. bxs.GetLength 1 - 1 do
        for x in 0 .. bxs.GetLength 0 - 1 do
            let box = bxs.[x,y]
            if box.IsSome then
                let (newx, newy) = tryMoveDown bxs (x, y)
                bxs.[x, y] <- None
                bxs.[newx, newy] <- box
                
let update message model =
    match message with
    | Tick x ->
        //printBoxes model.boxes
        
        {model with tickCount = x}
    | AddBox (x, y) ->
        model.boxes.[x, y] <- Some {colour= Colour.Black}
        model



let mapScreenPositionIntoTiles position =
    let x, y = position
    (x / conf.TileWidth, y / conf.TileWidth)



let view model dispatch =
    [
        yield onclickpoint (fun inputs ->
            let x, y = inputs
            dispatch (AddBox (mapScreenPositionIntoTiles (x, y)) )            
        )
        for y in 0 ..  (model.boxes.GetLength 1) - 1 do
            for x in 0 .. (model.boxes.GetLength 0)- 1 do
                match model.boxes.[x, y] with
                | Some g -> yield colour g.colour (conf.TileWidth, conf.TileWidth) (x * conf.TileWidth, y * conf.TileWidth)
                | None -> yield colour Colour.Gray (conf.TileWidth, conf.TileWidth) (x * conf.TileWidth, y * conf.TileWidth)
        
        yield onupdate (fun inputs ->
            let interval = 100L
            if(inputs.totalGameTime - model.tickCount) >= interval then
                dispatch (Tick inputs.totalGameTime)
        )
    ]                
    

[<EntryPoint>]
let main _ =
    let config = {
        resolution = Windowed (conf.ScreenWidth, conf.ScreenHeight)
        clearColour = Some Colour.SlateGray
        mouseVisible = true
        assetsToLoad = [
            PipelineFont ("defaultFont", "./content/SourceCodePro")
        ]
    }
    Program.mkSimple init update view
    //|> Program.withConsoleTrace
    |> Xelmish.Program.runGameLoop config
    0
