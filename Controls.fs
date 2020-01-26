module xelmish_template.Controls
open Xelmish.Viewables

let text = text "defaultFont"

let button displaytext click position size backcolor forecolour =
    let x, y = position
    let width, height = size
    [
        colour backcolor (width, height) (x, y)
        text 20. forecolour (-0.5, -0.5) displaytext (x + width/2, y+height/2)
        onclick click (width, height) (x, y)
    ]   
