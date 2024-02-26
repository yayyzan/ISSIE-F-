module SheetBeautifyHelpers
open CommonTypes
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open Optics
open Optics.Operators
open BlockHelpers
open Symbol

//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team

type Lens<'A,'B> = ('A -> 'B) * ('B -> 'A -> 'A)

// dimensions in form (height * width)
let customComponentDimensionsB1RW_ =
    
    let getCCDimensions (symbol: Symbol) = 
        let h = symbol.Component.H
        let w = symbol.Component.W
        (h, w)
    
    let setCCDimensions (newDimensions: (float * float)) (symbol: Symbol) = 
        let newHeight, newWidth = newDimensions
        setCustomCompHW newHeight newWidth symbol

    Lens.create getCCDimensions setCCDimensions

// Update the position of a symbol on the sheet
let updateSymbolPositionB2W (model: SheetT.Model) (symbol: Symbol)(newPos: XYPos) = 

    let newComponent = {symbol.Component with X = newPos.X; Y = newPos.Y}

    let updatedSymbol = {symbol with 
                            Pos = newPos;
                            Component = newComponent}
                        |> calcLabelBoundingBox

    let updatedBB = getSymbolBoundingBox updatedSymbol

    let updatedBBMap = 
                model.BoundingBoxes
                |> Map.change symbol.Id (fun x ->
                    match x with
                    | Some sym -> Some updatedBB
                    | None -> None )

    model
    |> Optic.set (SheetT.symbolOf_ symbol.Id) updatedSymbol
    |> Optic.set SheetT.boundingBoxes_ updatedBBMap