module SheetBeautifyHelpers
open CommonTypes
open DrawModelType
open DrawModelType.SheetT
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open Optics
open Optics.Operators
open BlockHelpers
open Symbol

//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team


// The dimensions of a custom component symbol
// dimensions in form (height * width)
// B1RW
let customComponentDimensions_ =
    let getCCDimensions (symbol: Symbol) = 
        let h = symbol.Component.H
        let w = symbol.Component.W
        (h, w)
    
    let setCCDimensions (newDimensions: (float * float)) (symbol: Symbol) = 
        let newHeight, newWidth = newDimensions
        setCustomCompHW newHeight newWidth symbol

    Lens.create getCCDimensions setCCDimensions


// Update the position of a symbol on the sheet
// B2W
let updateSymbolPosition (model: SheetT.Model) (symbol: Symbol)(newPos: XYPos) = 
    let updatedSymbol = {symbol with 
                            Pos = newPos;}
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
    |> Optic.set boundingBoxes_ updatedBBMap


// Read/write the order of ports on a specified side of a symbol

// B3R
let getPortOrderOnEdge (symbol: Symbol) (edge: Edge) = 
        (Optic.get portMaps_ symbol).Order[edge]
       
// B3W
let setPortOrderOnEdge (symbol: Symbol) (edge: Edge) (newPortOrder: list<string>) = 
        let newOrderMap = 
                (Optic.get portMaps_ symbol).Order
                |> Map.change edge (fun x ->
                    match x with
                    | Some sym -> Some newPortOrder
                    | None -> None )

        let newPortMap = Optic.get portMaps_ symbol
                         |> Optic.set order_ newOrderMap
        
        Optic.set portMaps_ newPortMap symbol


// The reverses state of the inputs of a MUX2
// B4RW
let reversedInputPorts_ = 
    Lens.create (fun symbol -> symbol.ReversedInputPorts) 
                (fun newState symbol -> {symbol with ReversedInputPorts = newState})


// The position of a port on the sheet. It cannot directly be written.
// B5R
let getPortPosition (model: SheetT.Model) (portId : string) = 
    let symModel = Optic.get SheetT.symbol_ model

    getPortLocation None symModel portId


// The Bounding box of a symbol outline (position is contained in this)
// B6R
let getSymbolBoundingBox (symbol: Symbol) = 
    getSymbolBoundingBox symbol

// The rotation state of a symbol
// B7RW
let symbolRotationState_ = 
    Lens.create (fun symbol -> symbol.STransform.Rotation) 
                (fun newRotation symbol -> {symbol with STransform = {symbol.STransform with Rotation = newRotation}})

// The flip state of a symbol
// B8RW
let symbolFlipState_ = 
    Lens.create (fun symbol -> symbol.STransform.Flipped) 
                (fun newFlip symbol -> {symbol with STransform = {symbol.STransform with Flipped = newFlip}})

