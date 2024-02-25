module SheetBeautifyHelpers

open CommonTypes
open Optics
open Operators
open DrawModelType.SymbolT
open DrawModelType.SheetT
open Symbol
open EEExtensions
open BlockHelpers
open DrawModelType




//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team
let readDimensions_B1R (symbol: Symbol) : (float * float) =
    let width =
        Option.defaultValue 1.0 symbol.HScale
        * symbol.Component.W
    let height =
        Option.defaultValue 1.0 symbol.VScale
        * symbol.Component.H
    (width, height)

let writeDimensions_B1W (desiredWidth: float) (desiredHeight: float) (symbol: Symbol) : Symbol =
    let originalWidth = symbol.Component.W
    let originalHeight = symbol.Component.H
    let newHScale = desiredWidth / originalWidth
    let newVScale = desiredHeight / originalHeight

    { symbol with HScale = Some newHScale; VScale = Some newVScale }
let updateSymbolPosition_B2W (newPos: XYPos) (givenSymbol: SymbolT.Symbol) (sheet: SheetT.Model) =
    let symbolModel = sheet.Wire.Symbol
    let tryCompId =
        symbolModel.Symbols
        |> Map.toList
        |> List.tryFind (fun (_, s) -> s = givenSymbol) 
        |> Option.map fst

    match tryCompId with
    | Some compId ->
        let updatePosition (symbol: SymbolT.Symbol) = { symbol with Pos = newPos }
        let updatedSymbols =
            symbolModel.Symbols
            |> Map.map (fun key s ->
                if key = compId then
                    updatePosition s
                else
                    s)
        let updatedSymbolModel = { symbolModel with Symbols = updatedSymbols }
        let updatedWireModel = { sheet.Wire with Symbol = updatedSymbolModel }
        { sheet with Wire = updatedWireModel }
    | None -> sheet

let readPortsOrder_B3R (edge: Edge) (symbol: SymbolT.Symbol) : string list option =
    symbol.PortMaps.Order |> Map.tryFind edge

let writePortsOrder_B3W (edge: Edge) (newOrder: string list) (symbol: SymbolT.Symbol) : SymbolT.Symbol =
    let updatedOrderMap = Map.add edge newOrder symbol.PortMaps.Order
    let updatedPortMaps = { symbol.PortMaps with Order = updatedOrderMap }
    { symbol with PortMaps = updatedPortMaps }

let readReversedInputPorts_B4R (symbol: SymbolT.Symbol) : bool option = symbol.ReversedInputPorts

let writeReversedInputPorts_B4W (reversed: bool) (symbol: SymbolT.Symbol) : SymbolT.Symbol =
    { symbol with ReversedInputPorts = Some(reversed) }

let getPortPositionOnSheet_B5R (symbol: SymbolT.Symbol) (port: Port) : XYPos =
    let relativePortPos = getPortPos symbol port
    let symbolPos = symbol.Pos
    { X = symbolPos.X + relativePortPos.X; Y = symbolPos.Y + relativePortPos.Y }

let calculateBoundingBox_B6R (symbol: SymbolT.Symbol) : BoundingBox =
    let (width, height) = readDimensions_B1R symbol
    let topleft = symbol.Pos
    { TopLeft = topleft; W = width; H = height }

let readRotationState_B7R (symbol: SymbolT.Symbol) : Rotation = symbol.STransform.Rotation

let writeRotationState_B7W (desiredRotation: Rotation) (symbol: SymbolT.Symbol) : SymbolT.Symbol =
    let updatedSTransform = { symbol.STransform with Rotation = desiredRotation }
    { symbol with STransform = updatedSTransform }

let readFlipState_B8R (symbol: Symbol) : bool = symbol.STransform.Flipped

let writeFlipState_B8W (flip: bool) (symbol: SymbolT.Symbol) : SymbolT.Symbol =
    let updatedSTransform = { symbol.STransform with Flipped = flip }
    { symbol with STransform = updatedSTransform }


let doBoundingBoxesIntersect (box1: BoundingBox) (box2: BoundingBox) : bool =
    let box1Right = box1.TopLeft.X + box1.W
    let box1Bottom = box1.TopLeft.Y + box1.H
    let box2Right = box2.TopLeft.X + box2.W
    let box2Bottom = box2.TopLeft.Y + box2.H

    not (box1Right < box2.TopLeft.X || box2Right < box1.TopLeft.X ||
         box1Bottom < box2.TopLeft.Y || box2Bottom < box1.TopLeft.Y)

let countIntersectingSymbolPairs_T1R (sheet: SheetT.Model) = 
    // let symbols = sheet.Wire.Symbol.Symbols
    let symbols = sheet.Wire.Symbol.Symbols |> Map.toList |> List.map snd   

    let pairs = 
        symbols 
        |> List.mapi (fun i sym1 -> symbols |> List.skip (i + 1) |> List.map (fun sym2 -> (sym1, sym2)))
        |> List.concat

    let intersectingPairs = 
        pairs
        |> List.filter (fun (s1, s2) -> doBoundingBoxesIntersect (calculateBoundingBox_B6R s1) (calculateBoundingBox_B6R s2))
    
    intersectingPairs 
    |> List.length


// let countDistinctWireSegmentsIntersectingSymbols (sheet: Model): int =
//     let symbols = Map.toList sheet.Wire.Symbol.Symbols |> List.map snd
//     let symbolBoxes = symbols |> List.map calculateBoundingBox_B6R

//     sheet.Wire.Wires
//     |> Map.toList
//     |> List.collect (fun (wId, wire) -> 
//         let visibleSegVectors = HLP3Tick.visibleSegments wId sheet
//         let initialPos = wire.StartPos
//         visibleSegVectors
//         |> List.fold (fun (acc, lastPos) vec -> 
//             let newPos = {X = lastPos.X + vec.X; Y = lastPos.Y + vec.Y})
//             let segBox = {TopLeft = lastPos; W = abs vec.X; H = abs vec.Y}
//             let intersects = symbolBoxes |> List.exists (BlockHelpers.overlap2D segBox)
//             if intersects then (acc + 1, newPos) else (acc, newPos)
//         ) (0, initialPos) |> snd 
//     )
//     |> List.sum   

// module DrawModelType.SheetT

let visibleSegments (wId: ConnectionId) (model: SheetT.Model): XYPos list =
       let wire = model.Wire.Wires[wId] // get wire from model
       /// helper to match even and off integers in patterns (active pattern)
       let (|IsEven|IsOdd|) (n: int) = match n % 2 with | 0 -> IsEven | _ -> IsOdd
       /// Convert seg into its XY Vector (from start to end of segment).
       /// index must be the index of seg in its containing wire.
       let getSegmentVector (index:int) (seg: BusWireT.Segment) =
           // The implicit horizontal or vertical direction  of a segment is determined by 
           // its index in the list of wire segments and the wire initial direction
           match index, wire.InitialOrientation with
           | IsEven, BusWireT.Vertical | IsOdd, BusWireT.Horizontal -> {X=0.; Y=seg.Length}
           | IsEven, BusWireT.Horizontal | IsOdd, BusWireT.Vertical -> {X=seg.Length; Y=0.}
       /// Return a list of segment vectors with 3 vectors coalesced into one visible equivalent
       /// if this is possible, otherwise return segVecs unchanged.
       /// Index must be in range 1..segVecs
       let tryCoalesceAboutIndex (segVecs: XYPos list) (index: int)  =
           if segVecs[index] =~ XYPos.zero
           then
               segVecs[0..index-2] @
               [segVecs[index-1] + segVecs[index+1]] @
               segVecs[index+2..segVecs.Length - 1]
           else
               segVecs
       wire.Segments
       |> List.mapi getSegmentVector
       |> (fun segVecs ->
               (segVecs,[1..segVecs.Length-2])
               ||> List.fold tryCoalesceAboutIndex)


let countDistinctWireSegmentsIntersectingSymbols (sheet: Model): int =
    let symbols = Map.toList sheet.Wire.Symbol.Symbols |> List.map snd
    let symbolBoxes = symbols |> List.map calculateBoundingBox_B6R
    sheet.Wire.Wires
    |> Map.toList
    |> List.collect (fun (wId, wire) ->
        let visibleSegVectors = visibleSegments wId sheet // XYPos list for each segment
        let initialPos = wire.StartPos
        [visibleSegVectors
         |> List.fold (fun ((lastPos: XYPos), acc) vec -> 
             let newPos = { X = lastPos.X + vec.X; Y = lastPos.Y + vec.Y }
             let segBox = 
                 { TopLeft = lastPos; W = abs vec.X; H = abs vec.Y } // Assuming calculation for a bounding box
             let intersects = symbolBoxes |> List.exists (overlap2DBox segBox)
             if intersects then (newPos, acc + 1) else (newPos, acc)
         ) (initialPos, 0) |> snd] // Wrap the fold result in a list
    )
    |> List.sum // Sum the counts of intersections for each wire
