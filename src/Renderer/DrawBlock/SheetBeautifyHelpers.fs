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

/// The visible segments of a wire, as a list of vectors, from source end to target end.
/// Note that in a wire with n segments a zero length (invisible) segment at any index [1..n-2] is allowed 
/// which if present causes the two segments on either side of it to coalesce into a single visible segment.
/// A wire can have any number of visible segments - even 1.
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
    /// Index must be in range >= 1
    let tryCoalesceAboutIndex (segVecs: XYPos list) (index: int)  =
        if index < segVecs.Length - 1 && segVecs[index] =~ XYPos.zero
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


//------------------------------------------------------------------------------------------------//
//--------------------------------------B Helper Functions----------------------------------------//
//------------------------------------------------------------------------------------------------//


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


//------------------------------------------------------------------------------------------------//
//--------------Some functions I have created to use in required T helper functions---------------//
//------------------------------------------------------------------------------------------------//

let getWiresWithVisibleSegmentVectors (model: SheetT.Model) = 
    model.Wire.Wires
    |> Map.toList
    |> List.map (fun (cId,wire) ->
        visibleSegments cId model
        |> (fun visSegmentVectors -> (cId, wire, visSegmentVectors)))


// return a list with (start pos, end pos) for visible segments of a wire
let getAbsVisibleSegments (wire: Wire) (visSegmentVectors: List<XYPos>) = 
    ([(wire.StartPos, wire.StartPos)], visSegmentVectors)
    ||> List.fold (fun posTupleList curVec -> 
            let (prev, curPos) = List.head posTupleList
            let newPos = curPos + curVec
            (curPos, newPos) :: posTupleList) 
    |> List.rev // Reverse the list to get the correct order
    |> List.tail // Remove first element which is just start pos


let getWiresWithAbsVisibleSegments (model: SheetT.Model) = 
    getWiresWithVisibleSegmentVectors model
    |> List.map (fun (cId, wire, visSegmentVectors) ->
        (cId, wire, getAbsVisibleSegments wire visSegmentVectors))


let segmentIntersectsASymbol (model: SheetT.Model) (segStart: XYPos) (segEnd: XYPos) = 
    let boundingBoxes =
        Helpers.mapValues model.BoundingBoxes
        |> Array.toList
        
    boundingBoxes
    |> List.exists (fun box ->
                    segmentIntersectsBoundingBox box segStart segEnd 
                    |> (function
                            | Some _ -> true
                            | None -> false))

//------------------------------------------------------------------------------------------------//
//--------------------------------------T Helper Functions----------------------------------------//
//------------------------------------------------------------------------------------------------//

// The number of pairs of symbols that intersect each other
// T1R
let countIntersectingSymbolPairs (model: SheetT.Model) = 
    let boundingBoxes =
        Helpers.mapValues model.BoundingBoxes
        |> Array.toList
        |> List.mapi (fun n box -> n,box)

    List.allPairs boundingBoxes boundingBoxes 
    |> List.map (fun ((n1,box1),(n2,box2)) -> match (n1 <> n2) && overlap2DBox box1 box2 with
                                                | true -> 1
                                                | false -> 0)
    |> List.reduce(+)
    |> (fun x -> x / 2) // get pairs


// The number of distinct wire visible segments that intersect with one or more symbols
// T2R
// currently counts wire intersection with source and dest
// If I did this per wire I could check if first or last and then check it has 2 intersections before true
let countVisibleSegmentSymbolIntersections (model: SheetT.Model) = 
    let wiresWithVisibleSegmentVectors = getWiresWithVisibleSegmentVectors model
    
    let allAbsVisibleSegments = 
        wiresWithVisibleSegmentVectors
        |> List.collect (fun (_,wire,visSegmentVectors) ->
            getAbsVisibleSegments wire visSegmentVectors)
                                                
    allAbsVisibleSegments
    |> List.filter (fun (startPos,endPos) ->
            segmentIntersectsASymbol model startPos endPos)
    |> List.length


// T4 use duplicate visible segments to determine overlap
// Sum of (visible) wiring segment length
// currently counts both if segment contained within another, only removes exact duplicates
// could check if contained and remove inner
// T4R
let totalVisibleWireSegmentLength (model: SheetT.Model) =
    let allDistinctAbsVisibleSegments = 
        getWiresWithVisibleSegmentVectors model
        |> List.collect (fun (_,wire,visSegmentVectors) ->
            getAbsVisibleSegments wire visSegmentVectors)
        |> List.distinct
     
    allDistinctAbsVisibleSegments
    |> List.map (fun (startPos, endPos) ->    
            abs(endPos.X - startPos.X + endPos.Y - startPos.Y))
    |> List.reduce (+)


// T5 just count visible segements - 1 for each wire
// issues when it comes to same net overlap on horizontal and vertical
// if same net then check if both destination ports are above or below source port, and if so -1 
// better to do with vertices
let countVisibleRightAngles (model: SheetT.Model) =
    getWiresWithAbsVisibleSegments model
    |> List.map (fun (_,_,absVisSegements) ->
        absVisSegements
        |> List.length
        |> (+) -1 )
    |> List.reduce(+)



// T3 just eliminate 0, check intersection where orientation different, remove segments where endA = StartB

// Same net segments are from wires with the same source port.
// BlockHelpers.partitionWiresIntoNets find with this

// when two segments are on the same net (same source port), 
// if the intersection is at the end of one or both of the segments, 
// this is a T junction in a connected net which is not a visual problem and therefore not counted as an intersection.

// similarly if two segments lie partly or wholly on top of each other, 
// and are on same net, it should not be counted as am intersection. 
// Note that in that case they have the same orientation: whereas for intersections the two segments always have opposite orientations.

