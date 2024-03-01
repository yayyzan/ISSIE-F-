module SheetBeautifyHelpers
open System
open CommonTypes
open DrawModelType
open DrawModelType.SheetT
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open Optics
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


    /// Return the list of segment vectors with 3 vectors coalesced into one visible equivalent
    /// wherever this is possible
    let rec coalesce (segVecs: XYPos list)  =
        match List.tryFindIndex (fun segVec -> segVec =~ XYPos.zero) segVecs[1..segVecs.Length-2] with          
        | Some zeroVecIndex ->
            let index = zeroVecIndex + 1 // base index onto full segVecs
            segVecs[0..index-2] @
            [segVecs[index-1] + segVecs[index+1]] @
            segVecs[index+2..segVecs.Length - 1]
            |> coalesce
        | None -> segVecs
    
    wire.Segments
    |> List.mapi getSegmentVector
    |> coalesce


//------------------------------------------------------------------------------------------------//
//--------------------------------------B Helper Functions----------------------------------------//
//------------------------------------------------------------------------------------------------//


/// <summary> Lens for retrieving/setting the dimensions of a custom component symbol</summary>
/// <param name="symbol"> The custom component symbol</param>
/// <param name="newDimensions"> New dimensions in form (height, width) if you wish to set them</param>
/// <returns> The dimensions of the custom component symbol in the form (height, width) 
///           OR the symbol with new dimensions
/// </returns>
// B1RW
let customComponentDimensions_ =
    let getCCDimensions (symbol: Symbol) = 
        getRotatedHAndW symbol
    
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


type SegmentDirection =
    | Up
    | Down
    | Left
    | Right


let listToTuple3 (list: 'T list) =
            match list with
            | [a; b; c] -> (a, b, c)
            | _ -> failwith "Expected list with 3 elements"

            
let calculateSegDirection (startPos: XYPos) (endPos: XYPos) =
    if startPos.X = endPos.X then 
        if startPos.Y < endPos.Y then Some Up
        else Some Down 
    elif startPos.Y = endPos.Y then 
        if startPos.X < endPos.X then Some Right
        else Some Left
    else None // Shouldn't happen with visible segments


let getWiresWithVisibleSegmentVectors (model: SheetT.Model) = 
    model.Wire.Wires
    |> Map.toList
    |> List.map (fun (cId,wire) ->
        visibleSegments cId model
        |> (fun visSegmentVectors -> (cId, wire, visSegmentVectors)))


// return a list with (start pos, end pos) for visible segments of a wire, taking visible sectors vectors as parameter
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


let segmentIntersectsASymbol (model: SheetT.Model) (wire: Wire) (segStart: XYPos) (segEnd: XYPos) = 
    let boundingBoxes =
        model.BoundingBoxes
        |> Map.remove (getSourceSymbol model.Wire wire).Id
        |> Map.remove (getTargetSymbol model.Wire wire).Id
        |> Helpers.mapValues 
        |> Array.toList
        
    boundingBoxes
    |> List.exists (fun box ->
                    segmentIntersectsBoundingBox box segStart segEnd 
                    |> (function
                        | Some _ -> true
                        | None -> false))


// Determine if two Asegments intersect at right angle, and return intersection position if they intersect
let intersectsAtRightAngle (seg1: ASegment) (seg2: ASegment) =
    let isHorizontal (segment: ASegment) = segment.Start.Y = segment.End.Y
    let isVertical (segment: ASegment) = segment.Start.X = segment.End.X

    if not (isHorizontal seg1 || isHorizontal seg2) then
        None
    else
        let (horizontalSeg, verticalSeg) =
            if isHorizontal seg1 then (seg1, seg2)
            else (seg2, seg1)

        let horizontalY = horizontalSeg.Start.Y
        let verticalX = verticalSeg.Start.X

        if (isHorizontal seg1 && isVertical seg2) || (isVertical seg1 && isHorizontal seg2) then
            // One segment is horizontal and the other is vertical
            if verticalX >= min horizontalSeg.Start.X horizontalSeg.End.X &&
               verticalX <= max horizontalSeg.Start.X horizontalSeg.End.X &&
               horizontalY >= min verticalSeg.Start.Y verticalSeg.End.Y &&
               horizontalY <= max verticalSeg.Start.Y verticalSeg.End.Y then
               Some { X = verticalX; Y = horizontalY } // Intersection
            else None
        else None


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
    |> (fun x -> x / 2) // divide by 2 since each pair counted twice


// The number of distinct wire visible segments that intersect with one or more symbols
// Get source and target symbol, dont count intersections with these
// T2R
let countVisibleSegmentSymbolIntersections (model: SheetT.Model) = 
    getWiresWithAbsVisibleSegments model
    |> List.map (fun (_,wire,absVisSegements) ->
        absVisSegements
        |> List.filter (fun (startPos,endPos) ->
            segmentIntersectsASymbol model wire startPos endPos)
        |> List.length) // num of segments on wire which overlap a BB
    |> List.reduce(+)


// The number of distinct pairs of segments that cross each other at right angles.
// getnonzeroAsegs 
// -> get all pairs of segs
// -> write intersection at right angle function that gives pos
// -> is pos = to any start or end pos of the segments then dont count 
// otherwise +1
// T3R 
let countIntersectingSegmentPairs (model: SheetT.Model) =
    let nonZeroASegments = Helpers.mapValues model.Wire.Wires
                           |> Array.toList
                           |> List.collect getNonZeroAbsSegments
    
    let intersectsWithAnEnd (pos: XYPos) (seg1: ASegment) (seg2: ASegment) =
        pos = seg1.Start ||  pos = seg1.End ||  pos = seg2.Start ||  pos = seg2.End 

    
    List.allPairs nonZeroASegments nonZeroASegments 
    |> List.map (fun (seg1, seg2) -> match intersectsAtRightAngle seg1 seg2 with
                                        | None -> 0
                                        | Some pos -> match intersectsWithAnEnd pos seg1 seg2 with
                                                        | true -> 0
                                                        | false -> 1)  
    |> List.reduce(+)
    |> (fun x -> x / 2) 


// T4 use duplicate visible segments to determine overlap
// Sum of (visible) wiring segment length
// currently counts both if segment contained within another, only removes exact duplicates
// could check if contained and remove inner
// group by net, then group by x, group by y. keep longest from each one
// or don't group by net, just same x/y
// List.groupBy
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


// Make list of vertices for each wire, and note in which direction the wire is leaving that vertex
// Then perform List.distinct to get only 1 right angle where multiple same-net wires are leaving the vertex in same direction
// T5R
let countVisibleRightAngles (model: SheetT.Model) =
    getWiresWithAbsVisibleSegments model
    |> List.collect (fun (_,_,absVisSegements) ->
        absVisSegements
        |> List.tail // remove first since vertex taken would be on symbol
        |> List.map (fun (startPos, endPos) ->
            (startPos, calculateSegDirection startPos endPos)))
    |> List.distinct
    |> List.length


// Return from one function a list of all the
// segments that retrace, and also a list of all the end of wire segments that retrace so
// far that the next segment (index = 3 or Segments.Length – 4) - starts inside a symbol.
// T6R 
let getRetracedSegments (model: SheetT.Model) =  
    let wires = Helpers.mapValues model.Wire.Wires
                |> Array.toList

    // empty array if none
    let getRetraced (wire: Wire) = 
        let segList = wire.Segments
        ([], segList)
        ||> List.fold (fun outputList curSeg -> 
                let index = curSeg.Index
                if curSeg.Length = 0
                then
                    if Math.Sign(segList[index-1].Length) <> Math.Sign(segList[index+1].Length) // these can't be zero 
                    then 
                        let retracedSegs = (segList[index-1],segList[index],segList[index+1])
                        retracedSegs :: outputList
                    else 
                        outputList
                else 
                    outputList ) 
        |> List.rev // to get in order found
        

    // assume first wire cannot go back into symbol
    let getRetracedIntoSymbol (wire: Wire) = 
        let segList = wire.Segments
                
        let startSegs = [segList[0];segList[1];segList[2]]
        let endIndex = (segList.Length) - 1
        // endSegs flipped so can be applied to same function
        let endSegsRev = [segList[endIndex];segList[endIndex-1];segList[endIndex-2]]

        let checkSegs (segs: List<Segment>) =
            if segs[1].Length = 0 && Math.Sign(segs[0].Length) <> Math.Sign(segs[2].Length)
            then
                if abs(segs[2].Length) > abs(segs[0].Length)
                then true
                else false
            else false
            
        if checkSegs startSegs && checkSegs endSegsRev
            then Some [listToTuple3 startSegs; listToTuple3 (List.rev <| endSegsRev)]
        elif checkSegs startSegs
            then Some [listToTuple3 startSegs]
        elif checkSegs endSegsRev 
            then Some [listToTuple3 (List.rev <| endSegsRev)]
        else None


    let allRetracedSegments =
        wires
        |> List.collect getRetraced
        |> (function
            | [] -> None
            | x -> Some x)
        

    let allRetracedIntoSymbol =
        wires
        |> List.collect (fun wire ->
            match getRetracedIntoSymbol wire with
            | Some list -> list 
            | None -> [])
        |> (function
            | [] -> None
            | x -> Some x)
        

    {| allRetracedSegments = allRetracedSegments; 
       allSegmentsRetracedIntoSymbol = allRetracedIntoSymbol |}

