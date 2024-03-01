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

// B1RW
/// <summary> Lens for retrieving/setting the dimensions of a custom component symbol</summary>
/// <param name="symbol"> The custom component symbol</param>
/// <param name="newDimensions"> New dimensions in form (height, width) if you wish to set them</param>
/// <returns> 
/// The dimensions of the custom component symbol in the form (height, width)
/// <para>
/// OR the symbol with new dimensions
/// </para> 
/// </returns>
let customComponentDimensions_ =
    let getCCDimensions (symbol: Symbol) = 
        getRotatedHAndW symbol
    
    let setCCDimensions (newDimensions: (float * float)) (symbol: Symbol) = 
        let newHeight, newWidth = newDimensions
        setCustomCompHW newHeight newWidth symbol

    Lens.create getCCDimensions setCCDimensions


// B2W
/// <summary> Update the position of a symbol on the sheet</summary>
/// <param name="model"> Model of the sheet</param>
/// <param name="symbol"> The symbol to have position adjusted</param>
/// <param name="newPos"> New position of top left of symbol </param>
/// <returns> A new sheet model with the postion of the symbol set to newPos</returns>
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


// B3R
/// <summary> Read the order of ports on a specified side of a symbol</summary>
/// <param name="symbol"> The symbol from which port order should be read</param>
/// <param name="edge"> The side on which port order should be read</param>
/// <returns> A list containing the port ids on the specified edge in order</returns>
let getPortOrderOnEdge (symbol: Symbol) (edge: Edge) = 
        (Optic.get portMaps_ symbol).Order[edge]
       
// B3W
/// <summary> Write the order of ports on a specified side of a symbol</summary>
/// <param name="symbol"> The symbol on which port order should be written</param>
/// <param name="edge"> The side on which port order should be written</param>
/// <param name="newPortOrder"> List containing port ids in desired order</param>
/// <returns> Symbol with updated port order on specified edge</returns>
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


// B4RW
/// <summary> Lens for retrieving/setting the ReversedInputPorts state of a symbol </summary>
/// <remarks> Used for MUX and DEMUX components</remarks>
/// <param name="symbol"> The symbol </param>
/// <param name="bool option"> The new ReversedInputPorts state if setting </param>
/// <returns> 
/// The ReversedInputPorts state of a symbol
/// <para>
/// OR the symbol with updated ReversedInputPorts state
/// </para> 
/// </returns>
let reversedInputPorts_ = 
    Lens.create (fun symbol -> symbol.ReversedInputPorts) 
                (fun newState symbol -> {symbol with ReversedInputPorts = newState})


// B5R
/// <summary> Get the XY position of a port on the sheet </summary>
/// <param name="model"> Model of the sheet</param>
/// <param name="portId"> The id of a port</param>
/// <returns> The XY position of the port specified by portId </returns>
let getPortPosition (model: SheetT.Model) (portId : string) = 
    let symModel = Optic.get SheetT.symbol_ model

    getPortLocation None symModel portId


// B6R
/// <summary> Get the bounding box a symbol </summary>
/// <remarks> There is an existing function in issie for this </remarks> 
/// <param name="symbol"> The symbol </param>
/// <returns> The bounding box of the specified symbol </returns>
let getSymbolBoundingBox (symbol: Symbol) = 
    getSymbolBoundingBox symbol


// B7RW
/// <summary> Lens for getting/setting the rotation state of a symbol </summary>
/// <param name="Symbol"> The symbol </param>
/// <param name="Rotation"> The new rotation state if setting </param>
/// <returns> 
/// The rotation state of a symbol
/// <para>
/// OR the symbol with updated rotation state
/// </para> 
/// </returns>
let symbolRotationState_ = 
    Lens.create (fun symbol -> symbol.STransform.Rotation) 
                (fun newRotation symbol -> {symbol with STransform = {symbol.STransform with Rotation = newRotation}})


// The flip state of a symbol
// B8RW
/// <summary> Lens for getting/setting the flip state of a symbol </summary>
/// <param name="Symbol"> The symbol </param>
/// <param name="bool"> The new flip state if setting </param>
/// <returns> 
/// The flip state of a symbol
/// <para>
/// OR the symbol with updated flip state
/// </para> 
/// </returns>
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


/// <summary>
/// Converts a list with three elements into a tuple of three values.
/// </summary>
/// <typeparam name="T">The type of elements in the list.</typeparam>
/// <param name="list">The input list.</param>
/// <returns>
/// A tuple of three values containing the elements from the input list.
/// </returns>
/// <exception cref="System.InvalidOperationException">
/// Thrown when the input list does not contain exactly three elements.
/// </exception>
let listToTuple3 (list: 'T list) =
            match list with
            | [a; b; c] -> (a, b, c)
            | _ -> failwith "Expected list with 3 elements" // Should not happen with correct usage


/// <summary>
/// Calculates the direction of a line segment defined by its start and end positions
/// </summary>
/// <param name="startPos">The starting position of the line segment</param>
/// <param name="endPos">The ending position of the line segment</param>
/// <returns>
/// A value indicating the direction of the line segment
/// </returns>
let calculateSegDirection (startPos: XYPos) (endPos: XYPos) =
    if startPos.X = endPos.X then 
        if startPos.Y < endPos.Y then Some Up
        else Some Down 
    elif startPos.Y = endPos.Y then 
        if startPos.X < endPos.X then Some Right
        else Some Left
    else None // Shouldn't happen with visible segments since always horizontal or vertical


/// <summary>
/// Retrieves wires with visible segment vectors from sheet model.
/// </summary>
/// <param name="model">The sheet model</param>
/// <returns>
/// A list of tuples containing:
/// - The component ID associated with the wire.
/// - The wire.
/// - The visible segment vectors of the wire.
/// </returns>
let getWiresWithVisibleSegmentVectors (model: SheetT.Model) = 
    model.Wire.Wires
    |> Map.toList
    |> List.map (fun (cId,wire) ->
        visibleSegments cId model
        |> (fun visSegmentVectors -> (cId, wire, visSegmentVectors)))


/// <summary>
/// Returns a list of tuples representing the absolute start and end positions of visible segments of a wire.
/// </summary>
/// <param name="wire">The wire for which absolute visible segments are determined.</param>
/// <param name="visSegmentVectors">The list of visible segment vectors.</param>
/// <returns>
/// A list of tuples where each tuple contains:
/// - The start position of a visible segment.
/// - The end position of the same visible segment.
/// </returns>
let getAbsVisibleSegments (wire: Wire) (visSegmentVectors: List<XYPos>) = 
    ([(wire.StartPos, wire.StartPos)], visSegmentVectors)
    ||> List.fold (fun posTupleList curVec -> 
            let (prev, curPos) = List.head posTupleList
            let newPos = curPos + curVec
            (curPos, newPos) :: posTupleList) 
    |> List.rev // Reverse the list to get the correct order
    |> List.tail // Remove first element which is just start pos


/// <summary>
/// Retrieves wires with absolute visible segments from sheet model.
/// </summary>
/// <param name="model">The sheet model</param>
/// <returns>
/// A list of tuples containing:
/// - The component ID associated with the wire.
/// - The wire.
/// - The list of absolute visible segments of the wire.
/// </returns>
let getWiresWithAbsVisibleSegments (model: SheetT.Model) = 
    getWiresWithVisibleSegmentVectors model
    |> List.map (fun (cId, wire, visSegmentVectors) ->
        (cId, wire, getAbsVisibleSegments wire visSegmentVectors))


/// <summary>
/// Checks if a line segment intersects with at least one symbol bounding box in the given sheet model,
/// excluding the source and target symbols of the wire the segment belongs to.
/// </summary>
/// <param name="model">The sheet model containing the symbols and bounding boxes.</param>
/// <param name="wire">The wire which segment belongs to</param>
/// <param name="segStart">The start position of the line segment.</param>
/// <param name="segEnd">The end position of the line segment.</param>
/// <returns>
/// <see langword="true"/> if the line segment intersects with any symbol bounding box excluding the source and target symbols of the wire; otherwise, <see langword="false"/>.
/// </returns>
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


/// <summary>
/// Determines if two Asegments intersect at a right angle and returns the intersection position if they intersect.
/// </summary>
/// <param name="seg1">The first Asegment.</param>
/// <param name="seg2">The second Asegment.</param>
/// <returns>
/// The intersection XY position if the line segments intersect at a right angle; otherwise, <see langword="None"/>.
/// </returns>
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

// All functions with the exception of T6R have been tested in issie 
// by passing current sheet model and checking if function output corresponds to what is displayed


// T1R
/// <summary>
/// Counts the number of pairs of symbols that intersect each other on a given sheet model.
/// </summary>
/// <param name="model">The sheet model containing the symbols and their bounding boxes.</param>
/// <returns>
/// The number of pairs of symbols that intersect each other.
/// </returns>
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


// T2R
/// <summary>
/// Counts the number of distinct wire visible segments that intersect with one or more symbols in the model.
/// </summary>
/// <param name="model">The sheet model containing the wires and symbols.</param>
/// <returns>
/// The number of distinct wire visible segments that intersect with one or more symbols.
/// </returns>
let countVisibleSegmentSymbolIntersections (model: SheetT.Model) = 
    getWiresWithAbsVisibleSegments model
    |> List.map (fun (_,wire,absVisSegements) ->
        absVisSegements
        |> List.filter (fun (startPos,endPos) ->
            segmentIntersectsASymbol model wire startPos endPos)
        |> List.length) // num of segments on wire which overlap a BB
    |> List.reduce(+)


// T3R
/// <summary>
/// Counts the number of distinct pairs of segments that cross each other at right angles.
/// </summary>
/// <remarks>
/// The function first retrieves all non-zero-length absolute segments from the wires in the model.
/// It then calculates all possible pairs of these segments.
/// <para>
/// For each pair of segments, it checks if they intersect at a right angle.
/// </para>
/// <para>
/// If they do, it checks if the intersection position coincides with the start or end position of any segment in the pair.
/// This is to remove from the count same net wires intersecting at one or both ends. If not, it increments the count by one. 
/// </para>
/// Finally, the function divides the total count by two since each intersecting pair is counted twice.
/// <para> NB Segments on same net on top of each other are not counted since these do not intersect at right angles</para>
/// </remarks>
/// <param name="model">The sheet model containing the wires and segments.</param>
/// <returns>
/// The number of distinct pairs of segments that intersect each other at right angles.
/// </returns>
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
    |> (fun x -> x / 2) // divide by 2 since each pair counted twice


// T4R
/// <summary>
/// Calculates the sum of the lengths of visible wiring segments in the model.
/// </summary>
/// <remarks>
/// The function first retrieves all distinct absolute visible segments from the wires in the model,
/// considering duplicate visible segments to determine overlap.
/// <para>
/// It then calculates the length of each segment and sums them up.
/// </para>
/// </remarks>
/// <param name="model">The sheet model containing the wires and segments.</param>
/// <returns>
/// The total length of visible wiring segments in the model.
/// </returns>
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


// T5R
/// <summary>
/// Counts the number of visible wire right-angles in the sheet model
/// </summary>
/// <remarks>
/// The function first retrieves absolute visible segments for each wire in the model.
/// For each wire, the first segment is removed since the vertex taken from it later on would be on a symbol.
/// <para>
/// The function then obtains a list of vertices from the remaining segments,
/// and calculates the direction in which the wire is leaving each vertex using <see cref="calculateSegDirection"/>.
/// </para>
/// <para>
/// The function then applies <see cref="List.distinct"/> to the list of vertices and directions to get only one vertex 
/// where multiple same-net wires are leaving the vertex in the same direction.
/// </para>
/// Finally, it counts the number of distinct vertices which corresponds to the number of visible wire right angles.
/// </remarks>
/// <param name="model">The sheet model containing the wires and segments.</param>
/// <returns>
/// The number of visible wire right-angles in the sheet model
/// </returns>
let countVisibleRightAngles (model: SheetT.Model) =
    getWiresWithAbsVisibleSegments model
    |> List.collect (fun (_,_,absVisSegements) ->
        absVisSegements
        |> List.tail // remove first since vertex taken would be on symbol
        |> List.map (fun (vertex, endPos) ->
            (vertex, calculateSegDirection vertex endPos)))
    |> List.distinct
    |> List.length


// T6R
/// <summary>
/// Retrieves all retraced wire segments and all segments retraced into symbols from the given sheet model.
/// </summary>
/// <remarks>
/// The function first retrieves a list of all wires from the model.
/// For each wire, it checks separately for retraced segments and for segments that are retraced into symbols.
/// <para>
/// Retraced segments are those where the lengths of the two segments on either side of a zero length segment have opposite signs.
/// </para>
/// <para>
/// Segments retraced into symbols are those where the wire retraces so far 
/// that the next segment starts inside or on the wrong side of a symbol.
/// </para>
/// These occur when the 2nd non-zero segment from either end of a wire has a 
/// length which has the opposite sign to and an absolute value greater than 
/// the length of the 1st non-zero segment at the same end of the wire.
/// <para>
/// NB The tuples returned in the lists contain 3 segments: 
/// the two segments that retrace AND the 0 length segment inbetween them
/// </para>
/// </remarks>
/// <param name="model">The sheet model containing the wires and symbols.</param>
/// <returns>
/// A record containing:
/// - <c>allRetracedSegments</c>: A list of all retraced wire segments.
/// - <c>allSegmentsRetracedIntoSymbol</c>: A list of all segments that retrace into a symbol.
/// </returns>
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
        // endSegs reversed so can be applied to same function
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
