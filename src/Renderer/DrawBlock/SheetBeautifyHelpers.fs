module SheetBeautifyHelpers

open CommonTypes
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open Optics
open Optics.Operators
open BlockHelpers
open Helpers
open BusWire

//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team

// Helpers for the Helpers

/// <summary>
/// This function takes a wire and returns a list of visible segments by coalescing all zero length segments.
/// Each segment is represented as a tuple of start and end positions (XYPos * XYPos).
/// </summary>
/// <param name="wire">The wire for which to find visible segments.</param>
/// <returns>A list of tuples representing the visible segments of the wire.</returns>
let visibleSegments (wire : BusWireT.Wire): (XYPos * XYPos) list =

    let tryCoalesceAboutIndex ((remainingSegs : ASegment List, coalescedSegs : (XYPos * XYPos) list)) (_)  =
        match remainingSegs with
        | prev::curr::next::tail -> 
            if curr.IsZero
            then 
                let coalescedSeg = (prev.Start, next.End)
                (tail, coalescedSegs @ [coalescedSeg])
            else        
                ([curr;next] @ tail, coalescedSegs @ [(prev.Start,prev.End)])
                
        | seg1::seg2::tail when tail = [] -> ([], coalescedSegs @ [(seg1.Start,seg1.End);(seg2.Start,seg2.End)])
        | _ -> ([], coalescedSegs)

    wire
    |> getAbsSegments
    |> (fun segVecs ->
            ((segVecs,[]),segVecs)
            ||> List.fold tryCoalesceAboutIndex)
    |> snd

/// <summary>
/// Given a list, returns all unique pairings of its elements, ignoring ordering.
/// </summary>
/// <param name="lst">The list for which to find all distinct pairs.</param>
/// <returns>A list of tuples representing all distinct pairs in the input list.</returns>
let rec allDistinctPairs lst =
    match lst with
    | [] -> []
    | h::t -> List.allPairs [h] t @ allDistinctPairs t

// adapted from findWireSymbolIntersection
/// <summary>
/// Retrieves the bounding boxes of all symbols in a given sheet.
/// </summary>
/// <param name="sheet">The sheet containing symbols.</param>
/// <returns>A list of tuples where each tuple contains the component type and the bounding box of the symbol.</returns>
let allSymbolBBoxInSheet ( sheet : SheetT.Model) =
    sheet.Wire.Symbol.Symbols
    |> Map.values
    |> Seq.toList
    |> List.map (fun s -> (s.Component.Type, Symbol.getSymbolBoundingBox s))

// B1RW
/// <summary>
/// This function gets and sets the dimensions of a custom component symbol.
/// </summary>
/// <remarks>
/// The function is composed of two sub-functions:
/// - getCustomComponentDimensionB1R: This function takes a custom component symbol and returns its dimensions (height and width).
/// - setCustomComponentDimensionB1W: This function takes new dimensions and a custom component symbol, and returns a new symbol with updated dimensions.
/// </remarks>
/// <returns>A lens that can be used to get or set the dimensions of a custom component symbol.</returns>
let componentDimension_ =
    let getCustomComponentDimensionB1R (customComponent : SymbolT.Symbol) : {|H:float;W:float|} =
        let comp = Optic.get SymbolT.component_ customComponent

        {|H=comp.H;W=comp.W|}

    let setCustomComponentDimensionB1W (newDims : {|H:float;W:float|}) (customComponent : SymbolT.Symbol) : SymbolT.Symbol =
        let comp = Optic.get SymbolT.component_ customComponent

        customComponent 
        |> Optic.set SymbolT.component_ {comp with H = newDims.H; W = newDims.W}

    Lens.create getCustomComponentDimensionB1R setCustomComponentDimensionB1W

// B2W
/// <summary>
/// Sets the position of a symbol on a sheet using a predefined lens.
/// </summary>
/// <returns>The position of the symbol on the sheet.</returns>
let setSymbolPosition = snd SymbolT.posOfSym_

// B3RW
/// <summary>
/// Reads and writes the order of ports on a specified side of a symbol.
/// </summary>
/// <param name="side">The side of the symbol for which to get or set the order of ports.</param>
/// <returns>A lens that can be used to get or set the order of ports on the specified side of a symbol.</returns>
let orderOfPortsBySide_ (side : Edge) =  
    let orderOfEdge_ = 
        ((fun (orderMap : Map<Edge, string list>) -> Map.find side orderMap),
        (fun (newOrder : string list) orderMap -> Map.add side newOrder orderMap))
        ||> Lens.create  
    
    SymbolT.portMaps_
    >-> SymbolT.order_
    >-> orderOfEdge_

// Function 4 : The reversed state of the inputs of a MUX2
// B4RW
/// <summary>
/// Reads and writes the reversed state of the inputs of a MUX2.
/// </summary>
/// <returns>A Lens that can be used to access the reversed state of the inputs of a MUX2.</returns>
let reversedState_ = 
    Lens.create (fun a -> a.ReversedInputPorts) (fun s a -> {a with ReversedInputPorts = s})

// Function 5 : The position of a port on the sheet. It cannot directly be written.
// B5R
/// <summary>
/// Retrieves the position of a port on the sheet.
/// </summary>
/// <param name="portId">The ID of the port.</param>
/// <param name="sheet">The sheet containing the port.</param>
/// <returns>The position of the port on the sheet.</returns>
let getPortPosInSheet ( portId : string ) ( sheet : SheetT.Model ) = 
    Symbol.getPortLocation None ( Optic.get SheetT.symbol_ sheet ) portId

// Function 6 : The Bounding box of a symbol outline (position is contained in this)
// B6R
/// <summary>
/// Wrapper for exising function Symbol.getSymbolBoundingBox which returns the bounding box of the provided symbol
/// </summary>
/// <returns>The function Symbol.getSymbolBoundingBox.</returns>
let getBoundingBoxOfSymbolOutline = Symbol.getSymbolBoundingBox


let stransform_ = Lens.create (fun a -> a.STransform) (fun s a -> {a with STransform = s})
let rotation_ = Lens.create (fun a -> a.Rotation) (fun s a -> {a with Rotation = s})
let flip_ = Lens.create (fun a -> a.Flipped) (fun s a -> {a with Flipped = s})


// Function 7 : The rotation state of a symbol
// B7RW
/// <summary>
/// Retrieves the rotation state of a symbol.
/// </summary>
/// <param name="symbol">The symbol whose rotation state is to be retrieved.</param>
/// <returns>A Lens for the rotation state of a symbol.</returns>
let rotationOfSymbol_ = stransform_ >-> rotation_

// Function 8 : The flip state of a symbol
// B8RW
/// <summary>
/// Retrieves the flip state of a symbol.
/// </summary>
/// <param name="symbol">The symbol whose flip state is to be retrieved.</param>
/// <returns>A Lens for the flip state of a symbol.</returns>
let flipOfSymbol_ = stransform_ >-> flip_

// Function 9 : The number of pairs of symbols that intersect each other. See Tick3 for a related function. Count over all pairs of symbols
// T1R
/// <summary> Count the number of distinct pairs of Symbols which intersect in a sheet </summary>
/// <param name="sheet"> The sheet Model containing the current state of the sheet</param>
/// <returns> The final state value </returns>
let countSymbolIntersectPairs ( sheet : SheetT.Model ) = 
    let boxes = 
        mapValues sheet.BoundingBoxes
        |> Array.toList
        |> List.mapi (fun n box -> n,box)

    allDistinctPairs boxes
    |> List.filter (fun ((n1, box1),(n2,box2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox box1 box2)
    |> List.length

// Function 10 : The number of distinct wire visible segments that intersect with one or more symbols. See Tick3.HLPTick3.visibleSegments for a helper. Count over all visible wire segments.
// T2R
/// <summary>
/// Counts the number of distinct wire segments intersecting with symbols on a given sheet.
/// </summary>
/// <param name="sheet">The sheet containing wires and symbols.</param>
/// <returns>
/// An integer representing the count of distinct wire segments intersecting with the symbols in the sheet.
/// </returns>
/// <remarks>
/// The count returned includes the Input and Outport port segment intersections with the parent bounding box.
/// </remarks>
let countDistinctWireSegmentIntersectSymbol ( sheet : SheetT.Model ) = 
    let allVisibleSegmentsInSheet  ( sheet : SheetT.Model ) = 
        sheet.Wire.Wires
        |> Map.values
        |> Seq.collect visibleSegments 
        |> Seq.toList

    List.allPairs (allSymbolBBoxInSheet sheet) (allVisibleSegmentsInSheet sheet)
    |> List.filter (fun ((_,bb),(startpos,endpos)) -> 
        match segmentIntersectsBoundingBox bb startpos endpos with
        | None -> false
        | Some something -> true
    )
    |> List.length


type IntresectionType = 
    | CrossJunction 
    | TJunction

// function 11 : The number of distinct pairs of segments that cross each other at right angles. 
// Does not include 0 length segments or segments on same net intersecting at one end, or segments on same net on top of each other. Count over whole sheet.
// T3R
/// <summary>
/// Counts the number of distinct wire segments that intersect orthogonally on a sheet.
/// </summary>
/// <param name="sheet">The sheet containing wire segments.</param>
/// <returns>
/// An integer representing the count of distinct wire segments that intersect at right angles.
/// </returns>
/// <remarks>
/// <para>
/// The IntersectionType D.U. is used to distinguish between different types of intersections: CrossJunction and TJunction.
/// If a TJunction is found within segments belonging to the same net it is ignored.
/// </para>
/// </remarks>
let countVisibleRightAngleSegmentIntersection ( sheet : SheetT.Model) = 
    // get a list of segments which intersect at right angles
    // for each segment obtain asbolute start and end position
    // check orthogonality by checking each distinct segment pair to ensure they have opposite orientation and are within range of each other

    /// Returns an option that indicated whether there is an intersection, and the position of said intersection
    /// Expects two ASegments of opposite orientation
    let getIntersectionOpt (seg1 : ASegment) (seg2: ASegment) = 
        let hori, vert = match seg1.Orientation  with | Horizontal -> (seg1,seg2) | Vertical -> (seg2,seg1)
        
        let xmin, xmax = min hori.Start.X hori.End.X, max hori.Start.X hori.End.X 
        let ymin, ymax = min vert.Start.Y vert.End.Y, max vert.Start.Y vert.End.Y

        let isTJunction = 
            hori.Start.Y = ymax 
            || hori.Start.Y = ymin 
            || vert.Start.X = xmin 
            || vert.Start.X = xmax

        let intersectRightAngle = 
            (vert.Start.X <= xmax) && (vert.Start.X >= xmin)
            &&
            (hori.Start.Y <= ymax) && (hori.Start.Y >= ymin)

        (match intersectRightAngle with
        | false -> None
        | true -> 
            if isTJunction 
            then Some TJunction
            else Some CrossJunction
        , {X=vert.Start.X;Y=hori.Start.Y})

    let allSegments = 
        sheet.Wire.Wires
        |> Map.toList
        |> List.collect (fun (wId, wire) -> getNonZeroAbsSegments wire |> List.map (fun v -> (wire.OutputPort, v)))
        |> List.distinctBy (fun (netid, seg) -> (netid,seg.Start,seg.End))


    allDistinctPairs allSegments
    |> List.collect (fun ((netId1,seg1), (netId2,seg2)) ->
        if seg2.Orientation <> seg1.Orientation // to cross at right angles orientation must be opposite
        then
            match getIntersectionOpt seg1 seg2 with
            | None, _ -> []
            | Some TJunction, intPos -> if netId1 <> netId2 then [intPos] else [] 
            | Some CrossJunction, intPos -> [intPos]
        else
            []
    )
    // remove any overlapping intersections
    |> List.distinct
    |> List.length


// T4R
/// <summary>
/// Calculates the total length of wiring segments on a sheet, counting only one when there are multiple segments overlapping within the same net.
/// </summary>
/// <param name="sheet">The sheet containing wiring segments.</param>
/// <returns>
/// A float representing the total visible wire length on the sheet.
/// </returns>
/// <remarks>
/// <para>
/// This function considers only one segment when multiple segments overlap within the same net, but duplicates the length for overlapping segments belonging to different nets.
/// </para>
/// <para>
/// Further implementation details and discussion can be found in the body of the function
/// </para>
/// </remarks>
let wireSegmentLength (sheet : SheetT.Model) = 
    // Discussion on spec:
    // This spec decision follows the discussion on edstem where the assumption is that such a case cannot occur
    // as the autoroute prevents such behaviour.
    // Changing the function behaviour to ensure all overlapping segments (regardless of whether they belong to the same net)
    // can be implemented by removing all grouping based on the wire output port

    // Description of implementation algorithm:
    // loop over all wires
    // group wires by same source port
    // extract visible segments for all such wires
    // group by horizontal or vertical
    // for each one group by either the y or the x according to which one is the constant dimension
    // sort by the start position
    // fold over keeping track of the current length, and the start and end position of the overlapping segment
    //      if the current segment overlaps then the bounds are updated, 
    //      otherwise the length is updated with the length of the segment and the start and end position are set as the current segment

    let getNonOverlappedWireLength (_, wires : BusWireT.Wire list) =
        
        wires
        |> List.collect visibleSegments 
        |> List.partition (fun (s, e) -> getSegmentOrientation s e = Horizontal)
        |> (fun (hori,vert) ->
            printf "h: %A" hori;
            printf "v: %A" vert;
            let groupByOri fixedVal varVal = 
                List.groupBy (fun (s : XYPos,_ : XYPos) -> fixedVal s) 
                >> List.map (fun (_, lst) -> 
                    lst 
                    |> List.sortBy (fun (s,_) -> varVal s)
                    |> List.map (fun (s,e) -> 
                        let (sOut,eOut) = (varVal s, varVal e)
                        {|Start=min sOut eOut;End=max sOut eOut|})) 

            groupByOri (fun p -> p.Y) (fun p -> p.X) hori @ groupByOri (fun p -> p.X) (fun p -> p.Y) vert
        )
        |> List.map (fun (alignedSegs) -> 
            // printf "aligned: %A\n" alignedSegs;
            let getLen (seg : {| End: float; Start: float |}) = seg.End - seg.Start

            ((alignedSegs.Head, getLen alignedSegs.Head), alignedSegs)
            ||> List.fold (fun (overlap, totalLen) seg -> 
                match overlap1D (overlap.Start, overlap.End) (seg.Start,seg.End) with
                | true ->  ({| Start = min overlap.Start seg.Start; End = max overlap.End seg.End |}, totalLen)
                | false -> (seg, totalLen + getLen overlap)
            ) |> snd
        )
        |> List.fold (+) 0.0

    sheet
    |> Optic.get SheetT.wires_
    |> Map.values 
    |> Seq.toList
    |> List.groupBy (fun w -> w.OutputPort)
    |> List.map getNonOverlappedWireLength
    |> List.reduce (+)

// function 13 : Number of visible wire right-angles. Count over whole sheet.
/// <summary>
/// Counts the number of visible right angles in a given sheet.
/// </summary>
/// <param name="sheet">The sheet containing wires.</param>
/// <returns>The total count of visible right angles in the sheet.</returns>
// TODO FIX THIS BY LOOKING AT ALL VERTICES AND COUNTING THE UNIQUE ONES
let countVisibleRightAnglesT5R ( sheet : SheetT.Model) =
    sheet.Wire.Wires
    |> Map.values
    |> Seq.map (fun wire -> 
        visibleSegments wire 
        |> List.length 
        |> (fun res -> res / 2))
        // there as many right angles as half the number of visible segments
    |> Seq.reduce (+)

// function 14 : 
// The zero-length segments in a wire with non-zero segments on either side that have 
// Lengths of opposite signs lead to a wire retracing itself. Note that this can also apply
// at the end of a wire (where the zero-length segment is one from the end). This is a
// wiring artifact that should never happen but errors in routing or separation can
// cause it. Count over the whole sheet. Return from one function a list of all the
// segments that retrace, and also a list of all the end of wire segments that retrace so
// far that the next segment (index = 3 or Segments.Length – 4) - starts inside a symbol.

/// <summary>
/// Retrieves retrace segments from a given wire.
/// </summary>
/// <param name="wire">The wire containing segments.</param>
/// <returns>
/// An option list of segments triples that retrace, containing the previous, current, and next segments.
/// </returns>
let getRetraceSegmentsOfWire ( wire : BusWireT.Wire ) = 
    (([], wire.Segments), wire.Segments)
    ||> List.fold (fun (retraceL, remainingSegs) seg ->
        match remainingSegs with
        | prev::curr::next::tail -> 
            let isRetrace = curr.Length = 0 && (sign prev.Length <> sign next.Length)
            let newRetraceL = if isRetrace then retraceL @ [(prev,curr,next)] else retraceL
            (newRetraceL, [curr;next] @ tail)
        | _ -> (retraceL, [])
    )
    |> fst
    |> function
    | [] -> None
    | s -> Some s


/// <summary>
/// Retrieves the start and end segment position tuple of a wire if 
/// </summary>
/// <param name="wire">The wire the segments belong to.</param>
/// <param name="model">The wire model.</param>
/// <param name="retraceSegmentList">The list of segments which trigger a retrace.</param>
/// <returns>
/// An option tuple containing the start and end segments if they retrace and the new Start and End position pair for the retraced segment.
/// </returns>
let getWireEndsRetrace (wire : BusWireT.Wire) (model : BusWireT.Model) retraceSegmentList = 
    /// Return start end position pair of the coalesced end of wire segment
    /// if the segment is at the start of the wire, the start position will remain unchanged, and the end will be updated
    /// the opposite happens for a segment at the end of the wire
    let getNewPosPair segIndex posChange isStart = 
        let oldSeg = getASegmentFromId model (segIndex,wire.WId)

        if isStart 
        then (oldSeg.Start, addLengthToPos oldSeg.End oldSeg.Orientation posChange) 
        else (addLengthToPos oldSeg.Start oldSeg.Orientation posChange, oldSeg.End) 


    let startWire = 
        List.tryHead retraceSegmentList 
        |> Option.bind (fun (startSeg : Segment,_zero,next) ->
            if startSeg.Index = 0 
            then Some (startSeg, getNewPosPair startSeg.Index next.Length true) 
            else None
        )

    let endWire = 
        List.tryLast retraceSegmentList 
        |> Option.bind (fun (prev ,_zero,endSeg : Segment) ->
            if endSeg.Index = wire.Segments.Length - 1 
            then Some (endSeg, getNewPosPair endSeg.Index prev.Length false) 
            else None
        )
    
    (startWire, endWire)

/// <summary>
/// Checks if a given position is inside any symbol's bounding box in a sheet.
/// </summary>
/// <param name="sheet">The sheet containing symbols.</param>
/// <param name="pos">The position to check.</param>
/// <returns>True if the position is inside any symbol's bounding box, false otherwise.</returns>
let posInsideAnySymbol (sheet : SheetT.Model) pos = 
    allSymbolBBoxInSheet sheet
    |> List.map (fun (_cid, bbox) ->
        overlap2DBox bbox {TopLeft=pos;W=1;H=1} // check for intersection with box with 1d bbox
    ) |> List.reduce (||)

/// <summary>
/// Checks if a segment defined by its start and end positions intersects with any symbol's bounding box in a sheet.
/// </summary>
/// <param name="sheet">The sheet containing symbols.</param>
/// <param name="startPos">The start position of the segment.</param>
/// <param name="endPos">The end position of the segment.</param>
/// <returns>True if the segment intersects with any symbol's bounding box, false otherwise.</returns>
let segIntersectsAnySymbol (sheet : SheetT.Model) (startPos, endPos) = 
    allSymbolBBoxInSheet sheet
    |> List.map (fun (_cid, bbox) ->
        segmentIntersectsBoundingBox bbox startPos endPos
        |> function | None -> false | Some _ -> true
    ) |> List.reduce (||)

// T6R
/// <summary>
/// Retrieves the segments which retrace along with the retrace segments that belong to the ends of wires (start and end) which after retrace either start in a symbol or intersect with a symbol.
/// </summary>
/// <param name="sheet">The sheet containing wires.</param>
/// <returns>
/// A tuple containing lists of segments that retrace, start symbols inside segments, and intersecting symbols with segments.
/// </returns>
let getRetraceSegmentsT6R ( sheet : SheetT.Model ) =
    sheet.Wire.Wires
    |> Map.toList
    |> List.fold (fun (retL,startInSymL, intersectSymL) (_wId, wire) -> 

        let retracedSegments = getRetraceSegmentsOfWire wire

        let (startInsideSymbol, intersectSymbol) =
            retracedSegments
            |> Option.map (fun s -> 
                s
                |> getWireEndsRetrace wire sheet.Wire 
                |> (fun (startSeg, endSeg) ->
                    let (ssInsideSym, ssIntersectSym) = 
                        match startSeg with
                        | None -> ([],[])
                        | Some (sseg, (sPos, newPos)) -> 
                            ((if posInsideAnySymbol sheet newPos then [sseg] else [])
                            , (if segIntersectsAnySymbol sheet (sPos,newPos) then [sseg] else []))
                    
                    let (esInsideSym, esIntersectSym) = 
                        match endSeg with
                        | None -> ([],[])
                        | Some (eseg, (newPos, epos)) -> 
                            ((if posInsideAnySymbol sheet newPos then [eseg] else [])
                            , (if segIntersectsAnySymbol sheet (newPos,epos) then [eseg] else []))

                    (ssInsideSym @ esInsideSym, ssIntersectSym @ esIntersectSym)
                )
            )
            |> Option.defaultValue ([],[])

        (retL @ Option.defaultValue [] retracedSegments,startInSymL @ startInsideSymbol, intersectSymL @ intersectSymbol)
    ) ([],[],[])