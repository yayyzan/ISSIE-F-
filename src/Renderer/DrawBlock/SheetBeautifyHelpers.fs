module SheetBeautifyHelpers

//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team
open System
open CommonTypes
open DrawModelType
open DrawModelType.BusWireT
open DrawModelType.SymbolT
open Optics
open Optics.Operators
open Sheet
open SimulatorTypes
open Symbol
open BlockHelpers

type Dimensions = {
    Width: float
    Height: float
}

/// <summary>Returns the dimensions of a custom component, fails on non-custom components</summary>
/// <param name="symbol"> Symbol of custom Component </param>
/// <returns>Dimensions of custom component</returns>
let customComponentDimensions (symbol: SymbolT.Symbol) : Dimensions = // B1R
    symbol
    |> Optic.get component_
    |> (fun comp ->
            match comp.Type with
            | _ -> failwithf "What? Component is not custom"
        )


/// <summary>Writes the dimensions of a custom component</summary>
/// <param name="dim"> Dimension to set for custom component </param>
/// <param name="symbol"> Symbol of custom Component </param>
/// <returns>Returns the symbol with the updated dimensions</returns>
let setCustomComponentDimension (dim: Dimensions) (symbol: SymbolT.Symbol): SymbolT.Symbol = // B1W
    ((fun (comp: Component) -> {comp with H = dim.Height; W = dim.Width}), symbol)
    ||> Optic.map component_

let symbolDimension_ = Lens.create customComponentDimensions setCustomComponentDimension


/// <summary>Writes the new position of a symbol</summary>
/// <param name="symbol"> Symbol to modify </param>
/// <param name="newPos"> The new position of the symbol </param>
/// <returns> The symbol with the updated position </returns>
let setSymbolPosition (symbol: SymbolT.Symbol) (newPos: XYPos) = // B2W
    {symbol with Pos = newPos}


/// <summary>returns a symbol command to move symbol with compID to a new location</summary>
/// <param name="compId"> Component id of the Symbol to move </param>
/// <param name="newPos"> The new position of the symbol </param>
/// <returns> Message command to dispatch </returns>
/// NB: This is arguably better than previous version for two reasons
/// 1) Does not change the model outside the messages
/// 2) More complex, there does not seem to be a real need for B2W
let moveSymbolMessage (compId: ComponentId) (newPos: XYPos) = // B2W'
    symbolCmd (MoveSymbols ([compId], newPos))


let portSymbol_ = portMaps_ >-> order_

/// <summary> Reads the port Order of a specific edge</summary>
/// <param name="symbol"> Symbol Containing target ports </param>
/// <param name="edge"> The edge of the symbol to read </param>
/// <returns> The portId's of the edge in order </returns>
let portIdsOnEdge (symbol: SymbolT.Symbol) (edge: Edge) =
    (Optic.get portSymbol_ symbol)
    |> (Map.find edge)

/// <summary> Set the ports of an edge in a new order</summary>
/// <param name="edge"> The edge of the symbol to modify </param>
/// <param name="newOrder"> The new order of the ports </param>
/// <returns> The symbol with the updated portIds on the edge</returns>
let setPortIdsOnEdge (edge: Edge) (newOrder: string list) =
    Optic.map portSymbol_ (Map.add edge newOrder)

/// <summary> Reads the reverse state of a MUX2 symbol, fails otherwise</summary>
/// <param name="symbol"> The MUX2 symbol </param>
/// <returns> The reverse state of a mux 2 </returns>
let reverseStateMux2 (symbol: SymbolT.Symbol) : bool option =
    symbol
    |> Optic.get component_
    |> (fun comp -> match comp.Type with
                    | Mux2 -> symbol.ReversedInputPorts
                    | _ -> failwithf "What? Not a Mux 2")

/// <summary> Sets the state of a MUX2 symbol </summary>
/// <param name="newReversedState"> The new reversed state of MUX2 symbol </param>
/// <param name="symbol"> The MUX2 symbol being modified </param>
/// <returns> The updated MUX2 </returns>
let setMux2ReverseState (newReversedState: bool) (symbol: SymbolT.Symbol) : SymbolT.Symbol =
    {symbol with ReversedInputPorts = Some newReversedState}

// NB: Prism instead of lens because state is bool option
let reversedPrism = Prism.create reverseStateMux2 setMux2ReverseState

/// <summary> The Position of the port </summary>
/// <param name="port"> The target post </param>
/// <param name="symbol"> The symbol which has that port </param>
/// <returns> The absolute position of the port on the sheet </returns>
let absPortPos (port:Port) (symbol: Symbol) =
    symbol.Pos + getPortPos symbol port

/// Returns the bounding box of a symbol. It is defined by the height and the width as well as the x,y position of the symbol.
/// Works with rotation. For a rotated symbol, TopLeft = Pos, and H,W swapped in getRotatedHAndW
let getSymbolBoundingBox = getSymbolBoundingBox


// lens to obtain the sTransform of a symbol
let sTransform_ = Lens.create (fun (a:SymbolT.Symbol) -> a.STransform) (fun b a -> {a with STransform = b })

/// <summary> Read the rotation of a symbol </summary>
/// <param name="symbol"> The symbol to read the rotation from </param>
/// <returns> Rotation state of a symbol </returns>
let symbolRotation (symbol: SymbolT.Symbol) : Rotation =
    Optic.get sTransform_ symbol
    |> (fun transform -> transform.Rotation)


// TODO: newSTransform is defined locally somewhere else maybe make it global ??

/// <summary> Read the rotation of a symbol </summary>
/// <param name="symbol"> The symbol to read the rotation from </param>
/// <remarks>Note that the function will combine the new rotation with the previous transform</remarks>
/// <param name="rotation"> The symbol to read the rotation from </param>
/// <returns> Symbol with updated rotation state </returns>
let setSymbolRotation (rotation: Rotation) (symbol: SymbolT.Symbol) =
    let newSTransform =
        match symbol.STransform.Flipped with
        | true ->
            {symbol.STransform with Rotation = combineRotation (invertRotation rotation) symbol.STransform.Rotation}
        | false ->
            {symbol.STransform with Rotation = combineRotation rotation symbol.STransform.Rotation}

    Optic.set sTransform_ newSTransform symbol

let symbolRotation_ = Lens.create symbolRotation setSymbolRotation


/// <summary> Read the Flip of a symbol </summary>
/// <param name="symbol"> The symbol to read the from </param>
/// <returns> bool to indicate whether the symbol is flipped or not </returns>
let isSymbolFlipped (symbol: SymbolT.Symbol) : bool =
    Optic.get sTransform_ symbol
    |> (fun transform -> transform.Flipped)

/// <summary> Set the Flip of a symbol </summary>
/// <returns> Updated Symbol with new flip state </returns>
let setSymbolFlip (flip: bool) =
    Optic.map sTransform_  (fun transform -> {transform with Flipped = flip})

let flippedLens_ = Lens.create isSymbolFlipped setSymbolFlip


// ----------------------------------------------------------------------------//
// ------------------------    Test Functions    ------------------------------//
// ----------------------------------------------------------------------------//


(*
    In the following seciton, a visible wire is defined as a list of non-zero length segments where the 
    segments do not overlap. The list is not ordered, i.e it does not follow the (horizontal, vertical, horizantal) order issie wires do.
    This is not a problem since these functions are all reading from the sheet. i.e they do not ever write a model. These are/ will be used to 
    test the beutify functions.
*)


/// <summary> Gets the values of a map as a list </summary>
/// <remarks> This is used quite often, it is much better to define it rather than use a lambda everytime </remarks>
/// <param name="map"> Map of any type </param>
/// <returns> The Values of the map as a list </returns>
let mapValuesToList (map: Map<'a,'b> ) =
    Seq.toList map.Values


/// <summary> Returns the Lengt of an ASegment </summary>
/// <param name="aSegment"> The ASegment to get the length of </param>
/// <returns> The length of the ASegment </returns>
let getAbsSegmentLength (aSegment : ASegment) =
    let delta = aSegment.End - aSegment.Start
    let squared = delta.X * delta.X + delta.Y * delta.Y
    Math.Sqrt squared



let (|IsEven|IsOdd|) (n: int) = match n % 2 with | 0 -> IsEven | _ -> IsOdd

/// <summary> Return unique pairs of elements of a list </summary>
/// <remarks> Unique in the sense that (a,b) = (b,a). Hence the function will only return 1 of these </remarks>
/// <returns> A list of tuples </returns>
let rec distinctPairs lst =
    match lst with
    | [] -> []
    | h::t -> List.map (fun elem -> (h, elem)) t @ distinctPairs t

// Type of Overlap to check, this is used in the updated version of Overlap1D
type OverlapType = WeakOverlap | StrictOverlap

// Modified Overlap1D, allows to choose between overlap, or just in contact range.
// This is better for this library since it allows me to check for Junction intersections alone and exclude T
// Sections, or both. This also applies to checking if two ranges overlap or not.
let improvedOverlap1D ((a1, a2): float * float) ((b1, b2): float * float) (overlapType: OverlapType) : bool =
    let a_min, a_max = min a1 a2, max a1 a2
    let b_min, b_max = min b1 b2, max b1 b2

    match overlapType with
    | StrictOverlap -> a_max > b_min && b_max > a_min
    | WeakOverlap -> a_max >= b_min && b_max >= a_min


// As a consequence of adding to overlap1D, i modified the functions that i need in order to use the improved overlap
let improvedOverlap2D ((a1, a2): XYPos * XYPos) ((b1, b2): XYPos * XYPos) (overlapType: OverlapType) : bool =
    (improvedOverlap1D (a1.X, a2.X) (b1.X, b2.X) overlapType) && (improvedOverlap1D (a1.Y, a2.Y) (b1.Y, b2.Y) overlapType)

let getWiresInBox' (box: BoundingBox) (model: BusWireT.Model) : (Wire * int) list =
    let wires =
        model
        |> Optic.get wires_
        |> mapValuesToList

    let bottomRight =
        {   X = box.TopLeft.X + box.W
            Y = box.TopLeft.Y + box.H
        }

    // State Tuple - (overlapping: bool, overlapping_wire_index: int)
    let checkOverlapFolder (startPos: XYPos) (endPos: XYPos) (state: bool * int) (segment: Segment) : bool * int =
        let overlap = improvedOverlap2D (startPos, endPos) (box.TopLeft, bottomRight) StrictOverlap
        (fst state || overlap), if overlap then segment.Index else snd state

    List.map (fun w -> foldOverNonZeroSegs checkOverlapFolder (false, -1) w, w) wires
    |> List.filter (fun l -> fst (fst l))
    |> List.map (fun ((_, index), w) -> w, index)




/// <summary> Checks if two visible ASegments overlap </summary>
/// <param name="vertSeg"> The Vertical ASegment </param>
/// <param name="horSeg"> The Horizontal ASegment </param>
/// <param name="overlapType"> The type of Comparison, (strict or weak) overlap </param>
/// <returns> True if the visible Segments overlap </returns>
let visibleSegmentOverlap (vertSeg: ASegment) (horSeg: ASegment) (overlapType: OverlapType) : bool =
    let ({Start=vertStart; End=vertEnd} : ASegment) =  vertSeg
    let ({Start=horStart; End=horEnd} : ASegment) = horSeg
    improvedOverlap2D (vertStart, vertEnd) (horStart, horEnd) overlapType


/// <summary> Orders horizontal segments by the X position vertical segment by the Y position </summary>
/// <remarks>Note this is not changed in the model, this is done to make it easier to analyze visible segments </remarks>
/// <param name="aSegment"> ASegment to order </param>
/// <returns> Modified A segment with Start and End ordered correctly </returns>
let orderASegment (aSegment: ASegment) : ASegment =
    let delta = aSegment.Start - aSegment.End
    aSegment
    |> (fun visSeg ->
        match visSeg.Orientation, delta.X > 0, delta.Y < 0 with
        | Horizontal, true, _ | Vertical, _, true  -> {aSegment with Start = visSeg.End; End = visSeg.Start}
        | _ -> visSeg
    )

/// <summary> Groups and sorts visible segments by their start position based on the given orientation </summary>
/// <param name="visSegments"> The list of ASegments to be ordered </param>
/// <param name="ori"> The orientation (Horizontal or Vertical) to order by </param>
/// <returns> A list of lists of ASegments, grouped by start position and sorted within each group </returns>
let orderByOrientation (visSegments: ASegment list) (ori: Orientation) =
       visSegments
       |>   match ori with
            | Horizontal -> List.groupBy (fun visSeg -> visSeg.Start.Y)
            | Vertical -> List.groupBy (fun visSeg -> visSeg.Start.X)
       |> List.map (fun (_, segments) ->
           match ori with
           | Horizontal -> List.sortBy (fun visSeg -> visSeg.Start.X) segments
           | Vertical -> List.sortBy (fun visSeg -> visSeg.Start.Y) segments
       )


/// <summary> Removes overlapping segments from a list of segments </summary>
/// <remarks> This is done by folding over the list of segments, and checking if the current segment overlaps with the last segment </remarks>
/// <param name="aSegments"> The list of ASegments to remove overlaps from </param>
/// <returns> A list of ASegments with overlaps removed </returns>
let removeOverlaps (aSegments: ASegment list) : ASegment list =
    ([], aSegments)
    ||> List.fold (fun visSegments visSeg ->
        match visSegments with
        | [] -> [visSeg]
        | lst ->
            let lastSegment = List.last lst
            match visibleSegmentOverlap visSeg lastSegment WeakOverlap with
            | false -> visSegments @ [visSeg]
            | true -> visSegments[0..visSegments.Length-2] @ [{lastSegment with End = visSeg.End}]
    )

(*
The algorithm being used:
1) Partition By Orientation
2) Group Horizontal segments by Y, and sort on the start position
3) Group Vertical segments by X, and sort by start position
4)  Apply a removeOverlap fold, since the segments are grouped and sorted, we guarantee that any segments
    That do overlap are consecutive. Thus rather than checking if a segment overlaps with all over segments,
    we only check if the segment overlaps with the previous one.

    Note that the Algorithm runs on ASegments after the order is corrected.
*)
/// <summary> Returns all visible segments from a list of segments </summary>
/// <remarks> This is done by partitioning the segments by orientation, ordering the segments, and then removing overlaps </remarks>
/// <param name="aSegments"> The list of ASegments to get the visible segments from </param>
/// <returns> The visible segments from the list of segments </returns>
let getVisibleSegments (aSegments: ASegment list) =
    aSegments
    |> List.map orderASegment    
    |> List.partition (fun visSeg -> visSeg.Orientation = Horizontal)
    |> (fun (hor, vert) -> orderByOrientation hor Horizontal, orderByOrientation vert Vertical)
    |> (fun (hor, vert) -> List.collect removeOverlaps hor @ List.collect removeOverlaps vert)


/// <summary> Returns the non-zero length segments of a wire </summary>
/// <param name="wire"> The wire to get the segments from </param>
/// <remarks> This is done by filtering the segments of the wire by length </remarks>
/// <returns> The non-zero length segments of the wire </returns>
let getVisibleSegmentsOfWire (wire: Wire) =
    wire
    |> getNonZeroAbsSegments
    |> getVisibleSegments

/// <summary> Returns all visible segments in the wire </summary>
/// <remarks> This function calls getVisible segments twice, once to remove overlaps within the wire (in getVisibleSegmentsOfWire), and another to remove global overlaps </remarks>
/// <param name="model"> The model to get the segments from </param>
/// <returns> All visible segments in the model </returns>
let getAllVisibleSegments (model: SheetT.Model) =
    model
    |> Optic.get SheetT.wires_
    |> mapValuesToList
    |> List.collect getVisibleSegmentsOfWire
    |> getVisibleSegments


/// <summary> The number of pairs of symbols that intersect each other </summary>
/// <param name="sheet"> The Sheet Model </param>
/// <returns> The count over the whole sheet </returns>
let intersectingSymbolPairs (sheet: SheetT.Model) : int = // T1R
    let bbs =
        sheet
        |> Optic.get SheetT.symbols_
        |> mapValuesToList
        |> List.map getSymbolBoundingBox

    bbs
    |> distinctPairs
    |> Seq.filter ( fun (bb1, bb2) -> overlap2DBox bb1 bb2)
    |> Seq.length


/// <summary> Returns the wire nets of a BusWire model </summary>
/// <param name="model"> The BusWire model of the Sheet </param>
/// <returns> The wire nets of the model </returns>
let getWireNets (model: BusWireT.Model) : Wire list list =
    model
    |> Optic.get wires_
    |> mapValuesToList
    |> List.groupBy (fun wire -> wire.OutputPort)
    |> List.map snd


/// <summary>  The number of distinct wire visible segments that intersect with one or more symbols </summary>
/// <param name="model"> The BusWire model of the Sheet </param>
/// <returns> The count over the whole sheet </returns>
let countSegmentsIntersectingSymbols (model: SheetT.Model) = // T2R
    model
    |> Optic.get SheetT.symbols_
    |> mapValuesToList
    |> List.map getSymbolBoundingBox
    |> List.collect (fun bb -> getWiresInBox' bb model.Wire)
    |> List.length

/// <summary> The number of distinct pairs of visible segments that cross each other at right angles </summary>
/// <remarks>This does not include 0 segments, or segments on same net intersecting at one end, or segments on same net on top of each other. </remarks>
/// <param name="model"> The Sheet model to count over </param>
/// <returns> The count over the whole sheet </returns>
let countNumberOfJunctions (model: SheetT.Model)  = // T3R
    let isASegmentsInSameNet (seg1: ASegment) (seg2: ASegment) =
        let wires = Optic.get SheetT.wires_ model
        let wire1 = Map.find (snd seg1.GetId) wires
        let wire2 = Map.find (snd seg2.GetId) wires
        wire1.OutputPort = wire2.OutputPort

    let visibleSegmentIntersect (seg1: ASegment) (seg2: ASegment) (overlapType: OverlapType): bool =
       if seg1.Orientation = seg2.Orientation
       then false
       else match seg1.Orientation with
            | Vertical -> visibleSegmentOverlap seg1 seg2 overlapType
            | Horizontal -> visibleSegmentOverlap seg2 seg1 overlapType

    model
    |> getAllVisibleSegments
    |> distinctPairs
    |> List.map (fun (seg1, seg2) -> if isASegmentsInSameNet seg1 seg2 then (seg1, seg2, StrictOverlap) else (seg1, seg2, WeakOverlap))
    |> List.map (fun (seg1, seg2, overlap) -> visibleSegmentIntersect seg1 seg2 overlap)
    |> List.filter (fun b -> b = true)
    |> List.length



/// <summary> Sum of wiring segment length, counting only one when there are N same-net segments overlapping </summary>
/// <remarks>The number of distinct pairs of visible segments that cross each other at right angles </remarks>
/// <param name="model"> The Sheet model to count over </param>
/// <returns> The count over the whole sheet </returns>

let getVisibleWireLength (model : SheetT.Model) = // T4R
    let reduceWithDefault def f lst =
        match lst with
        | [] -> def
        | _ -> List.reduce f lst

    let getWireNetLength (wireNet : Wire list) : float =
        wireNet
        |> List.collect getNonZeroAbsSegments
        |> getVisibleSegments
        |> List.map getAbsSegmentLength
        |> reduceWithDefault 0.0 (+)

    model
    |> Optic.get SheetT.wire_
    |> getWireNets
    |> List.map getWireNetLength
    |> reduceWithDefault 0.0 (+)


/// <summary> Number of visible wire right-angles </summary>
/// <remarks>The number of distinct pairs of visible segments that cross each other at right angles </remarks>
/// <param name="model"> The BusWire model of the Sheet </param>
/// <returns> The count over the whole sheet </returns>
let countRightAngles (model: SheetT.Model) = // T5R
    let countRightAngles (count: int) (segments: ASegment list) : int =
        segments
        |> List.pairwise
        |> List.filter (fun (currSeg, nextSeg) -> currSeg.Orientation <> nextSeg.Orientation)
        |> (fun lst -> lst.Length + count)

    model
    |> Optic.get SheetT.wires_
    |> mapValuesToList
    |> List.map getNonZeroAbsSegments
    |> (fun segments ->
            (0, segments)
            ||> List.fold countRightAngles
        )


// A sliding window for going through segments. It contains the portId if the window is the first of last window of a wire
type Window = {
    Segments: ASegment list
    PortId: string option
}

// Window Maker
let makeWindows (wire: Wire) (visSegments: ASegment list list) : Window list =
    visSegments
    |> List.mapi (fun idx window ->
        match idx with
        | 0 -> {Segments = window; PortId = Some (getOutputPortIdStr wire.OutputPort); }
        | n when n = visSegments.Length - 1 -> {Segments = window; PortId = Some (getInputPortIdStr wire.InputPort)  }
        | _ -> {Segments = window; PortId = None }
    )


/// <summary> The zero-length segments in a wire with non-zero segments on either side that have Lengths of opposite signs lead to a wire retracing itself </summary>
/// <param name="sheet"> The Sheet model of the Sheet </param>
/// <returns> The count of retracing segments, A list of all retracing segments, All retracing segments that intersect symbols </returns>
let getRetracingSegments (sheet: SheetT.Model) =
    let segmentsIntersectsSymbol (segments: ASegment list) (portId: string)  : ASegment list =
        let sym =
            sheet
            |> Optic.get SheetT.symbol_
            |> (fun symModel -> getSymbol symModel portId)

        let bb = getSymbolBoundingBox sym

        let portType =
            sym
            |> Optic.get component_
            |> (fun comp -> comp.getPort (PortId portId))
            |> Option.map (fun port -> port.PortType)

        let segmentIntersectsSymbol =
                (false, segments)
                ||> List.fold (fun state seg -> state && Option.isSome (segmentIntersectsBoundingBox bb seg.Start seg.End))

        match portType with
        | Some PortType.Output when segmentIntersectsSymbol -> [segments[2]]
        | Some PortType.Input when segmentIntersectsSymbol -> [segments[segments.Length-3]]
        | _ -> []

    let wires =
        sheet
        |> Optic.get SheetT.wires_
        |> mapValuesToList

    let isSegmentsRetrace (window: Window) =
        match window.Segments with
        | a::b::c::_ -> b.IsZero && (sign a.Segment.Length <> sign c.Segment.Length)
        | _ -> false

    let getRetracingSegments' (wire: Wire) =
        let retracingSegmentsFold (foundSegments: ASegment list * ASegment list) (window: Window) =
            let allRetracingSegments, intersectingRetracingSegments = foundSegments
            match isSegmentsRetrace window with
            | true ->
                match window.PortId with
                | None -> allRetracingSegments @ [window.Segments[0]], intersectingRetracingSegments
                | Some pId-> allRetracingSegments @ [window.Segments[0]], segmentsIntersectsSymbol window.Segments pId
            | _ -> foundSegments

        wire
        |> getAbsSegments
        |> List.map orderASegment
        |> List.windowed 3
        |> makeWindows wire
        |> (fun windows ->
            (([], []), windows)
            ||> List.fold retracingSegmentsFold
        )
        ||> (fun retraceSeg intersectingRetraces -> retraceSeg.Length, retraceSeg, intersectingRetraces)


    List.map getRetracingSegments' wires
