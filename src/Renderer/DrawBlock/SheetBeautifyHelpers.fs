module SheetBeautifyHelpers

open EEExtensions
open Optics
open Optics.Operators
open Helpers
open CommonTypes
open DrawModelType
open BlockHelpers
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BusWireUpdateHelpers
open Optics.Optic
open Operators
open Symbol

/// <summary>Checks if two 1D segments overlap strictly.</summary>
/// <param name="a1">The first endpoint of the first segment.</param>
/// <param name="a2">The second endpoint of the first segment.</param>
/// <param name="b1">The first endpoint of the second segment.</param>
/// <param name="b2">The second endpoint of the second segment.</param>
/// <returns>True if the segments overlap strictly, false otherwise.</returns>
let strictOverlap1D ((a1, a2): float * float) ((b1, b2): float * float) : bool =
    let a_min, a_max = min a1 a2, max a1 a2
    let b_min, b_max = min b1 b2, max b1 b2
    a_max > b_min && b_max > a_min

/// <summary>Checks if two 2D segments overlap strictly.</summary>
/// <param name="a1">The first endpoint of the first segment.</param>
/// <param name="a2">The second endpoint of the first segment.</param>
/// <param name="b1">The first endpoint of the second segment.</param>
/// <param name="b2">The second endpoint of the second segment.</param>
/// <returns>True if the segments overlap strictly, false otherwise.</returns>
let strictOverlap2D ((a1, a2): XYPos * XYPos) ((b1, b2): XYPos * XYPos) : bool =
    (strictOverlap1D (a1.X, a2.X) (b1.X, b2.X))
    && (strictOverlap1D (a1.Y, a2.Y) (b1.Y, b2.Y))

/// <summary>Checks if two segments overlap with a custom operator.</summary>
/// <param name="overlapper">The function to check if two 1D segments overlap.</param>
/// <param name="operator">The function to check if the orientations of the segments are compatible.</param>
/// <param name="seg1">The first segment.</param>
/// <param name="seg2">The second segment.</param>
/// <returns>True if the segments overlap with the custom operator, false otherwise.</returns>
let segmentsOverlapWithCustomOperator overlapper operator (seg1: ASegment) (seg2: ASegment) : bool =
    overlapper (seg1.Start.X, seg1.End.X) (seg2.Start.X, seg2.End.X)
    && overlapper (seg1.Start.Y, seg1.End.Y) (seg2.Start.Y, seg2.End.Y)
    && operator seg1.Orientation seg2.Orientation

/// <summary>Gets the distinct pairs of elements from two lists.</summary>
/// <param name="list1">The first list.</param>
/// <param name="list2">The second list.</param>
/// <returns>The distinct pairs of elements from the two lists.</returns>
let getDistinctPairs (list1: 'a list) (list2: 'a list) : ('a * 'a) list =
    (list1, list2)
    ||> List.allPairs
    |> List.map (fun (a, b) -> if a < b then (a, b) else (b, a))
    |> List.distinct

/// <summary>Calculates the length of a segment.</summary>
/// <param name="seg">The segment to calculate the length of.</param>
/// <returns>The length of the segment.</returns>
let calculateSegmentLength (seg: ASegment) : float =
    abs (seg.Start.X - seg.End.X)
    + abs (seg.Start.Y - seg.End.Y)

/// <summary>Merges two segments.</summary>
/// <param name="seg1">The first segment.</param>
/// <param name="seg2">The second segment.</param>
/// <returns>The merged segment.</returns>
let mergeSegments (seg1: ASegment) (seg2: ASegment) : ASegment =
    let minX = min (min seg1.Start.X seg1.End.X) (min seg2.Start.X seg2.End.X)
    let minY = min (min seg1.Start.Y seg1.End.Y) (min seg2.Start.Y seg2.End.Y)
    let maxX = max (max seg1.Start.X seg1.End.X) (max seg2.Start.X seg2.End.X)
    let maxY = max (max seg1.Start.Y seg1.End.Y) (max seg2.Start.Y seg2.End.Y)

    match seg1.Orientation with
    | Horizontal ->
        { seg1 with
            Start = { X = minX; Y = seg1.Start.Y }
            End = { X = maxX; Y = seg1.End.Y } }
    | Vertical ->
        { seg1 with
            Start = { X = seg1.Start.X; Y = minY }
            End = { X = seg1.End.X; Y = maxY } }

/// <summary>Folds a list of segments into a list of merged segments.</summary>
/// <param name="segments">The list of segments to fold.</param>
/// <returns>The folded list of merged segments.</returns>
let foldSegments (segments: ASegment list) : ASegment list =
    ([], segments)
    ||> List.fold (fun acc current ->
        let matching, nonMatching =
            List.partition (fun seg -> segmentsOverlapWithCustomOperator overlap1D (=) seg current) acc
        match matching with
        | seg :: _ -> (mergeSegments seg current) :: nonMatching
        | [] -> current :: acc)

/// <summary>Gets the visible absolute segments of a list of wires.</summary>
/// <param name="wire">The wire to get the visible absolute segments of.</param>
/// <returns>The visible absolute segments of the wires combined.</returns>
let getVisibleAbsoluteSegments (wire: Wire list) : ASegment list =
    wire
    |> List.collect getNonZeroAbsSegments
    |> foldSegments

/// <summary>Gets the visible absolute segments of a single wire.</summary>
/// <param name="wire">The wire to get the visible absolute segments of.</param>
/// <returns>The visible absolute segments of the wire.</returns>
let getVisibleAbsoluteSegmentsOneWire (wire: Wire) : ASegment list =
    wire |> getNonZeroAbsSegments |> foldSegments

/// <summary>Checks if two wires intersect.</summary>
/// <param name="wire1">The first wire to check for intersections.</param>
/// <param name="wire2">The second wire to check for intersections.</param>
/// <returns>The number of intersections between the two wires.</returns>
let checkIntersection (wire1: int * ASegment list, wire2: int * ASegment list) : int =
    let segmentsOverlap (seg1, seg2) =
        segmentsOverlapWithCustomOperator strictOverlap1D (<>) seg1 seg2

    let segmentsDistinct (seg1, seg2) =
        match (fst wire1), (fst wire2) with
        | int1, int2 when int1 = int2 ->
            seg1.Start <> seg2.End
            && seg1.End <> seg2.Start
            && seg1.Start <> seg2.Start
            && seg1.End <> seg2.End
        | _ -> true

    let getOverlapCount segList1 segList2 =
        List.allPairs (snd segList1) (snd segList2)
        |> List.map (fun (a, b) -> if a < b then (a, b) else (b, a))
        |> List.distinct
        |> List.filter segmentsDistinct
        |> List.map segmentsOverlap
        |> List.fold (fun acc x -> if x then 1 + acc else acc) 0

    getOverlapCount wire1 wire2

/// <summary>Gets the wires that strictly overlap with a box.</summary>
/// <param name="box">The box to check for strict overlap with the wires.</param>
/// <param name="model">The model to get the wires from.</param>
/// <returns>The wires that strictly overlap with the box.</returns>
let getWiresInBoxStrictOverlap (box: BoundingBox) (model: Model) : (Wire * int) list =
    let wires = (List.ofSeq (Seq.cast model.Wires.Values))

    let bottomRight = { X = box.TopLeft.X + box.W; Y = box.TopLeft.Y + box.H }

    let checkOverlapFolder (startPos: XYPos) (endPos: XYPos) (state: bool * int) (segment: Segment) : bool * int =
        let overlap = strictOverlap2D (startPos, endPos) (box.TopLeft, bottomRight)
        (fst state || overlap),
        if overlap then
            segment.Index
        else
            snd state

    List.map (fun w -> foldOverNonZeroSegs checkOverlapFolder (false, -1) w, w) wires
    |> List.filter (fun l -> fst (fst l))
    |> List.map (fun ((_, index), w) -> w, index)

/// <summary>Calculates the total length of a list of segments.</summary>
/// <param name="segments">The list of segments to calculate the total length of.</param>
/// <returns>The total length of the list of segments.</returns>
let totalSegmentLength (segments: ASegment list) : float =
    (0., segments)
    ||> List.fold (fun acc seg -> acc + calculateSegmentLength seg)

/// <summary>Calculates the total number of intersections between a list of segments.</summary>
/// <param name="list">The list of segments to calculate the total number of intersections between.</param>
/// <returns>The total number of intersections between the list of segments.</returns>
let calculateTotalIntersections list =
    list |> List.map checkIntersection |> List.sum

/// <summary>Checks if a point is inside a box.</summary>
/// <param name="segStart">The point to check if it is inside the box.</param>
/// <param name="box">The box to check if the point is inside.</param>
/// <returns>True if the point is inside the box, false otherwise.</returns>
let isInsideBox (segStart: XYPos) (box: BoundingBox) : bool =
    let br = box.BottomRight()
    segStart.X >= box.TopLeft.X
    && segStart.X <= br.X
    && segStart.Y <= box.TopLeft.Y
    && segStart.Y >= br.Y

/// <summary>Checks if a segment is inside a symbol.</summary>
/// <param name="seg">The segment to check if it is inside the symbol.</param>
/// <param name="boundingBoxes">The bounding boxes of the symbol to check if the point is inside.</param>
/// <returns>True if the segment is inside the symbol, false otherwise.</returns>
let insideSymbol (seg: ASegment) (boundingBoxes: BoundingBox list) : bool =
    boundingBoxes
    |> List.exists (isInsideBox seg.Start)

/// <summary>Finds the retracing segments of a wire.</summary>
/// <param name="wire">The wire to find the retracing segments of.</param>
/// <returns>The retracing segments of the wire.</returns>
let findRetracingSegments (wire: Wire) : Segment list =
    let isRetracing seg index =
        index > 0
        && index < wire.Segments.Length - 1
        && seg.Length = 0
        && (wire.Segments.[index - 1].Length
            * wire.Segments.[index + 1].Length < 0)

    wire.Segments
    |> List.mapi (fun index seg -> (seg, index))
    |> List.filter (fun (seg, index) -> isRetracing seg index)
    |> List.collect (fun (seg, index) -> [ wire.Segments.[index - 1]; seg; wire.Segments.[index + 1] ])

/// <summary>Finds the retracing end segments of a wire.</summary>
/// <param name="sheet">The sheet to find the retracing end segments of.</param>
/// <param name="wire">The wire to find the retracing end segments of.</param>
/// <returns>The retracing end segments of the wire.</returns>
let findRetracingEndSegments (sheet: SheetT.Model) (wire: Wire) : Segment list =
    let boundingBoxes = sheet.BoundingBoxes |> mapValues |> Array.toList
    let absoluteSegments = getAbsSegments wire

    let isRetracingEnd seg index =
        (index = 2 || index = wire.Segments.Length - 4)
        && seg.Segment.Length = 0
        && (wire.Segments.[index + 1].Length
            * wire.Segments.[index - 1].Length < 0)
        && (insideSymbol absoluteSegments.[index + 1] boundingBoxes)

    absoluteSegments
    |> List.mapi (fun index seg -> (seg, index))
    |> List.filter (fun (seg, index) -> isRetracingEnd seg index)
    |> List.collect (fun (seg, index) -> [ wire.Segments.[index - 1]; seg.Segment; wire.Segments.[index + 1] ])

let symbolOrder_ = portMaps_ >-> order_

/// B1R
/// <summary>Gets the height and width of a symbol.</summary>
/// <param name="sym">The symbol to get the height and width.</param>
/// <returns>The height and width of the symbol.</returns>
let getSymbolDim (sym: Symbol) : float * float = getRotatedHAndW sym

/// B1W
/// <summary>Updates the height and width of a symbol.</summary>
/// <param name="sym">The symbol to update the height and width.</param>
/// <param name="newDimensions">The new height and width of the symbol.</param>
/// <returns>The symbol with the updated height and width.</returns>
let setSymbolDim (sym: Symbol) (newDimensions: float * float) : Symbol =
    setCustomCompHW (fst newDimensions) (snd newDimensions) sym

/// B2W
/// <summary>Updates the position of a symbol.</summary>
/// <param name="sym">The symbol to update the position.</param>
/// <param name="newPosition">The new position of the symbol.</param>
/// <returns>The symbol with the updated position.</returns>
let setSymbolPos (sym: Symbol) (newPosition: XYPos) : Symbol = set posOfSym_ newPosition sym

/// B3R
/// <summary>Gets the order of the ports of a symbol.</summary>
/// <param name="sym">The symbol to get the order of the ports.</param>
/// <param name="edge">The edge to get the order of the ports from.</param>
/// <returns>The order of the ports of the symbol.</returns>
let getPortOrder (sym: Symbol) (edge: Edge) : string list = get symbolOrder_ sym |> Map.find edge

/// B3W
/// <summary>Updates the order of the ports of a symbol.</summary>
/// <param name="sym">The symbol to update the order of the ports.</param>
/// <param name="edge">The edge to update the order of the ports from.</param>
/// <param name="newOrder">The new order of the ports of the symbol.</param>
/// <returns>The symbol with the updated order of the ports.</returns>
let setPortOrder (sym: Symbol) (edge: Edge) (newOrder: string list) : Symbol =
    map symbolOrder_ (Map.add edge newOrder) sym

/// B4R
/// <summary>Gets the reversed input ports of a MUX component.</summary>
/// <param name="sym">The symbol to get the reversed input ports.</param>
/// <returns>The reversed input ports of the component.</returns>
let getReversedInputPorts (sym: Symbol) : bool option = sym.ReversedInputPorts

/// B4W
/// <summary>Updates the reversed input ports of a symbol.</summary>
/// <param name="sym">The symbol to update the reversed input ports.</param>
/// <returns>The symbol with the updated reversed input ports.</returns>
let setReversedInputPorts (sym: Symbol) (bool: bool) : Symbol =
    match getReversedInputPorts sym with
    | Some b -> { sym with ReversedInputPorts = Some bool }
    | None -> sym

/// B5R
/// <summary>Gets the position of a port of a symbol.</summary>
/// <param name="sym">The symbol to get the position of the port.</param>
/// <param name="port">The port to get the position from.</param>
/// <returns>The position of the port of the symbol.</returns>
let getPortPosition (sym: Symbol) (port: Port) : XYPos =
    let topLeftOffset = getPortPos sym port
    sym.Pos + topLeftOffset

/// B6R
/// <summary>Gets the bounding box of a symbol.</summary>
/// <param name="sym">The symbol to get the bounding box.</param>
/// <returns>The bounding box of the symbol.</returns>
let getSymbolBB (sym: Symbol) : BoundingBox = getSymbolBoundingBox sym

/// B7R
/// <summary>Gets the rotation of a symbol.</summary>
/// <param name="sym">The symbol to get the rotation.</param>
/// <returns>The rotation of the symbol.</returns>
let getSymbolRotation (sym: Symbol) : Rotation = sym.STransform.Rotation

/// B7W
/// <summary>Updates the rotation of a symbol.</summary>
/// <param name="sym">The symbol to update the rotation.</param>
/// <param name="newRotation">The new rotation of the symbol.</param>
/// <returns>The symbol with the updated rotation.</returns>
let setSymbolRotation (sym: Symbol) (newRotation: Rotation) : Symbol =
    let newTransform = { sym.STransform with Rotation = newRotation }
    { sym with STransform = newTransform }

/// B8R
/// <summary>Gets the flip type of a symbol.</summary>
/// <param name="sym">The symbol to get the flip type.</param>
/// <returns>The flip type of the symbol.</returns>
let getSymbolFlip (sym: Symbol) : bool = sym.STransform.Flipped

/// B8W
/// <summary>Updates the flip type of a symbol.</summary>
/// <param name="sym">The symbol to update the flip type.</param>
/// <returns>The symbol with the updated flip type.</returns>
let setSymbolFlip (sym: Symbol) (newFlip: bool) : Symbol =
    let newTransform = { sym.STransform with Flipped = newFlip }
    { sym with STransform = newTransform }

/// T1R
/// <summary>Gets the number of overlapping bounding boxes in a sheet.</summary>
/// <param name="sheet">The sheet to get the number of overlapping bounding boxes from.</param>
/// <returns>The number of overlapping bounding boxes in the sheet.</returns>
let countOverlappingBoundingBoxes (sheet: SheetT.Model) : int =
    let boxes =
        mapValues sheet.BoundingBoxes
        |> Array.toList
        |> List.mapi (fun n box -> n, box)
    getDistinctPairs boxes boxes
    |> List.filter (fun ((n1, box1), (n2, box2)) -> n1 <> n2 && overlap2DBox box1 box2)
    |> List.length

/// T2R
/// <summary>Gets the number of intersections between visible wires and bounding boxes.</summary>
/// <param name="sheet">The sheet to get the number of intersections from.</param>
/// <returns>The number of intersections between visible wires and bounding boxes.</returns>
let countWireBoundingBoxOverlaps (sheet: SheetT.Model) : int =
    let boundingBoxes = sheet.BoundingBoxes |> mapValues |> Array.toList
    List.collect (fun x -> getWiresInBoxStrictOverlap x sheet.Wire) boundingBoxes
    |> List.length

/// T3R
/// <summary>Gets the number of intersections between visible wires.</summary>
/// <param name="sheet">The sheet to get the number of intersections from.</param>
/// <returns>The number of intersections between visible wires.</returns>
let countVisibleSegmentIntersections (sheet: SheetT.Model) : int =
    let aSegments =
        groupWiresByNet sheet.Wire.Wires
        |> List.mapi (fun i x -> i, getVisibleAbsoluteSegments x)
    (aSegments, aSegments)
    ||> getDistinctPairs
    |> calculateTotalIntersections

/// T4R
/// <summary>Gets the total length of visible wires.</summary>
/// <param name="sheet">The sheet to get the total length of visible wires from.</param>
/// <returns>The total length of visible wires.</returns>
let getTotalVisibleSegmentLength (sheet: SheetT.Model) : float =
    getWireList sheet.Wire
    |> getVisibleAbsoluteSegments
    |> totalSegmentLength

/// T5R
/// <summary>Gets the total number of 90 degree angles from wires.</summary>
/// <param name="sheet">The sheet to get the total number of 90 degree angles from.</param>
/// <returns>The number of 90 degree angles.</returns>
let countRightAnglesFromWires (sheet: SheetT.Model) : int =
    getWireList sheet.Wire
    |> List.map (getVisibleAbsoluteSegmentsOneWire >> List.length)
    |> List.map (fun x -> x - 1)
    |> List.sum

/// T6R
/// <summary>Gets retracing segments and retracing end segments of a sheet.</summary>
/// <param name="sheet">The sheet to get the retracing segments and retracing end segments from.</param>
/// <returns>The retracing segments and retracing end segments of the sheet.</returns>
let getRetracingSegments (sheet: SheetT.Model) : Segment list * Segment list =
    let mapAndCollect f =
        sheet.Wire.Wires
        |> mapValues
        |> Array.toList
        |> List.collect f

    let retracingSegments = mapAndCollect findRetracingSegments
    let retracingEndSegments = mapAndCollect (findRetracingEndSegments sheet)

    retracingSegments, retracingEndSegments

