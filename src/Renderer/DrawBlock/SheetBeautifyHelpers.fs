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

let hScale_ =
    Lens.create (fun s -> s.HScale) (fun hScale s -> { s with HScale = hScale })
let vScale_ =
    Lens.create (fun s -> s.VScale) (fun vScale s -> { s with VScale = vScale })
let componentW_ =
    Lens.create (fun s -> s.Component.W) (fun w s -> { s with Component = { s.Component with W = w } })
let componentH_ =
    Lens.create (fun s -> s.Component.H) (fun h s -> { s with Component = { s.Component with H = h } })

// B1R
/// <summary>
/// Retrieves the current scaled dimensions of a symbol.
/// </summary>
/// <param name="symbol">The symbol whose dimensions are being queried.</param>
/// <returns>A tuple containing the scaled width and height of the symbol.</returns>
/// <remarks>
/// This function considers the symbol's scaling factors (if any) to return the effective dimensions.
/// </remarks>
let readSymbolDimensions (symbol: Symbol) : (float * float) =
    let width = Optic.get componentW_ symbol
    let height = Optic.get componentH_ symbol
    let scaledWidth = width * Option.defaultValue 1.0 symbol.HScale
    let scaledHeight = height * Option.defaultValue 1.0 symbol.VScale
    (scaledWidth, scaledHeight)

// B1W
/// <summary>
/// Adjusts the symbol's dimensions by setting new scaling factors based on the desired dimensions.
/// </summary>
/// <param name="symbol">The symbol to adjust.</param>
/// <param name="desiredWidth">The target width for the symbol.</param>
/// <param name="desiredHeight">The target height for the symbol.</param>
/// <returns>A new symbol instance with updated scaling factors to achieve the desired dimensions.</returns>
/// <remarks>
/// This function calculates new horizontal and vertical scaling factors based on the current dimensions of the symbol and the desired dimensions. The original proportions of the symbol are preserved.
/// </remarks>
let writeSymbolDimensions (symbol: Symbol) (desiredWidth: float) (desiredHeight: float) : Symbol =
    let currentWidth = Optic.get componentW_ symbol
    let currentHeight = Optic.get componentH_ symbol
    let newHScale = desiredWidth / currentWidth
    let newVScale = desiredHeight / currentHeight
    let symbolWithNewHScale = Optic.set hScale_ (Some newHScale) symbol
    Optic.set vScale_ (Some newVScale) symbolWithNewHScale

// B2W
/// <summary>
/// Updates the position of a specified symbol within the sheet model.
/// </summary>
/// <param name="newPos">The new position for the symbol.</param>
/// <param name="givenSymbol">The symbol to update.</param>
/// <param name="sheet">The current sheet model containing the symbol.</param>
/// <returns>A new sheet model with the symbol's position updated.</returns>
/// <remarks>
/// If the given symbol is not found within the sheet, the original sheet model is returned unchanged.
/// </remarks>
let updateSymbolPosition (newPos: XYPos) (givenSymbol: SymbolT.Symbol) (sheet: SheetT.Model) =
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

let portMapsOrder_ =
    Lens.create (fun (symbol: SymbolT.Symbol) -> symbol.PortMaps.Order) (fun newOrder symbol ->
        { symbol with PortMaps = { symbol.PortMaps with Order = newOrder } })

// B3R
/// <summary>
/// Retrieves the current order of ports on a specified edge of a symbol.
/// </summary>
/// <param name="edge">The edge (Top, Bottom, Left, Right) for which to retrieve the port order.</param>
/// <param name="symbol">The symbol whose port order is being queried.</param>
/// <returns>
/// An option containing a list of port IDs in their current order on the specified edge if present; otherwise, None.
/// </returns>
let readPortsOrder (edge: Edge) (symbol: SymbolT.Symbol) : string list option =
    let currentOrderMap = Optic.get portMapsOrder_ symbol
    let currentOrder = Map.tryFind edge currentOrderMap
    currentOrder

// B3W
/// <summary>
/// Updates the order of ports on a specified edge of a symbol.
/// </summary>
/// <param name="edge">The edge (Top, Bottom, Left, Right) where the ports order is to be updated.</param>
/// <param name="newOrder">A list of port IDs representing the new order of ports on the specified edge.</param>
/// <param name="symbol">The symbol whose port order is being updated.</param>
/// <returns>
/// A new symbol instance with the updated port order on the specified edge.
/// </returns>
let writePortsOrder (edge: Edge) (newOrder: string list) (symbol: SymbolT.Symbol) : SymbolT.Symbol =
    let currentOrderMap = Optic.get portMapsOrder_ symbol
    let updatedOrderMap = Map.add edge newOrder currentOrderMap
    let updatedSymbol = Optic.set portMapsOrder_ updatedOrderMap symbol
    updatedSymbol

let reversedInputPorts_ =
    Lens.create (fun (symbol: SymbolT.Symbol) -> symbol.ReversedInputPorts) (fun reversed symbol ->
        { symbol with ReversedInputPorts = reversed })

// B4R
/// <summary>
/// Retrieves the current status indicating whether the input ports of a symbol are reversed.
/// </summary>
/// <param name="symbol">The symbol to check for reversed input ports.</param>
/// <returns>
/// A boolean option indicating if the input ports are reversed; None if the symbol does not specify this property.
/// </returns>
let readReversedInputPorts (symbol: SymbolT.Symbol) : bool option = Optic.get reversedInputPorts_ symbol

// B4W
/// <summary>
/// Updates the reversal status of input ports for a given symbol.
/// </summary>
/// <param name="reversed">The new reversal status. True to reverse the input ports, False otherwise.</param>
/// <param name="symbol">The symbol to update with the new input port reversal status.</param>
/// <returns>
/// A modified symbol instance reflecting the updated input port reversal status.
/// </returns>
let writeReversedInputPorts (reversed: bool) (symbol: SymbolT.Symbol) : SymbolT.Symbol =
    Optic.set reversedInputPorts_ (Some reversed) symbol

// B5R
/// <summary>
/// Calculates the absolute position of a port on the sheet, taking into account the position of the symbol.
/// </summary>
/// <param name="symbol">The symbol containing the port.</param>
/// <param name="port">The port whose position is to be calculated.</param>
/// <returns>
/// The absolute XY position of the port on the sheet.
/// </returns>
let readPortPositionOnSheet (symbol: SymbolT.Symbol) (port: Port) : XYPos =
    let relativePortPos = getPortPos symbol port
    let symbolPos = symbol.Pos
    { X = symbolPos.X + relativePortPos.X; Y = symbolPos.Y + relativePortPos.Y }

// B6R
/// <summary>
/// Calculates the bounding box of a symbol based on its dimensions and position.
/// </summary>
/// <param name="symbol">The symbol to calculate the bounding box for.</param>
/// <returns>
/// A BoundingBox record representing the top left corner, width, and height of the symbol.
/// </returns>
let calculateBoundingBox (symbol: SymbolT.Symbol) : BoundingBox =
    let (width, height) = readSymbolDimensions symbol
    let topleft = symbol.Pos
    { TopLeft = topleft; W = width; H = height }


let sTransformRotation_ =
    Lens.create (fun (symbol: SymbolT.Symbol) -> symbol.STransform.Rotation) (fun newRotation sTransform ->
        { sTransform with
            STransform = { sTransform.STransform with Rotation = newRotation } })

// B7R
/// <summary>
/// Retrieves the current rotation state of a symbol.
/// </summary>
/// <param name="symbol">The symbol whose rotation state is being queried.</param>
/// <returns>
/// The Rotation value indicating the symbol's current rotation.
/// </returns>
let readRotationState (symbol: SymbolT.Symbol) : Rotation = Optic.get sTransformRotation_ symbol

// B7W
/// <summary>
/// Sets the rotation state of a symbol to a desired rotation.
/// </summary>
/// <param name="desiredRotation">The desired rotation state to set for the symbol.</param>
/// <param name="symbol">The symbol to update with the desired rotation state.</param>
/// <returns>
/// A new symbol instance with the updated rotation state.
/// </returns>
let writeRotationState (desiredRotation: Rotation) (symbol: SymbolT.Symbol) : SymbolT.Symbol =
    Optic.set sTransformRotation_ desiredRotation symbol

let sTransformFlipped_ =
    Lens.create (fun (symbol: SymbolT.Symbol) -> symbol.STransform.Flipped) (fun newFlipped sTransform ->
        { sTransform with
            STransform = { sTransform.STransform with Flipped = newFlipped } })

// B8R
/// <summary>
/// Reads the flip state of a symbol.
/// </summary>
/// <param name="symbol">The symbol from which to read the flip state.</param>
/// <returns>
/// A boolean indicating if the symbol is flipped.
/// </returns>
let readFlipState (symbol: Symbol) : bool = Optic.get sTransformFlipped_ symbol

// B8W
/// <summary>
/// Sets the flip state of a symbol.
/// </summary>
/// <param name="flip">Boolean indicating the desired flip state.</param>
/// <param name="symbol">The symbol to modify.</param>
/// <returns>
/// The symbol with its flip state updated.
/// </returns>
let writeFlipState (flip: bool) (symbol: SymbolT.Symbol) : SymbolT.Symbol =
    Optic.set sTransformFlipped_ flip symbol

/// <summary>
/// Determines if two bounding boxes intersect.
/// </summary>
/// <param name="box1">The first bounding box.</param>
/// <param name="box2">The second bounding box.</param>
/// <returns>
/// True if the bounding boxes intersect; otherwise, false.
/// </returns>
let doBoundingBoxesIntersect (box1: BoundingBox) (box2: BoundingBox) : bool =
    let box1Right = box1.TopLeft.X + box1.W
    let box1Bottom = box1.TopLeft.Y + box1.H
    let box2Right = box2.TopLeft.X + box2.W
    let box2Bottom = box2.TopLeft.Y + box2.H

    not (
        box1Right < box2.TopLeft.X
        || box2Right < box1.TopLeft.X
        || box1Bottom < box2.TopLeft.Y
        || box2Bottom < box1.TopLeft.Y
    )

// T1R
/// <summary>
/// Counts the number of symbol pairs whose bounding boxes intersect on a sheet.
/// </summary>
/// <param name="sheet">The sheet model containing the symbols.</param>
/// <returns>The number of intersecting symbol pairs.</returns>
let countIntersectingSymbolPairs (sheet: SheetT.Model) =
    let symbols =
        sheet.Wire.Symbol.Symbols
        |> Map.toList
        |> List.map snd

    let pairs =
        symbols
        |> List.mapi (fun i sym1 ->
            symbols
            |> List.skip (i + 1)
            |> List.map (fun sym2 -> (sym1, sym2)))
        |> List.concat

    let intersectingPairs =
        pairs
        |> List.filter (fun (s1, s2) -> doBoundingBoxesIntersect (calculateBoundingBox s1) (calculateBoundingBox s2))

    intersectingPairs |> List.length

let visibleSegments (wId: ConnectionId) (model: SheetT.Model) : XYPos list =
    let wire = model.Wire.Wires[wId] // get wire from model
    /// helper to match even and off integers in patterns (active pattern)
    let (|IsEven|IsOdd|) (n: int) =
        match n % 2 with
        | 0 -> IsEven
        | _ -> IsOdd
    /// Convert seg into its XY Vector (from start to end of segment).
    /// index must be the index of seg in its containing wire.
    let getSegmentVector (index: int) (seg: BusWireT.Segment) =
        // The implicit horizontal or vertical direction  of a segment is determined by
        // its index in the list of wire segments and the wire initial direction
        match index, wire.InitialOrientation with
        | IsEven, BusWireT.Vertical
        | IsOdd, BusWireT.Horizontal -> { X = 0.; Y = seg.Length }
        | IsEven, BusWireT.Horizontal
        | IsOdd, BusWireT.Vertical -> { X = seg.Length; Y = 0. }
    /// Return a list of segment vectors with 3 vectors coalesced into one visible equivalent
    /// if this is possible, otherwise return segVecs unchanged.
    /// Index must be in range 1..segVecs
    let tryCoalesceAboutIndex (segVecs: XYPos list) (index: int) =
        if segVecs[index] =~ XYPos.zero then
            segVecs[0 .. index - 2]
            @ [ segVecs[index - 1] + segVecs[index + 1] ]
            @ segVecs[index + 2 .. segVecs.Length - 1]
        else
            segVecs
    wire.Segments
    |> List.mapi getSegmentVector
    |> (fun segVecs ->
        (segVecs, [ 1 .. segVecs.Length - 2 ])
        ||> List.fold tryCoalesceAboutIndex)

// T2R
/// <summary>
/// Counts the number of distinct wire visible segments that intersect with any symbol on the sheet.
/// </summary>
/// <param name="model">The model of the sheet containing wires and symbols.</param>
/// <returns>The count of distinct wire segments intersecting any symbol.</returns>
let countDistinctWireSegmentsIntersectingSymbols (model: SheetT.Model) : int =
    let symbols =
        Map.toList model.Wire.Symbol.Symbols
        |> List.map snd
    let symbolBoxes = symbols |> List.map calculateBoundingBox
    model.Wire.Wires
    |> Map.toList
    |> List.collect (fun (wId, wire) ->
        let visibleSegVectors = visibleSegments wId model
        let initialPos = wire.StartPos
        [ visibleSegVectors
          |> List.fold
              (fun ((lastPos: XYPos), acc) vec ->
                  let newPos = { X = lastPos.X + vec.X; Y = lastPos.Y + vec.Y }
                  let segBox = { TopLeft = lastPos; W = abs vec.X; H = abs vec.Y }
                  let intersects = symbolBoxes |> List.exists (overlap2DBox segBox)
                  if intersects then
                      (newPos, acc + 1)
                  else
                      (newPos, acc))
              (initialPos, 0)
          |> snd ])
    |> List.sum

/// <summary>
/// Determines whether two wire segments intersect at a right angle.
/// </summary>
/// <param name="seg1">The first wire segment.</param>
/// <param name="seg2">The second wire segment.</param>
/// <returns>True if the segments intersect at a right angle, false otherwise.</returns>
/// <remarks>
/// Assumes that segments are either horizontal or vertical.
/// </remarks>
let doIntersectAtRightAngle (seg1: BusWireT.ASegment) (seg2: BusWireT.ASegment) =
    match seg1.Orientation, seg2.Orientation with
    | BusWireT.Orientation.Horizontal, BusWireT.Orientation.Vertical
    | BusWireT.Orientation.Vertical, BusWireT.Orientation.Horizontal ->
        let rectIntersect (segH: BusWireT.ASegment) (segV: BusWireT.ASegment) =
            segH.Start.Y <= segV.Start.Y
            && segV.Start.Y <= segH.End.Y
            && segV.Start.X <= segH.Start.X
            && segH.Start.X <= segV.End.X

        if seg1.Orientation = BusWireT.Orientation.Horizontal then
            rectIntersect seg1 seg2
        else
            rectIntersect seg2 seg1
    | _ -> false

// T3R
/// <summary>
/// Calculates the total number of unique right angle intersections between wire segments in the schematic.
/// </summary>
/// <param name="sheet">The schematic model containing all wires and segments.</param>
/// <returns>The number of unique right angle intersections between wire segments.</returns>
/// <remarks>
/// Assumes wire segments can only be horizontal or vertical. Intersections within the same wire are not considered.
/// </remarks>
let countRightAngleIntersections (sheet: SheetT.Model) : int =
    let wires = Map.toList sheet.Wire.Wires
    let allSegments =
        wires
        |> List.collect (snd >> getNonZeroAbsSegments)

    let segmentPairs = List.allPairs allSegments allSegments

    let rightAngleIntersections =
        segmentPairs
        |> List.filter (fun (seg1, seg2) ->
            seg1.Segment.WireId <> seg2.Segment.WireId
            || seg1.Segment.Index <> seg2.Segment.Index)
        |> List.filter (fun (seg1, seg2) -> doIntersectAtRightAngle seg1 seg2)

    rightAngleIntersections.Length / 2

/// <summary>
/// Calculates the length of a 2D vector.
/// </summary>
/// <param name="vec">The 2D vector to calculate the length of.</param>
/// <returns>The length of the vector (float).</returns>
let vectorLength (vec: XYPos) : float = sqrt ((vec.X ** 2.0) + (vec.Y ** 2.0))

// T4R
/// <summary>
/// Calculates the sum of lengths of unique visible wire segments on the sheet.
/// Only counts each segment once for segments that overlap and belong to the same net (shared InputPortId).
/// </summary>
/// <param name="model">The model containing all wires and segments.</param>
/// <returns>The total length of all unique visible segments, accounting for overlaps on the same net.</returns>
let sumOfUniqueVisibleSegmentLengths (model: SheetT.Model) : float =
    let allVisibleSegmentsWithInputPort =
        model.Wire.Wires
        |> Map.toList
        |> List.collect (fun (wId, wire) ->
            visibleSegments wId model
            |> List.map (fun vec -> wire.InputPort, vec))

    let groupedSegmentsByNetAndVector =
        allVisibleSegmentsWithInputPort
        |> List.groupBy fst // Group by InputPortId
        |> List.collect (fun (_, segments) -> segments |> List.groupBy snd |> List.map snd)

    let uniqueSegmentLengths =
        groupedSegmentsByNetAndVector
        |> List.map (fun segments -> segments |> List.head |> snd |> vectorLength)

    uniqueSegmentLengths |> List.sum

// T5R
/// <summary>
/// Counts the total number of visible right angles formed by wire segments across the entire sheet.
/// </summary>
/// <param name="model">The model of the sheet containing wires and their segments.</param>
/// <returns>
/// The total count of right angles visible in the wiring of the sheet.
/// </returns>
/// <remarks>
/// A right angle is considered visible if it is formed by two consecutive non-zero length segments with different orientations.
/// </remarks>
let countVisibleRightAngles (model: SheetT.Model) =
    model.Wire.Wires
    |> Map.values
    |> Seq.toList
    |> List.map (fun w -> getNonZeroAbsSegments w)
    |> List.map (fun segs ->
        segs
        |> List.fold
            (fun (numRightAngles, prevOriOpt) seg ->
                let updatedNumRightAngles =
                    match prevOriOpt with
                    | Some prevOri ->
                        if prevOri <> seg.Orientation then
                            numRightAngles + 1
                        else
                            numRightAngles
                    | None -> numRightAngles
                (updatedNumRightAngles, Some seg.Orientation))
            (0, None)
        |> fst)
    |> List.sum

/// <summary>
/// Determines if the start point of a wire segment is located inside any symbol's bounding box.
/// </summary>
/// <param name="seg">The wire segment to check.</param>
/// <param name="symbolsBoundingBoxes">A map of symbol component IDs to their corresponding bounding boxes.</param>
/// <returns>True if the start point of the segment is inside a symbol's bounding box; otherwise, false.</returns>
let isSegmentStartInsideSymbol (seg: BusWireT.ASegment) (symbolsBoundingBoxes: Map<ComponentId, BoundingBox>) : bool =
    symbolsBoundingBoxes
    |> Map.exists (fun _ bbox ->
        let segStartInside =
            bbox.TopLeft.X <= seg.Start.X
            && seg.Start.X <= bbox.TopLeft.X + bbox.W
            && bbox.TopLeft.Y <= seg.Start.Y
            && seg.Start.Y <= bbox.TopLeft.Y + bbox.H
        segStartInside)

// T6R
/// <summary>
/// Identifies wire segments that retrace themselves or end by retracing into a symbol on the sheet.
/// </summary>
/// <param name="sheet">The model of the sheet containing all wire segments.</param>
/// <returns>
/// A tuple of two lists: the first contains all retracing segments, and the second contains all end segments that retrace into symbols.
/// </returns>
/// <remarks>
/// A segment is considered to be retracing if it is a zero-length segment flanked by two segments of opposite directions. An end segment retraces if it either starts or ends inside a symbol's bounding box.
/// </remarks>
let identifyRetracingSegmentsAndEnds (sheet: SheetT.Model) : BusWireT.ASegment list * BusWireT.ASegment list =
    let symbolsBoundingBoxes = getBoundingBoxes sheet.Wire.Symbol
    sheet.Wire.Wires
    |> Map.fold
        (fun acc _ wire ->
            let absSegments = getNonZeroAbsSegments wire
            let processSegment i (seg: BusWireT.ASegment) =
                let isRetracing =
                    i > 0
                    && i < List.length absSegments - 1
                    && let prevSeg = absSegments.[i - 1] in
                       let nextSeg = absSegments.[i + 1] in

                       prevSeg.Segment.Length * nextSeg.Segment.Length < 0.0
                       && seg.Segment.Length = 0.0

                let isEndRetracing =
                    (i = 0 || i = List.length absSegments - 1)
                    && isSegmentStartInsideSymbol seg symbolsBoundingBoxes

                if isRetracing then
                    Some seg, None
                else if isEndRetracing then
                    None, Some seg
                else
                    None, None

            absSegments
            |> List.mapi processSegment
            |> List.fold
                (fun (accRetracing, accEnds) (retracingOpt, endRetracingOpt) ->
                    let updatedRetracing = Option.fold (fun acc r -> r :: acc) accRetracing retracingOpt
                    let updatedEndRetracing =
                        Option.fold (fun acc e -> e :: acc) accEnds endRetracingOpt
                    (updatedRetracing, updatedEndRetracing))
                acc)
        ([], [])
