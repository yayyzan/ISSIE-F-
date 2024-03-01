module SheetBeautifyHelpers

open DrawModelType
open Symbol
open CommonTypes
open Optics
open Operators

//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team

/// create prism from Model to any Symbol property, useful in combination with predefined lenses to access components, position and others.
/// If the property does not exist, the setter will return the unchanged sheet (this is a policy decision, this could be changed to cause an exception)
let createSymbolToSymbolPropertyPrism (lens: Lens<SymbolT.Symbol, 'a>) (id: ComponentId): Prism<SheetT.Model, 'a> =
  let prismGet (lens: Lens<SymbolT.Symbol, 'a>) (id: ComponentId) (sheet: SheetT.Model) = 
    Optic.get SheetT.symbols_ sheet
    |> Map.tryFind id
    |> Option.map (Optic.get lens)

  let prismSet (lens: Lens<SymbolT.Symbol, 'a>) (id: ComponentId) (prop: 'a) (sheet: SheetT.Model) =
    let originalSymbols = Optic.get SheetT.symbols_ sheet
    
    originalSymbols
    |> Map.tryFind id
    |> Option.map (Optic.set lens prop)
    |> Option.map (fun newSymb -> 
      Map.add newSymb.Id newSymb originalSymbols
    )
    |> Option.defaultValue originalSymbols
    |> fun newMap -> Optic.set SheetT.symbols_ newMap sheet

  Prism.create (prismGet lens id) (prismSet lens id)

// These three functions make it easier to print without interrupting a pipeline (easy to comment in and out)

/// <summary> Print the argument as "${param}"</summary>
/// <param name="print">The value to print and pass</param>
/// <returns>The unchanged argument</returns>
let printInline print = printf $"{print}"; print

/// <summary> Print each element of the list as "${param}"</summary>
/// <param name="print">The list of values to print and pass</param>
/// <returns>The unchanged list</returns>
let printListInline print = List.map (fun elem -> printf $"{elem}") print |> ignore; print

/// <summary> Print an argument as "${param}"</summary>
/// <param name="print">The value to print</param>
/// <param name="pass">The value to return</param>
/// <returns>The unchanged argument</returns>
let printAndPass print pass = printf $"{print}"; pass

// B1
/// <summary>Create a lens between symbol and its dimensions</summary>
let symbolDimensions_: Lens<SymbolT.Symbol, float * float> = 
  let dims_ = {| h = SymbolT.component_ >-> h_; w = SymbolT.component_ >-> w_ |}

  let getSymbolDimensions (symbol: SymbolT.Symbol) =
    Optic.get dims_.h symbol, Optic.get dims_.w symbol

  let setSymbolDimensions (dims: float * float) (symbol: SymbolT.Symbol) =
    Optic.set dims_.h (fst dims) symbol
    |> Optic.set dims_.w (snd dims)

  getSymbolDimensions, setSymbolDimensions

// B2W
// B2W is inside the function so that the principal function can be made to return
// a lens if so desired. This would be achieved by designing a getter function.
/// <summary>Set symbol position in sheet</summary>
/// <param name="sheet">Sheet with corresponding symbol</param>
/// <param name="id">Component ID of symbol whose position to change</param>
/// <param name="position">Desired symbol position</param>
/// <returns>The sheet with the modified symbol</returns>
let setSybolPosition =
  /// <summary>Use custom prism to access position property of symbol from sheet</summary>
  let B2W (sheet: SheetT.Model) (id: ComponentId) (position: XYPos) =
    let sheetPosition_ = createSymbolToSymbolPropertyPrism SymbolT.posOfSym_ id

    Optic.set sheetPosition_ position sheet

  /// <summary>Set symbol position directly on symbol</summary>
  let B2W' (symbol: SymbolT.Symbol) (position: XYPos) =
    {symbol with Pos = position}

  B2W

// B3
/// <summary>Create lens between symbol and the order of ports</summary>
let symbolPortOrderOnEdge_ edge: Lens<SymbolT.Symbol, string list> =
  let getSymbolPortOrder (symbol: SymbolT.Symbol) =
    let symbolInfo_ = SymbolT.portMaps_ >-> SymbolT.order_
    
    Optic.get symbolInfo_ symbol
    |> Map.tryFind edge
    |> Option.defaultValue []

  let setSymbolPortOrder (order: string list) (symbol: SymbolT.Symbol) =
    let symbolInfo_ = SymbolT.portMaps_ >-> SymbolT.order_

    Optic.get symbolInfo_ symbol
    |> Map.add edge order
    |> fun map -> Optic.set symbolInfo_ map symbol

  getSymbolPortOrder, setSymbolPortOrder

/// Alternative to read SymbolInfo and get order there. I had initially done this before realising this was wrong
/// I had fun with prisms so decided to leave this here
let B3' edge: Lens<SymbolT.Symbol, string list> =
  let symbolInfo_ = Prism.create (fun c -> c.SymbolInfo) (fun i c -> {c with SymbolInfo = Some i})
  let symbolToInfo_ = SymbolT.component_ >-> symbolInfo_ >?> portOrder_
  let B3R' (symbol: SymbolT.Symbol) =
     Optic.get symbolToInfo_ symbol
    |> Option.map (Map.tryFind edge)
    |> Option.defaultValue None
    |> Option.defaultValue []
    
  let B3W' (order: string list) (symbol: SymbolT.Symbol) =
    Optic.get symbolToInfo_ symbol
    |> Option.map (Map.add edge order)
    |> Option.map (fun map -> Optic.set symbolToInfo_ map symbol)
    |> Option.defaultValue symbol // if unchanged return original symbol

  B3R', B3W'

// B4
/// <summary>Create lens between symbol and the state of inputs of a MUX2</summary>
let symbolInputFlipped: Lens<SymbolT.Symbol, bool> =
  let flippedInput_ = Prism.create (fun (s: SymbolT.Symbol) -> s.ReversedInputPorts) (fun b s -> {s with ReversedInputPorts = Some b})
  
  let isSymbolInputFlipped (symbol: SymbolT.Symbol) =
    Optic.get flippedInput_ symbol
    |> Option.defaultValue false

  let setSymbolInputFlip isFlipped (symbol: SymbolT.Symbol) =
    Optic.set flippedInput_ isFlipped symbol
  
  isSymbolInputFlipped, setSymbolInputFlip

// B5R
// B5R is inside the function so that the principal function can be made to return
// a lens if so desired. This would be achieved by designing a getter function.
/// <summary>Get the position of a port on a sheet.</summary>
let getPortSheetPosition (sheet: SheetT.Model) (port: Port)=
  let model_ = SheetT.wire_ >-> BusWireT.symbol_
  getPortLocation None (Optic.get model_ sheet) port.Id

// B6R
/// <summary>Get the bounding box of a symbol on a sheet.</summary>
let getSymbolBoundingBox (sheet: SheetT.Model) (symbol: SymbolT.Symbol) =
  let model_ = SheetT.wire_ >-> BusWireT.symbol_
  getBoundingBox (Optic.get model_ sheet) symbol.Id

// B7
/// <summary>Create lens from Symbol to rotation state of a symbol</summary>
let symbolRotateState_: Lens<SymbolT.Symbol, Rotation> =
  let rotation_ = 
    Lens.create (fun (s: SymbolT.Symbol) -> s.STransform) (fun t s -> {s with STransform = t}) 
    >-> Lens.create (fun st -> st.Rotation) (fun r st -> {st with Rotation = r})

  let getSymbolRotateState (symbol: SymbolT.Symbol) =
    Optic.get rotation_ symbol

  let setSymbolRotateState (rotation: Rotation) (symbol: SymbolT.Symbol) =
    Optic.set rotation_ rotation symbol

  getSymbolRotateState, setSymbolRotateState

// B8
/// <summary>Create lens from Symbol to flip state of a symbol</summary>
let symbolFlipState_: Lens<SymbolT.Symbol, bool> =
  let flip_ = 
    Lens.create (fun (s: SymbolT.Symbol) -> s.STransform) (fun t s -> {s with STransform = t}) 
    >-> Lens.create (fun st -> st.Flipped) (fun f st -> {st with Flipped = f})

  let getSymbolFlipState (symbol: SymbolT.Symbol) =
    Optic.get flip_ symbol

  let setSymbolFlipState (flip: bool) (symbol: SymbolT.Symbol) =
    Optic.set flip_ flip symbol

  getSymbolFlipState, setSymbolFlipState

// T1R
/// <summary>Counts the number of pairs of symbols whose bounding boxes intersect on the sheet.</summary>
/// <remarks>This function retrieves bounding boxes for symbols on the sheet and checks for intersections between them.
/// It removes duplicates to count unique pairs of intersecting symbols.</remarks>
/// <param name="sheet">The sheet with the symbols where intersections will be counted</param>
/// <returns>An integer representing the number of symbol pairs intersecting on the sheet</returns>
let numberSymbolPairsIntersecting (sheet: SheetT.Model) =
  let BoundingBoxes = Optic.get SheetT.boundingBoxes_ sheet
  let boxes =
      Helpers.mapValues BoundingBoxes
      |> Array.toList
      |> List.mapi (fun n box -> n,box)
  List.allPairs boxes boxes
  |> List.filter (fun ((n1,box1),(n2,box2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox box1 box2)
  |> List.length
  |> (/) 2 // remove duplicates

/// copied here because I couldn't access it
/// 
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
      /// Index must be in range 1..segVecs
      let tryCoalesceAboutIndex (segVecs: XYPos list) (index: int)  =
          // MODIFICATION HERE to address exception
          // if segVecs[index] =~ XYPos.zero
          if index < segVecs.Length && segVecs[index] =~ XYPos.zero
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

// T2R
/// <summary>Counts the number of visible wire segments that intersect with one or more symbols on the sheet.</summary>
/// <remarks>This function calculates intersections between wire segments and bounding boxes of symbols.
/// It excludes wire segments that touch the bounding boxes of the symbols they connect.</remarks>
/// <param name="sheet">The sheet with the wires and symbols where intersections will be counted</param>
/// <returns>An integer representing the number of visible wire intersections in this sheet</returns>
let countVisibleSegmentsIntersectingSymbols (sheet: SheetT.Model) =
  sheet.Wire.Wires
  |> Helpers.mapValues
  |> Seq.toList
  |> List.map (fun w -> w, visibleSegments w.WId sheet, BusWireRoute.findWireSymbolIntersections sheet.Wire w)
  |> List.filter (fun (_, vSeg, intersectingSymbBBs) -> vSeg <> [] && intersectingSymbBBs <> [])
  |> List.map (fun (thisWire, vSeg, intersectingSymbBBs) -> // for each wire
    // for each segment, does at least one symbol intersect
    vSeg
    |> List.scan (+) thisWire.StartPos
    // We need to remove the first and last segments because they can potentially touch the bounding boxes of the symbols they connect
    // Not sure what the best way to implement this could be, but it works for now
    |> List.tail
    |> List.rev
    |> List.tail
    |> List.rev
    |> List.pairwise // list will have minimum 2 elements (1 seg)
    |> List.map (fun (segStart, segEnd) -> 
      intersectingSymbBBs
      |> List.map (fun BB -> 
        let distance = BlockHelpers.segmentIntersectsBoundingBox BB segStart segEnd
        distance)
      // coalesce over number of intersected Symbols per segment
      |> List.fold (fun numSymbolsIntersected opt ->
        match opt with
        | Some _ -> numSymbolsIntersected + 1
        | _ -> numSymbolsIntersected
      ) 0
    )
    // coalesce over segments of wire
    |> List.fold (fun count numberOfSymbolsIntersected -> if numberOfSymbolsIntersected = 0 then count else count + 1) 0) 
  // coalesce over wires
  |> List.fold (+) 0

// with ASegments but doesn't coalesce :(
// let T2R' (sheet: SheetT.Model) =
//   sheet.Wire.Wires
//   |> Helpers.mapValues
//   |> Seq.toList
//   |> List.map (fun w -> w, BlockHelpers.getNonZeroAbsSegments w, BusWireRoute.findWireSymbolIntersections sheet.Wire w)
//   |> List.filter (fun (_, vSeg, intersectingSymbBBs) -> vSeg <> [] && intersectingSymbBBs <> [])
//   |> List.map (fun (_, aSeg, intersectingSymbBBs) -> // for each wire
//     // for each segment, does at least one symbol intersect
//     aSeg
//     // |> List.pairwise
//     // |>  // need to coalesce here!
//     |> List.map (fun aSeg ->
//       intersectingSymbBBs
//       |> List.map (fun BB -> 
//         let distance = BlockHelpers.segmentIntersectsBoundingBox BB aSeg.Start aSeg.End
//         distance)
//       // coalesce over number of intersected Symbols per segment
//       |> List.fold (fun numSymbolsIntersected opt ->
//         match opt with
//         | Some _ -> numSymbolsIntersected + 1
//         | _ -> numSymbolsIntersected
//       ) 0
//     )
//     // coalesce over segments of wire
//     |> List.fold (fun count numberOfSymbolsIntersected -> if numberOfSymbolsIntersected = 0 then count else count + 1) 0) 
//   // coalesce over wires
//   |> List.fold (+) 0

// T3R
/// <summary>Counts the number of intersections between horizontal and vertical wire segments on the sheet.
/// Does not double count intersections where multiple overlapping wires in the same net intersect other wires.</summary>
/// <param name="sheet">The sheet with the wires where intersections will be counted</param>
/// <returns>An integer representing the number of visible wire intersections in this sheet</returns>
let visibleWireIntersections (sheet: SheetT.Model): int =
  /// CAUTION: This function only counts intersections when the first segment is horizontal
  /// and the second is vertical. This is to prevent multiple counting of the same intersection
  let absSegmentsIntersect (h: BusWireT.ASegment) (v: BusWireT.ASegment): XYPos option =
      match h.Orientation, v.Orientation with
      | BusWireT.Horizontal, BusWireT.Vertical ->
        let ymin, ymax = min v.Start.Y v.End.Y, max v.Start.Y v.End.Y
        let xmin, xmax = min h.Start.X h.End.X, max h.Start.X h.End.X 
        
        if (h.Start.Y < ymax && h.Start.Y > ymin) && (v.Start.X < xmax && v.Start.X > xmin)
        then Some {X = v.Start.X; Y = h.Start.Y}
        else None
      | _ -> None

  let wires = 
    sheet.Wire.Wires
    |> Helpers.mapValues
    |> Seq.toList

  (wires, wires)
  ||> List.allPairs
  |> List.fold (fun intersections (firstWire, secondWire) ->
    (BlockHelpers.getNonZeroAbsSegments firstWire, BlockHelpers.getNonZeroAbsSegments secondWire)
    ||> List.allPairs
    |> List.map (fun (seg1, seg2) -> absSegmentsIntersect seg1 seg2)
    |> List.choose id
    |> List.distinct
    |> (@) intersections
  ) []
  |> List.distinct
  // |> printInline
  |> List.length

// T4R
/// <summary>Measure the total length of wiring visible in sheet.
/// Computes the total length considering non-overlapping segments and adjusting for overlaps at T junctions.
/// The result is the cumulative length of wires visible on the sheet.</summary>
/// <remarks>If two wires in the same net overlap and the overlapping segments do not
/// share either a start or end point, then the lengths will be double counted.</remarks>
/// <param name="sheet">The sheet with the wires where right angles will be counted</param>
/// <returns>An integer representing the number of visible right angles in wiring for this sheet</returns>
let totalVisibleWireLength (wModel: BusWireT.Model) =
  let netlist = 
    wModel.Wires
    |> Helpers.mapValues
    |> Seq.toList
    |> List.groupBy (fun wire -> wire.OutputPort)

  netlist
  |> List.map (fun net ->
    snd net
    |> fun lst -> 
      if lst.Length = 1
      then // no overlaps possible
        lst.Head.Segments
        |> List.map (fun seg -> seg.Length |> abs)
        |> List.fold (+) 0.0
      else
        lst
        |> List.map BlockHelpers.getNonZeroAbsSegments
        |> List.concat
        |> List.distinctBy (fun seg -> seg.Start, seg.End) // remove obvious overlaps
        |> (fun segs -> 
          List.fold (fun length (seg: BusWireT.ASegment) -> 
            segs
            |> List.tryFind (fun seg' ->
              // do not double count near T junction
              seg'.Segment.GetId <> seg.Segment.GetId 
              && seg'.Segment.Length < seg.Segment.Length
              && (seg'.Start = seg.Start
                || seg'.End = seg.End))
            |> function
            | Some dupl -> length + (abs seg.Segment.Length) - (abs dupl.Segment.Length)
            | None -> length + (abs seg.Segment.Length)
          ) 0.0 segs
        )
  ) 
  |> List.fold (+) 0.0

// T5R
/// <summary>Calculate the number of right angles formed by wires. When wires are 
/// in a net and overlapping, the turn will not be double counted. Only visible
/// right angles turns in wiring are counted.</summary>
/// <param name="sheet">The sheet with the wires where right angles will be counted</param>
/// <returns>An integer representing the number of visible right angles in wiring for this sheet</returns>
let visibleWireRightAngles (sheet: SheetT.Model) =
  let netlist = 
    sheet.Wire.Wires
    |> Helpers.mapValues
    |> Seq.toList
    |> List.groupBy (fun wire -> wire.OutputPort)

  netlist
  |> List.map (fun net ->
    snd net
    |> fun lst -> 
      if lst.Length = 1
      then // no overlaps possible
        let posList = 
          lst.Head.WId
          |> fun wid -> visibleSegments wid sheet

        posList
        |> List.length
        |> (fun len -> 
          // check for 0 0 segment
          let zeroes = 
            posList
            |> List.tryFind ((=) XYPos.zero) 
            |> function
            | Some _ -> 3
            | _ -> 1

          len - zeroes
        )

      else
        lst
        |> List.map (fun wire ->
          visibleSegments wire.WId sheet
          |> List.scan (+) wire.StartPos
          |> List.tail // remove startpos
        )
        |> List.concat 
        |> List.distinct
        |> List.length
        |> fun len -> len - lst.Length
  ) 
  |> List.fold (+) 0

// TODO T6
// 
// Well, I spent far too long trying to get the above to work well (shouldn't have tested them :( ) and now
// I no longer have time to try T6.