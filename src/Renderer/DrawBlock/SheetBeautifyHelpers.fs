module SheetBeautifyHelpers

open DrawModelType
open Symbol
open CommonTypes
open Optics
open Operators


//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team

// let position_ = 
//   Prism.create
//     (fun (sheet: SheetT.Model, symbolId: ComponentId) ->
//       sheet.Wire.Symbol.Symbols
//       |> Map.tryPick (fun _ symbol ->
//         match (fst SymbolT.component_ symbol).Id with
//         | id when ComponentId id = symbolId -> Some symbol.Pos
//         | _ -> None))

//     (fun (position: XYPos) (sheet: SheetT.Model, symbolId: ComponentId) ->
//       let symbol = 
//         sheet.Wire.Symbol.Symbols
//         |> Map.tryPick (fun _ symbol ->
//           match (fst SymbolT.component_ symbol).Id with
//           | id when ComponentId id = symbolId -> Some symbol
//           | _ -> None) 

//       match symbol with
//       | Some symbol -> (Optic.set SheetT.symbols_ (Map.add symbolId {symbol with Pos = position} sheet.Wire.Symbol.Symbols) sheet), symbolId
//       | None -> sheet, symbolId)

/// create prism from Model to any Symbol property, useful in combination with predefined lenses to access components, position and others.
/// If the property does not exist, the set will return the unchanged sheet (this is a policy decision, this could be changed to cause an exception)
let makeMyPrism_ (lens: Lens<SymbolT.Symbol, 'a>) (id: ComponentId): Prism<SheetT.Model, 'a> =
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

let printInline print = printf $"{print}"; print
let printListInline print = List.map (fun elem -> printf $"{elem}") print |> ignore; print

let B1: Lens<SymbolT.Symbol, float * float> = 
  let dims_ = {| h = SymbolT.component_ >-> h_; w = SymbolT.component_ >-> w_ |}

  let B1R (symbol: SymbolT.Symbol) =
    Optic.get dims_.h symbol, Optic.get dims_.w symbol

  let B1W (dims: float * float) (symbol: SymbolT.Symbol) =
    Optic.set dims_.h (fst dims) symbol
    |> Optic.set dims_.w (snd dims)

  B1R, B1W

let B2 =
  let B2W (sheet: SheetT.Model) (id: ComponentId) (position: XYPos) =
    let sheetPosition_ = makeMyPrism_ SymbolT.posOfSym_ id

    Optic.set sheetPosition_ position sheet
    
  let B2W' (symbol: SymbolT.Symbol) (position: XYPos) =
    {symbol with Pos = position}

  (), B2W

let B3 edge: Lens<SymbolT.Symbol, string list> =
  let B3R (symbol: SymbolT.Symbol) =
    let symbolInfo_ = SymbolT.portMaps_ >-> SymbolT.order_
    
    Optic.get symbolInfo_ symbol
    |> Map.tryFind edge
    |> Option.defaultValue []

  let B3W (order: string list) (symbol: SymbolT.Symbol) =
    let symbolInfo_ = SymbolT.portMaps_ >-> SymbolT.order_

    Optic.get symbolInfo_ symbol
    |> Map.add edge order
    |> fun map -> Optic.set symbolInfo_ map symbol

  B3R, B3W

/// Alternative to read SymbolInfo and get order there. I had initially done this before realising this was wrong
/// I had fun with prisms so decided to leave this here
let B3' edge: Lens<SymbolT.Symbol, string list> =
  let B3R' (symbol: SymbolT.Symbol) =
    let symbolInfo_ = Prism.create (fun c -> c.SymbolInfo) (fun i c -> {c with SymbolInfo = Some i})

    let symbolToInfo_ = SymbolT.component_ >-> symbolInfo_ >?> portOrder_

    Optic.get symbolToInfo_ symbol
    |> Option.map (Map.tryFind edge)
    |> Option.defaultValue None
    |> Option.defaultValue []
    
  let B3W' (order: string list) (symbol: SymbolT.Symbol) =
    let symbolInfo_ = Prism.create (fun c -> c.SymbolInfo) (fun i c -> {c with SymbolInfo = Some i})
    let symbolToInfo_ = SymbolT.component_ >-> symbolInfo_ >?> portOrder_

    Optic.get symbolToInfo_ symbol
    |> Option.map (Map.add edge order)
    |> Option.map (fun map -> Optic.set symbolToInfo_ map symbol)
    |> Option.defaultValue symbol // if unchanged return original symbol

  B3R', B3W'

let B4: Lens<SymbolT.Symbol, bool> =
  let flippedInput_ = Prism.create (fun (s: SymbolT.Symbol) -> s.ReversedInputPorts) (fun b s -> {s with ReversedInputPorts = Some b})
  
  let B4R (symbol: SymbolT.Symbol) =
    Optic.get flippedInput_ symbol
    |> Option.defaultValue false

  let B4W isFlipped (symbol: SymbolT.Symbol) =
    Optic.set flippedInput_ isFlipped symbol
  
  B4R, B4W

let B5 =
  let model_ = SheetT.wire_ >-> BusWireT.symbol_

  let B5R (sheet: SheetT.Model) (port: Port) =
    getPortPosModel (Optic.get model_ sheet) port
    // getPortPosModel sheet.Wire.Symbol port // alternative without lens

  B5R, ()

let B6 =
  let model_ = SheetT.wire_ >-> BusWireT.symbol_

  let B6R (sheet: SheetT.Model) (symbol: SymbolT.Symbol) =
    getBoundingBox (Optic.get model_ sheet) symbol.Id
    // getBoundingBox sheet symbol.Id // alternative without lens

  B6R, ()

let B7: Lens<SymbolT.Symbol, Rotation> =
  let rotation_ = 
    Lens.create (fun (s: SymbolT.Symbol) -> s.STransform) (fun t s -> {s with STransform = t}) 
    >-> Lens.create (fun st -> st.Rotation) (fun r st -> {st with Rotation = r})

  let B7R (symbol: SymbolT.Symbol) =
    Optic.get rotation_ symbol

  let B7W (rotation: Rotation) (symbol: SymbolT.Symbol) =
    Optic.set rotation_ rotation symbol

  B7R, B7W

let B8: Lens<SymbolT.Symbol, bool> =
  let flip_ = 
    Lens.create (fun (s: SymbolT.Symbol) -> s.STransform) (fun t s -> {s with STransform = t}) 
    >-> Lens.create (fun st -> st.Flipped) (fun f st -> {st with Flipped = f})

  let B8R (symbol: SymbolT.Symbol) =
    Optic.get flip_ symbol

  let B8W (flip: bool) (symbol: SymbolT.Symbol) =
    Optic.set flip_ flip symbol

  B8R, B8W

let T1R (sheet: SheetT.Model) =
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

let T2R (sheet: SheetT.Model) =
  sheet.Wire.Wires
  |> Helpers.mapValues
  |> Seq.toList
  |> List.map (fun w -> w, visibleSegments w.WId sheet, BusWireRoute.findWireSymbolIntersections sheet.Wire w)
  |> List.filter (fun (_, vSeg, intersectingSymbBBs) -> vSeg <> [] && intersectingSymbBBs <> [])
  |> List.map (fun (thisWire, vSeg, intersectingSymbBBs) -> // for each wire
    // for each segment, does at least one symbol intersect
    vSeg
    |> List.scan (+) thisWire.StartPos
    // We need to remove the first and last segments because they touch the bounding boxes of the symbols they connect
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

let T3R (sheet: SheetT.Model): int =
  let absSegmentsIntersect (h: BusWireT.ASegment) (v: BusWireT.ASegment): XYPos option =
      match h.Orientation, v.Orientation with
      | BusWireT.Horizontal, BusWireT.Vertical ->
        let ymin, ymax = min v.Start.Y v.End.Y, max v.Start.Y v.End.Y
        let xmin, xmax = min h.Start.X h.End.X, max h.Start.X h.End.X 
        
        if (h.Start.Y < ymax && h.Start.Y > ymin) && (v.Start.X < xmax && v.Start.X > xmin)
        then Some {X = v.Start.X; Y = h.Start.Y}
        else None
        
      | _ -> None
      |> fun opt -> match opt with | Some _ -> printf ">> int"; opt | _ -> opt

  let wires = 
    sheet.Wire.Wires
    |> Helpers.mapValues
    |> Seq.toList

  (wires, wires)
  ||> List.allPairs
  |> List.fold (fun intersections (firstWire, secondWire) ->
    // remove self intersection
    if firstWire.WId = secondWire.WId then intersections else
  
    match 
      BlockHelpers.isWireInNet sheet.Wire firstWire,
      BlockHelpers.isWireInNet sheet.Wire secondWire,
      BlockHelpers.getNonZeroAbsSegments firstWire,
      BlockHelpers.getNonZeroAbsSegments secondWire
    with
    // Case where both wires do not belong to nets
    | None, None, aS1, aS2 ->
      printf $">> 0nl"; (aS1, aS2)
      ||> List.allPairs
      |> List.map (fun (seg1, seg2) -> absSegmentsIntersect seg1 seg2)
    // | Some nl1, None, _, aSeg | None, Some nl1, aSeg, _ ->
    | Some nl1, None, _, aSeg ->
      snd nl1 |> (printf $">> 1nl"; List.map snd)
      // snd nl1 |> (printf $">> 1nl {snd nl1 |> List.map snd} {aSeg}"; List.map snd)
      |> List.map BlockHelpers.getNonZeroAbsSegments
      |> List.concat
      |> List.distinctBy (fun seg -> seg.Start, seg.End) // remove duped segments
      |> fun netSegs -> (netSegs, aSeg)
      ||> List.allPairs
      |> List.map (fun (seg1, seg2) -> absSegmentsIntersect seg1 seg2)
    | None, Some _, _, _ -> [None] // already counted (twice)
    | Some nl1, Some nl2, aS1, aS2 ->
        match
          fst nl1,
          fst nl2,
          snd nl1,
          snd nl2
        with
        // same net
        | oPort1, oPort2, _, _ when oPort1 = oPort2 -> [None]
        | _, _, nl1, nl2 -> [None]

    |> List.choose id
    |> List.distinct
    |> (@) intersections
  ) []
  |> List.distinct
  |> printInline
  |> List.length

let T4R (sheet: SheetT.Model) =
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

let T5R (sheet: SheetT.Model) =
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
        lst.Head.WId
        |> fun wid -> visibleSegments wid sheet
        |> printListInline
        |> List.length
        |> fun len -> len - 1 
        |> printInline 
      else
        lst
        |> List.map (fun wire ->
          visibleSegments wire.WId sheet
          // |> List.map (fun seg -> seg + wire.StartPos)
          |> List.distinct
        )
        |> List.concat 
        |> printListInline
        |> List.distinct
        |> List.length
        |> fun len -> len - 1
        |> printInline
  ) 
  |> List.fold (+) 0
