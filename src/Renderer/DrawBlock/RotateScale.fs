module RotateScale

(*

Ezra (er121) 
Refactoring Summary of RotateScale Module:

- Improved Function Naming: Adopted more descriptive function names for clarity and ease of understanding.
- Enhanced Documentation: Added detailed XML documentation to every function, explaining their purpose, parameters, and return values for better maintenance and usage clarity.
- Simplified Logic: Streamlined calculations for bounding boxes, rotations, and flips, making the logic more straightforward and reducing computational redundancy.
- Error Handling: Introduced error checking in functions like `getBlock` to handle empty lists gracefully, improving the robustness of the code.
- Alignment with F# Best Practices: Ensured that the code adheres more closely to F# conventions and functional programming principles, including effective use of pattern matching.
- Clarified Transformation Effects: Made the effects of transformations on symbols more transparent, aiding in predictability and ease of use.
- Reduced Syntactic Noise: Eliminated unnecessary verbosity in the code, making it cleaner and more readable.

These changes aim to make the RotateScale module more maintainable, understandable, and efficient, laying a solid foundation for future extensions and improvements.

*)

open CommonTypes
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open SymbolUpdate
open Symbol
open Optics
open Operators
open BlockHelpers
open SymbolResizeHelpers

(* 
    This module contains the code that rotates and scales blocks of components.
    It was collected from HLP work in 2023 and has some technical debt and also unused functions.
    It requires better documentation of teh pasrts now used.
*)

/// Record containing all the information required to calculate the position of a port on the sheet.
type PortInfo =
    { port: Port
      sym: Symbol
      side: Edge
      ports: string list
      gap: float
      topBottomGap: float
      portDimension: float
      h: float
      w: float
      portGap: float }

/// Type used to record a wire between two symbols.
type WireSymbols = { SymA: Symbol; SymB: Symbol; Wire: Wire }

/// TODO: this is mostly copy pasted code from Symbol.getPortPos, perhaps abstract out the existing code there to use makePortInfo.
/// Could not simply use getPortPos because more data (side, topBottomGap, etc.) is needed to caclulate the new dimensions of the resized symbol.
let makePortInfo (sym: Symbol) (port: Port) =
    let side = getSymbolPortOrientation sym port
    let ports = sym.PortMaps.Order[side] //list of ports on the same side as port
    let gap = getPortPosEdgeGap sym.Component.Type
    let topBottomGap = gap + 0.3 // extra space for clk symbol
    let portDimension = float ports.Length - 1.0
    let h, w = getRotatedHAndW sym

    let portGap =
        match side with
        | Left
        | Right -> float h / (portDimension + 2.0 * gap)
        | Bottom
        | Top -> float w / (portDimension + 2.0 * topBottomGap)

    { port = port
      sym = sym
      side = side
      ports = ports
      gap = gap
      topBottomGap = topBottomGap
      portDimension = portDimension
      h = h
      w = w
      portGap = portGap }

let getPortAB wModel wireSyms =
    let ports = portsOfWires wModel [ wireSyms.Wire ]
    let portA = filterPortBySym ports wireSyms.SymA |> List.head
    let portB = filterPortBySym ports wireSyms.SymB |> List.head
    portA, portB

/// Try to get two ports that are on opposite edges.
let getOppEdgePortInfo
    (wModel: BusWireT.Model)
    (symbolToSize: Symbol)
    (otherSymbol: Symbol)
    : (PortInfo * PortInfo) option
    =
    let wires = wiresBtwnSyms wModel symbolToSize otherSymbol

    let tryGetOppEdgePorts wireSyms =
        let portA, portB = getPortAB wModel wireSyms
        let edgeA = getSymbolPortOrientation wireSyms.SymA portA
        let edgeB = getSymbolPortOrientation wireSyms.SymB portB

        match edgeA = edgeB.Opposite with
        | true -> Some(makePortInfo wireSyms.SymA portA, makePortInfo wireSyms.SymB portB)
        | _ -> None

    wires
    |> List.tryPick (fun w -> tryGetOppEdgePorts { SymA = symbolToSize; SymB = otherSymbol; Wire = w })

let alignPortsOffset (movePInfo: PortInfo) (otherPInfo: PortInfo) =
    let getPortRealPos pInfo =
        getPortPos pInfo.sym pInfo.port + pInfo.sym.Pos

    let movePortPos = getPortRealPos movePInfo
    let otherPortPos = getPortRealPos otherPInfo
    let posDiff = otherPortPos - movePortPos

    match movePInfo.side with
    | Top
    | Bottom -> { X = posDiff.X; Y = 0.0 }
    | Left
    | Right -> { X = 0.0; Y = posDiff.Y }

let alignSymbols (wModel: BusWireT.Model) (symbolToSize: Symbol) (otherSymbol: Symbol) : BusWireT.Model =

    // Only attempt to align symbols if they are connected by ports on parallel edges.
    match getOppEdgePortInfo (wModel: BusWireT.Model) symbolToSize otherSymbol with
    | None -> wModel
    | Some(movePortInfo, otherPortInfo) ->
        let offset = alignPortsOffset movePortInfo otherPortInfo
        let symbol' = moveSymbol offset symbolToSize
        let model' = Optic.set (symbolOf_ symbolToSize.Id) symbol' wModel
        BusWireSeparate.routeAndSeparateSymbolWires model' symbolToSize.Id

/// HLP23: To test this, it must be given two symbols interconnected by wires. It then resizes symbolToSize
/// so that the connecting wires are exactly straight
/// HLP23: It should work out the interconnecting wires (wires) from
/// the two symbols, wModel.Wires and sModel.Ports
/// It will do nothing if symbolToOrder is not a Custom component (which has adjustable size).
let reSizeSymbol (wModel: BusWireT.Model) (symbolToSize: Symbol) (otherSymbol: Symbol) : (Symbol) =
    let wires = wiresBtwnSyms wModel symbolToSize otherSymbol

    // Try to get two ports that are on opposite edges, if none found just use any two ports.
    let resizePortInfo, otherPortInfo =
        match getOppEdgePortInfo wModel symbolToSize otherSymbol with
        | None ->
            let pA, pB =
                getPortAB wModel { SymA = symbolToSize; SymB = otherSymbol; Wire = wires[0] }
            makePortInfo symbolToSize pA, makePortInfo symbolToSize pB
        | Some(pIA, pIB) -> (pIA, pIB)

    let h, w =
        match resizePortInfo.side with
        | Left
        | Right ->
            otherPortInfo.portGap
            * (resizePortInfo.portDimension
               + 2.0 * resizePortInfo.gap),
            resizePortInfo.w
        | Top
        | Bottom ->
            resizePortInfo.h,
            otherPortInfo.portGap
            * (resizePortInfo.portDimension
               + 2.0 * resizePortInfo.topBottomGap)

    match symbolToSize.Component.Type with
    | Custom _ ->
        let scaledSymbol = setCustomCompHW h w symbolToSize
        let scaledInfo = makePortInfo scaledSymbol resizePortInfo.port
        let offset = alignPortsOffset scaledInfo otherPortInfo
        moveSymbol offset scaledSymbol
    | _ -> symbolToSize

/// For UI to call ResizeSymbol.
let reSizeSymbolTopLevel (wModel: BusWireT.Model) (symbolToSize: Symbol) (otherSymbol: Symbol) : BusWireT.Model =
    printfn $"ReSizeSymbol: ToResize:{symbolToSize.Component.Label}, Other:{otherSymbol.Component.Label}"

    let scaledSymbol = reSizeSymbol wModel symbolToSize otherSymbol

    let model' = Optic.set (symbolOf_ symbolToSize.Id) scaledSymbol wModel
    BusWireSeparate.routeAndSeparateSymbolWires model' symbolToSize.Id

/// For each edge of the symbol, store a count of how many connections it has to other symbols.
type SymConnDataT = { ConnMap: Map<ComponentId * Edge, int> }

/// If a wire between a target symbol and another symbol connects opposite edges, return the edge that the wire is connected to on the target symbol
let tryWireSymOppEdge (wModel: Model) (wire: Wire) (sym: Symbol) (otherSym: Symbol) =
    let symEdge = wireSymEdge wModel wire sym
    let otherSymEdge = wireSymEdge wModel wire otherSym

    match symEdge = otherSymEdge.Opposite with
    | true -> Some symEdge
    | _ -> None

let updateOrInsert (symConnData: SymConnDataT) (edge: Edge) (cid: ComponentId) =
    let m = symConnData.ConnMap
    let count =
        Map.tryFind (cid, edge) m
        |> Option.defaultValue 0
        |> (+) 1
    { ConnMap = Map.add (cid, edge) count m }

// TODO: this is copied from Sheet.notIntersectingComponents. It requires SheetT.Model, which is not accessible from here. Maybe refactor it.
let noSymbolOverlap (boxesIntersect: BoundingBox -> BoundingBox -> bool) boundingBoxes sym =
    let symBB = getSymbolBoundingBox sym

    boundingBoxes
    |> Map.filter (fun sId boundingBox -> boxesIntersect boundingBox symBB && sym.Id <> sId)
    |> Map.isEmpty

/// Finds the optimal size and position for the selected symbol w.r.t. to its surrounding symbols.
let optimiseSymbol
    (wModel: BusWireT.Model)
    (symbol: Symbol)
    (boundingBoxes: Map<CommonTypes.ComponentId, BoundingBox>)
    : BusWireT.Model
    =

    // If a wire connects the target symbol to another symbol, note which edge it is connected to
    let updateData (symConnData: SymConnDataT) _ (wire: Wire) =
        let symS, symT = getSourceSymbol wModel wire, getTargetSymbol wModel wire

        let otherSymbol =
            match symS, symT with
            | _ when (symS.Id <> symbol.Id) && (symT.Id = symbol.Id) -> Some symS
            | _ when (symS = symbol) && (symT <> symbol) -> Some symT
            | _ -> None

        match otherSymbol with
        | Some otherSym ->
            let edge = tryWireSymOppEdge wModel wire symbol otherSym

            match edge with
            | Some e -> updateOrInsert symConnData e otherSym.Id
            | None -> symConnData // should not happen
        | None -> symConnData

    // Look through all wires to build up SymConnDataT.
    let symConnData =
        ({ ConnMap = Map.empty }, wModel.Wires)
        ||> Map.fold updateData

    let tryResize (symCount: ((ComponentId * Edge) * int) array) sym =

        let alignSym (sym: Symbol) (otherSym: Symbol) =
            let resizedSym = reSizeSymbol wModel sym otherSym
            let noOverlap = noSymbolOverlap DrawHelpers.boxesIntersect boundingBoxes resizedSym

            match noOverlap with
            | true -> true, resizedSym
            | _ -> false, sym

        let folder (hAligned, vAligned, sym) ((cid, edge), _) =
            let otherSym = Optic.get (symbolOf_ cid) wModel

            match hAligned, vAligned with
            | false, _ when edge = Top || edge = Bottom ->
                let hAligned', resizedSym = alignSym sym otherSym
                (hAligned', vAligned, resizedSym)
            | _, false when edge = Left || edge = Right ->
                let vAligned', resizedSym = alignSym sym otherSym
                (hAligned, vAligned', resizedSym)
            | _ -> (hAligned, vAligned, sym)

        let (_, _, sym') =
            ((false, false, sym), symCount)
            ||> Array.fold folder
        sym'

    let scaledSymbol =
        let symCount =
            Map.toArray symConnData.ConnMap
            |> Array.filter (fun (_, count) -> count > 1)
            |> Array.sortByDescending snd

        tryResize symCount symbol

    let model' = Optic.set (symbolOf_ symbol.Id) scaledSymbol wModel
    BusWireSeparate.routeAndSeparateSymbolWires model' symbol.Id

// ############################################################################################################
//                                      ----- Ezra's refactoring below -----
// ############################################################################################################

/// <summary>
/// Calculates the bounding box of all symbols in the given list, adjusting for rotation and scale.
/// </summary>
/// <param name="symbols">A list of selected symbols to calculate the bounding box for. Each symbol must have a position and dimensions.</param>
/// <returns>A BoundingBox structure representing the smallest rectangle that contains all the symbols, accounting for their rotation and scale. The BoundingBox is defined by its top-left corner (X, Y), width (W), and height (H).</returns>
/// <exception cref="System.Exception">Throws an exception if the symbol list is empty, indicating that a bounding box cannot be calculated.</exception>
/// <remarks>
/// The function iterates through all symbols in the list to find the extreme values (min and max) for both X and Y coordinates, adjusted by each symbol's width and height post-rotation and scale. This approach ensures the bounding box accurately represents the spatial extent of all symbols, including any transformations they have undergone.
/// </remarks>
let getBlock (symbols: Symbol list) : BoundingBox =
    if List.isEmpty symbols then
        failwith "[ROTATESCALE - getBlock] : Symbol list is empty."
    else
        let firstSymbol = List.head symbols
        let initialWidth, initialHeight = getRotatedHAndW firstSymbol
        let initialMaxX = firstSymbol.Pos.X + initialWidth
        let initialMinX = firstSymbol.Pos.X
        let initialMaxY = firstSymbol.Pos.Y + initialHeight
        let initialMinY = firstSymbol.Pos.Y

        let minX, maxX, minY, maxY =
            symbols
            |> List.fold
                (fun (minX, maxX, minY, maxY) symbol ->
                    let width, height = getRotatedHAndW symbol
                    let symbolMaxX = symbol.Pos.X + width
                    let symbolMaxY = symbol.Pos.Y + height

                    (min minX symbol.Pos.X, max maxX symbolMaxX, min minY symbol.Pos.Y, max maxY symbolMaxY))
                (initialMinX, initialMaxX, initialMinY, initialMaxY)

        { TopLeft = { X = minX; Y = minY }; W = maxX - minX; H = maxY - minY }

/// <summary>
/// Rotates a point around the center of a block by the specified rotation angle.
/// </summary>
/// <param name="point">The point to rotate, original XYPos.</param>
/// <param name="centre">The center point of the block around which to rotate.</param>
/// <param name="rotation">The rotation angle (Degree0, Degree90, Degree180, Degree270).</param>
/// <returns>The rotated point.</returns>
let rotatePointAboutCentre (point: XYPos) (centre: XYPos) (rotation: Rotation) : XYPos =
    let deltaX = point.X - centre.X
    let deltaY = point.Y - centre.Y

    let newX, newY =
        match rotation with
        | Degree0 -> (deltaX, deltaY)
        | Degree90 -> (-deltaY, deltaX)
        | Degree180 -> (-deltaX, -deltaY)
        | Degree270 -> (deltaY, -deltaX)
    { X = centre.X - newX; Y = centre.Y - newY }

/// <summary>
/// Flips a point about a specified center point either horizontally or vertically.
/// </summary>
/// <param name="point">The point to flip.</param>
/// <param name="centre">The center point around which to flip.</param>
/// <param name="flipType">Specifies the direction of the flip (horizontal or vertical).</param>
/// <returns>The flipped point.</returns>
let flipPointAboutCentre (point: XYPos) (centre: XYPos) (flipType: FlipType) =
    match flipType with
    | FlipHorizontal -> { X = 2.0 * centre.X - point.X; Y = point.Y }
    | FlipVertical -> { X = point.X; Y = 2.0 * centre.Y - point.Y }

/// <summary>
/// Calculates the new top-left position of a symbol after rotation.
/// </summary>
/// <param name="rotation">The rotation applied to the symbol (CW or CCW).</param>
/// <param name="height">The original height of the symbol before rotation.</param>
/// <param name="width">The original width of the symbol before rotation.</param>
/// <param name="position">The original top-left position of the symbol.</param>
/// <returns>The new top-left position of the symbol after rotation.</returns>
let calculateNewTopLeftAfterRotation (rotation: Rotation) (height: float) (width: float) (position: XYPos) : XYPos =
    match rotation with
    | Degree0 -> position
    | Degree90 -> { X = position.X - height; Y = position.Y }
    | Degree180 -> { X = position.X - width; Y = position.Y + height }
    | Degree270 -> { X = position.X; Y = position.Y - width }

/// <summary>
/// Calculates the new top-left position of a symbol after applying a flip transformation.
/// </summary>
/// <param name="flipType">The type of flip to apply (horizontal or vertical).</param>
/// <param name="height">The height of the symbol before the flip.</param>
/// <param name="width">The width of the symbol before the flip.</param>
/// <param name="pos">The original top-left position of the symbol.</param>
/// <returns>The new top-left position of the symbol after the specified flip.</returns>
let calculateNewTopLeftAfterFlip (flipType: FlipType) (height: float) (width: float) (pos: XYPos) : XYPos =
    match flipType with
    | FlipHorizontal -> { X = pos.X - width; Y = pos.Y }
    | FlipVertical -> { X = pos.X; Y = pos.Y - height }

// ############################################################################################################
//                                      ----- Ezra's refactoring above -----
// ############################################################################################################

/// <summary>HLP 23: AUTHOR Ismagilov - Rotate a symbol in its block.</summary>
/// <param name="rotation">  Clockwise or Anticlockwise rotation</param>
/// <param name="block"> Bounding box of selected components</param>
/// <param name="sym"> Symbol to be rotated</param>
/// <returns>New symbol after rotated about block centre.</returns>
let rotateSymbolInBlock (rotation: Rotation) (blockCentre: XYPos) (sym: Symbol) : Symbol =

    let h, w = getRotatedHAndW sym

    let newTopLeft =
        rotatePointAboutCentre sym.Pos blockCentre (invertRotation rotation)
        |> calculateNewTopLeftAfterRotation (invertRotation rotation) h w

    let newComponent = { sym.Component with X = newTopLeft.X; Y = newTopLeft.Y }

    let newSTransform =
        match sym.STransform.Flipped with
        | true ->
            { sym.STransform with
                Rotation = combineRotation (invertRotation rotation) sym.STransform.Rotation }
        | _ ->
            { sym.STransform with
                Rotation = combineRotation rotation sym.STransform.Rotation }

    { sym with
        Pos = newTopLeft
        PortMaps = rotatePortInfo rotation sym.PortMaps
        STransform = newSTransform
        LabelHasDefaultPos = true
        Component = newComponent }
    |> calcLabelBoundingBox

/// <summary>HLP 23: AUTHOR Ismagilov - Flip a symbol horizontally or vertically in its block.</summary>
/// <param name="flip">  Flip horizontally or vertically</param>
/// <param name="block"> Bounding box of selected components</param>
/// <param name="sym"> Symbol to be flipped</param>
/// <returns>New symbol after flipped about block centre.</returns>
let flipSymbolInBlock (flip: FlipType) (blockCentre: XYPos) (sym: Symbol) : Symbol =

    let h, w = getRotatedHAndW sym
    //Needed as new symbols and their components need their Pos updated (not done in regular flip symbol)
    let newTopLeft =
        flipPointAboutCentre sym.Pos blockCentre flip
        |> calculateNewTopLeftAfterFlip flip h w

    let portOrientation =
        sym.PortMaps.Orientation
        |> Map.map (fun id side -> flipSideHorizontal side)

    let flipPortList currPortOrder side =
        currPortOrder
        |> Map.add (flipSideHorizontal side) sym.PortMaps.Order[side]

    let portOrder =
        (Map.empty, [ Edge.Top; Edge.Left; Edge.Bottom; Edge.Right ])
        ||> List.fold flipPortList
        |> Map.map (fun edge order -> List.rev order)

    let newSTransform =
        { Flipped = not sym.STransform.Flipped; Rotation = sym.STransform.Rotation }

    let newcomponent = { sym.Component with X = newTopLeft.X; Y = newTopLeft.Y }

    { sym with
        Component = newcomponent
        PortMaps = { Order = portOrder; Orientation = portOrientation }
        STransform = newSTransform
        LabelHasDefaultPos = true
        Pos = newTopLeft }
    |> calcLabelBoundingBox
    |> (fun sym ->
        match flip with
        | FlipHorizontal -> sym
        | FlipVertical ->
            let newblock = getBlock [ sym ]
            let newblockCenter = newblock.Centre()
            sym
            |> rotateSymbolInBlock Degree270 newblockCenter
            |> rotateSymbolInBlock Degree270 newblockCenter)

/// <summary>HLP 23: AUTHOR Ismagilov - Scales selected symbol up or down.</summary>
/// <param name="scaleType"> Scale up or down. Scaling distance is constant</param>
/// <param name="block"> Bounding box of selected components</param>
/// <param name="sym"> Symbol to be rotated</param>
/// <returns>New symbol after scaled about block centre.</returns>
let scaleSymbolInBlock
    //(Mag: float)
    (scaleType: ScaleType)
    (block: BoundingBox)
    (sym: Symbol)
    : Symbol
    =

    let symCenter = getRotatedSymbolCentre sym

    //Get x and y proportion of symbol to block
    let xProp, yProp =
        (symCenter.X - block.TopLeft.X) / block.W, (symCenter.Y - block.TopLeft.Y) / block.H

    let newCenter =
        match scaleType with
        | ScaleUp ->
            { X = (block.TopLeft.X - 5.) + ((block.W + 10.) * xProp)
              Y = (block.TopLeft.Y - 5.) + ((block.H + 10.) * yProp) }
        | ScaleDown ->
            { X = (block.TopLeft.X + 5.) + ((block.W - 10.) * xProp)
              Y = (block.TopLeft.Y + 5.) + ((block.H - 10.) * yProp) }

    let h, w = getRotatedHAndW sym
    let newPos = { X = (newCenter.X) - w / 2.; Y = (newCenter.Y) - h / 2. }
    let newComponent = { sym.Component with X = newPos.X; Y = newPos.Y }

    { sym with Pos = newPos; Component = newComponent; LabelHasDefaultPos = true }

/// HLP 23: AUTHOR Klapper - Rotates a symbol based on a degree, including: ports and component parameters.

let rotateSymbolByDegree (degree: Rotation) (sym: Symbol) =
    let pos =
        { X = sym.Component.X + sym.Component.W / 2.0
          Y = sym.Component.Y + sym.Component.H / 2.0 }
    match degree with
    | Degree0 -> sym
    | _ -> rotateSymbolInBlock degree pos sym

/// <summary>HLP 23: AUTHOR Ismagilov - Rotates a block of symbols, returning the new symbol model</summary>
/// <param name="compList"> List of ComponentId's of selected components</param>
/// <param name="model"> Current symbol model</param>
/// <param name="rotation"> Type of rotation to do</param>
/// <returns>New rotated symbol model</returns>
let rotateBlock (compList: ComponentId list) (model: SymbolT.Model) (rotation: Rotation) =

    printfn "running rotateBlock"
    let SelectedSymbols = List.map (fun x -> model.Symbols |> Map.find x) compList
    let UnselectedSymbols =
        model.Symbols
        |> Map.filter (fun x _ -> not (List.contains x compList))

    //Get block properties of selected symbols
    let block = getBlock SelectedSymbols

    //Rotated symbols about the center
    let newSymbols =
        List.map (fun x -> rotateSymbolInBlock (invertRotation rotation) (block.Centre()) x) SelectedSymbols

    //return model with block of rotated selected symbols, and unselected symbols
    { model with
        Symbols =
            ((Map.ofList (List.map2 (fun x y -> (x, y)) compList newSymbols)
              |> Map.fold (fun acc k v -> Map.add k v acc) UnselectedSymbols)) }

let oneCompBoundsBothEdges (selectedSymbols: Symbol list) =
    let maxXSymCentre =
        selectedSymbols
        |> List.maxBy (fun (x: Symbol) -> x.Pos.X + snd (getRotatedHAndW x))
        |> getRotatedSymbolCentre
    let minXSymCentre =
        selectedSymbols
        |> List.minBy (fun (x: Symbol) -> x.Pos.X)
        |> getRotatedSymbolCentre
    let maxYSymCentre =
        selectedSymbols
        |> List.maxBy (fun (y: Symbol) -> y.Pos.Y + fst (getRotatedHAndW y))
        |> getRotatedSymbolCentre
    let minYSymCentre =
        selectedSymbols
        |> List.minBy (fun (y: Symbol) -> y.Pos.Y)
        |> getRotatedSymbolCentre
    (maxXSymCentre.X = minXSymCentre.X)
    || (maxYSymCentre.Y = minYSymCentre.Y)

let findSelectedSymbols (compList: ComponentId list) (model: SymbolT.Model) =
    List.map (fun x -> model.Symbols |> Map.find x) compList

let getScalingFactorAndOffsetCentre (min: float) (matchMin: float) (max: float) (matchMax: float) =
    let scaleFact =
        if min = max || matchMax <= matchMin then
            1.
        else
            (matchMin - matchMax) / (min - max)
    let offsetC =
        if scaleFact = 1. then
            0.
        else
            (matchMin - min * scaleFact) / (1. - scaleFact)
    (scaleFact, offsetC)

/// Return set of floats that define how a group of components is scaled
let getScalingFactorAndOffsetCentreGroup
    (matchBBMin: XYPos)
    (matchBBMax: XYPos)
    (selectedSymbols: Symbol list)
    : ((float * float) * (float * float))
    =
    //(compList: ComponentId list)
    //(model: SymbolT.Model)

    //let selectedSymbols = List.map (fun x -> model.Symbols |> Map.find x) compList

    let maxXSym =
        selectedSymbols
        |> List.maxBy (fun (x: Symbol) -> x.Pos.X + snd (getRotatedHAndW x))

    let oldMaxX = (maxXSym |> getRotatedSymbolCentre).X
    let newMaxX =
        matchBBMax.X
        - (snd (getRotatedHAndW maxXSym)) / 2.

    let minXSym =
        selectedSymbols
        |> List.minBy (fun (x: Symbol) -> x.Pos.X)

    let oldMinX = (minXSym |> getRotatedSymbolCentre).X
    let newMinX =
        matchBBMin.X
        + (snd (getRotatedHAndW minXSym)) / 2.

    let maxYSym =
        selectedSymbols
        |> List.maxBy (fun (y: Symbol) -> y.Pos.Y + fst (getRotatedHAndW y))

    let oldMaxY = (maxYSym |> getRotatedSymbolCentre).Y
    let newMaxY =
        matchBBMax.Y
        - (fst (getRotatedHAndW maxYSym)) / 2.

    let minYSym =
        selectedSymbols
        |> List.minBy (fun (y: Symbol) -> y.Pos.Y)

    let oldMinY = (minYSym |> getRotatedSymbolCentre).Y
    let newMinY =
        matchBBMin.Y
        + (fst (getRotatedHAndW minYSym)) / 2.

    let xSC = getScalingFactorAndOffsetCentre oldMinX newMinX oldMaxX newMaxX
    let ySC = getScalingFactorAndOffsetCentre oldMinY newMinY oldMaxY newMaxY
    (xSC, ySC)

/// Alter position of one symbol as needed in a scaling operation
let scaleSymbol (xYSC: (float * float) * (float * float)) (sym: Symbol) : Symbol =
    let symCentre = getRotatedSymbolCentre sym
    let translateFunc scaleFact offsetC coordinate =
        (coordinate - offsetC) * scaleFact + offsetC
    let xSC = fst xYSC
    let ySC = snd xYSC
    let newX = translateFunc (fst xSC) (snd xSC) symCentre.X
    let newY = translateFunc (fst ySC) (snd ySC) symCentre.Y

    let symCentreOffsetFromTopLeft =
        { X = (snd (getRotatedHAndW sym)) / 2.; Y = (fst (getRotatedHAndW sym)) / 2. }
    let newTopLeftPos =
        { X = newX; Y = newY }
        - symCentreOffsetFromTopLeft
    let newComp = { sym.Component with X = newTopLeftPos.X; Y = newTopLeftPos.Y }

    { sym with Pos = newTopLeftPos; Component = newComp; LabelHasDefaultPos = true }

/// Part of the rotate and scale code
let groupNewSelectedSymsModel
    (compList: ComponentId list)
    (model: SymbolT.Model)
    (selectedSymbols: Symbol list)
    (modifySymbolFunc)
    =

    //let SelectedSymbols = List.map (fun x -> model.Symbols |> Map.find x) compList
    let UnselectedSymbols =
        model.Symbols
        |> Map.filter (fun x _ -> not (List.contains x compList))

    // let block = getBlock SelectedSymbols
    // printfn "bbCentreX:%A" (block.Centre()).X

    // let newSymbols = List.map (modifySymbolFunc (block.Centre())) SelectedSymbols
    let newSymbols = List.map (modifySymbolFunc) selectedSymbols

    { model with
        Symbols =
            ((Map.ofList (List.map2 (fun x y -> (x, y)) compList newSymbols)
              |> Map.fold (fun acc k v -> Map.add k v acc) UnselectedSymbols)) }

/// <summary>HLP 23: AUTHOR Ismagilov - Flips a block of symbols, returning the new symbol model</summary>
/// <param name="compList"> List of ComponentId's of selected components</param>
/// <param name="model"> Current symbol model</param>
/// <param name="flip"> Type of flip to do</param>
/// <returns>New flipped symbol model</returns>
let flipBlock (compList: ComponentId list) (model: SymbolT.Model) (flip: FlipType) =
    //Similar structure to rotateBlock, easy to understand
    let SelectedSymbols = List.map (fun x -> model.Symbols |> Map.find x) compList
    let UnselectedSymbols =
        model.Symbols
        |> Map.filter (fun x _ -> not (List.contains x compList))

    let block = getBlock SelectedSymbols

    let newSymbols =
        List.map (fun x -> flipSymbolInBlock flip (block.Centre()) x) SelectedSymbols

    { model with
        Symbols =
            ((Map.ofList (List.map2 (fun x y -> (x, y)) compList newSymbols)
              |> Map.fold (fun acc k v -> Map.add k v acc) UnselectedSymbols)) }

/// After every model update this updates the "scaling box" part of the model to be correctly
/// displayed based on whetehr multiple components are selected and if so what is their "box"
/// In addition to changing the model directly, cmd may contain messages that make further changes.
let postUpdateScalingBox (model: SheetT.Model, cmd) =

    let symbolCmd (msg: SymbolT.Msg) =
        Elmish.Cmd.ofMsg (ModelType.Msg.Sheet(SheetT.Wire(BusWireT.Symbol msg)))
    let sheetCmd (msg: SheetT.Msg) =
        Elmish.Cmd.ofMsg (ModelType.Msg.Sheet msg)

    if (model.SelectedComponents.Length < 2) then
        match model.ScalingBox with
        | None -> model, cmd
        | _ ->
            { model with ScalingBox = None },
            [ symbolCmd (SymbolT.DeleteSymbols (model.ScalingBox.Value).ButtonList)
              sheetCmd SheetT.UpdateBoundingBoxes ]
            |> List.append [ cmd ]
            |> Elmish.Cmd.batch
    else
        let newBoxBound =
            model.SelectedComponents
            |> List.map (fun id -> Map.find id model.Wire.Symbol.Symbols)
            |> getBlock
        match model.ScalingBox with
        | Some value when value.ScalingBoxBound = newBoxBound -> model, cmd
        | _ ->
            let topleft = newBoxBound.TopLeft
            let rotateDeg90OffSet: XYPos =
                { X = newBoxBound.W + 57.; Y = (newBoxBound.H / 2.) - 12.5 }
            let rotateDeg270OffSet: XYPos = { X = -69.5; Y = (newBoxBound.H / 2.) - 12.5 }
            let buttonOffSet: XYPos = { X = newBoxBound.W + 47.5; Y = -47.5 }
            let dummyPos = (topleft + buttonOffSet)

            let makeButton = SymbolUpdate.createAnnotation ThemeType.Colourful
            let buttonSym =
                { makeButton ScaleButton dummyPos with Pos = (topleft + buttonOffSet) }
            let makeRotateSym sym =
                { sym with Component = { sym.Component with H = 25.; W = 25. } }
            let rotateDeg90Sym =
                makeButton (RotateButton Degree90) (topleft + rotateDeg90OffSet)
                |> makeRotateSym
            let rotateDeg270Sym =
                { makeButton (RotateButton Degree270) (topleft + rotateDeg270OffSet) with
                    SymbolT.STransform = { Rotation = Degree90; Flipped = false } }
                |> makeRotateSym

            let newSymbolMap =
                model.Wire.Symbol.Symbols
                |> Map.add buttonSym.Id buttonSym
                |> Map.add rotateDeg270Sym.Id rotateDeg270Sym
                |> Map.add rotateDeg90Sym.Id rotateDeg90Sym
            let initScalingBox: SheetT.ScalingBox =
                { ScalingBoxBound = newBoxBound
                  ScaleButton = buttonSym
                  RotateDeg90Button = rotateDeg90Sym
                  RotateDeg270Button = rotateDeg270Sym
                  ButtonList = [ buttonSym.Id; rotateDeg270Sym.Id; rotateDeg90Sym.Id ] }
            let newCmd =
                match model.ScalingBox with
                | Some _ ->
                    [ symbolCmd (SymbolT.DeleteSymbols (model.ScalingBox.Value).ButtonList)
                      sheetCmd SheetT.UpdateBoundingBoxes ]
                    |> List.append [ cmd ]
                    |> Elmish.Cmd.batch
                | None -> cmd
            model
            |> Optic.set SheetT.scalingBox_ (Some initScalingBox)
            |> Optic.set SheetT.symbols_ newSymbolMap,
            newCmd
