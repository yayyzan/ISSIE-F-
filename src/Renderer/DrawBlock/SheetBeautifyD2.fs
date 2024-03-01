module TestD2
(*
    This module updates the Tick3 testing setup for Deliverable 2. It tests circuit layouts by:
    - Flipping and rotating gates and MUXes randomly.
    - Swapping inputs on 2-MUX symbols randomly.
    - Counting wire crossings and straightened wires to check the layout.

    Functions included are: 
    - randomRotation: returns a random rotation for a symbol
    - randomFlipType: returns a random flip type for a symbol
    - shufflePortMapsOrder: shuffles the order of ports on relevant edge of a component's port maps
    - updateMux2PortOrder: updates the order of ports for all Mux2 type symbols in a model, shuffling the port orders
    - checkWireCrossing: checks if two wire segments intersect by evaluating their horizontal and vertical overlap
    - countCrossingsInSheet: counts the number of wire crossings in a given sheet and reports if any are found
    - countWireStraightInSheet: counts the number of straightened wire segments within a given sheet model
    - functionalShuffle: shuffles a list of items into a random order
    - placeAndOrientSymbol: places and orients a symbol on the sheet based on specified position, rotation, and flip type
    - baseCircuit1: creates a base circuit with an AND gate and a MUX2
    - baseCircuit2: creates a base circuit with an OR gate and a MUX2
    - testWireCrossingD2: runs a test with manually generated circuits and checks for wire crossings
    - testWireStraightD2: runs a test with manually generated circuits and checks for wires straightenedhelp place symbols with specific rotations and flips, shuffle ports, and create test circuits. 
    
    The tests check for wire crossings and straight wires to see how these changes affect the layout.
*)

open GenerateData
open Elmish

module TestLib =

    /// convenience unsafe function to extract Ok part of Result or fail if value is Error
    let getOkOrFail (res: Result<'a, string>) =
        match res with
        | Ok x -> x
        | Error mess -> failwithf "%s" mess

    type TestStatus =
        | Fail of string
        | Exception of string

    type Test<'a> =
        {
            Name: string
            Samples: Gen<'a>
            StartFrom: int
            /// The 1st argument is the test number: allows assertions that fail on a specific sample
            /// to display just one sample.
            /// The return value is None if test passes, or Some message if it fails.
            Assertion: int -> 'a -> string option
        }

    type TestResult<'a> =
        { TestName: string
          TestData: Gen<'a>
          FirstSampleTested: int
          TestErrors: (int * TestStatus) list }

    let catchException name func arg =
        try
            Ok(func arg)
        with e ->
            Error($"Exception when running {name}\n" + e.StackTrace)

    /// Run the Test samples from 0 up to test.Size - 1.
    /// The return list contains all failures or exceptions: empty list => everything has passed.
    /// This will always run to completion: use truncate if text.Samples.Size is too large.
    let runTests (test: Test<'a>) : TestResult<'a> =
        [ test.StartFrom .. test.Samples.Size - 1 ]
        |> List.map (fun n ->
            catchException $"generating test {n} from {test.Name}" test.Samples.Data n
            |> (fun res -> n, res))
        |> List.collect (function
            | n, Error mess -> [ n, Exception mess ]
            | n, Ok sample ->
                match catchException $"'test.Assertion' on test {n} from 'runTests'" (test.Assertion n) sample with
                | Ok None -> []
                | Ok(Some failure) -> [ n, Fail failure ]
                | Error(mess) -> [ n, Exception mess ])
        |> (fun resL ->
            { TestName = test.Name
              FirstSampleTested = test.StartFrom
              TestData = test.Samples
              TestErrors = resL })

(******************************************************************************************
   This submodule contains a set of functions that enable random data generation
   for property-based testing of Draw Block wire routing functions.
   basic idea.
   1. Generate, in various ways, random circuit layouts
   2. For each layout apply smartautoroute to regenerate all wires
   3. Apply check functions to see if the resulting wire routing obeys "good layout" rules.
   4. Output any layouts with anomalous wire routing
*******************************************************************************************)
module StarterD2T =
    open EEExtensions
    open Optics
    open Optics.Operators
    open Helpers
    open CommonTypes
    open ModelType
    open DrawModelType
    open Sheet.SheetInterface
    open TestLib
    open BlockHelpers

    /// create an initial empty Sheet Model
    let initSheetModel = DiagramMainView.init().Sheet

    /// Optic to access SheetT.Model from Issie Model
    let sheetModel_ = sheet_

    /// Optic to access BusWireT.Model from SheetT.Model
    let busWireModel_ = SheetT.wire_

    /// Optic to access SymbolT.Model from SheetT.Model
    let symbolModel_ = SheetT.symbol_

    /// allowed max X or y coord of svg canvas
    let maxSheetCoord = Sheet.Constants.defaultCanvasSize
    let middleOfSheet = { X = maxSheetCoord / 2.; Y = maxSheetCoord / 2. }

    /// Used throughout to compare labels since these are case invariant "g1" = "G1"
    let caseInvariantEqual str1 str2 =
        String.toUpper str1 = String.toUpper str2

    /// Identify a port from its component label and number.
    /// Usually both an input and output port will mathc this, so
    /// the port is only unique if it is known to be input or output.
    /// used to specify the ends of wires, since tehee are known to be
    /// connected to outputs (source) or inputs (target).
    type SymbolPort = { Label: string; PortNumber: int }

    /// convenience function to make SymbolPorts
    let portOf (label: string) (number: int) = { Label = label; PortNumber = number }

    //-----------------------------------------------------------------------------------------------
    // visibleSegments is included here as ahelper for info, and because it is needed in project work
    //-----------------------------------------------------------------------------------------------

    /// The visible segments of a wire, as a list of vectors, from source end to target end.
    /// Note that in a wire with n segments a zero length (invisible) segment at any index [1..n-2] is allowed
    /// which if present causes the two segments on either side of it to coalesce into a single visible segment.
    /// A wire can have any number of visible segments - even 1.
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

    //------------------------------------------------------------------------------------------------------------------------//
    //------------------------------functions to build issue schematics programmatically--------------------------------------//
    //------------------------------------------------------------------------------------------------------------------------//
    module Builder =
        let placeSymbol
            (symLabel: string)
            (compType: ComponentType)
            (position: XYPos)
            (model: SheetT.Model)
            : Result<SheetT.Model, string>
            =
            let symLabel = String.toUpper symLabel // make label into its standard casing
            let symModel, symId =
                SymbolUpdate.addSymbol [] (model.Wire.Symbol) position compType symLabel
            let sym = symModel.Symbols[symId]
            match position + sym.getScaledDiagonal with
            | { X = x; Y = y } when x > maxSheetCoord || y > maxSheetCoord ->
                Error
                    $"symbol '{symLabel}' position {position + sym.getScaledDiagonal} lies outside allowed coordinates"
            | _ ->
                model
                |> Optic.set symbolModel_ symModel
                |> SheetUpdateHelpers.updateBoundingBoxes // could optimise this by only updating symId bounding boxes
                |> Ok

        /// <summary>
        /// Places and orients a symbol on the sheet based on specified position, rotation, and flip type.
        /// </summary>
        /// <param name="symLabel">The label of the symbol to place.</param>
        /// <param name="compType">The component type of the symbol.</param>
        /// <param name="position">The position where the symbol should be placed.</param>
        /// <param name="rotation">The rotation to apply to the symbol.</param>
        /// <param name="flip">The flip type to apply to the symbol.</param>
        /// <param name="model">The current sheet model.</param>
        /// <returns>
        /// A Result containing the updated sheet model if successful, or an error string if the symbol could not be placed due to overlap or out-of-bounds position.
        /// </returns>
        let placeAndOrientSymbol
            (symLabel: string)
            (compType: ComponentType)
            (position: XYPos)
            (rotation: Rotation)
            (flip: SymbolT.FlipType)
            (model: SheetT.Model)
            : Result<SheetT.Model, string>
            =
            let symLabelUpper = String.toUpper symLabel
            match SymbolUpdate.addSymbol [] (model.Wire.Symbol) position compType symLabelUpper with
            | symModel, symId ->
                let sym = symModel.Symbols[symId]
                let pos =
                    { X = sym.Component.X + sym.Component.W / 2.0
                      Y = sym.Component.Y + sym.Component.H / 2.0 }
                match position + sym.getScaledDiagonal with
                | { X = x; Y = y } when x > maxSheetCoord || y > maxSheetCoord ->
                    Error
                        $"symbol '{symLabelUpper}' position {position + sym.getScaledDiagonal} lies outside allowed coordinates"
                | _ ->
                    // printfn "[DEBUG]TestDrawBlocks/placeAndOrientSymbol -- TESTING ROTATION"
                    // printfn $"[DEBUG]TestDrawBlocks/placeAndOrientSymbol -- Rotation: {rotation}"
                    let rotatedSym = RotateScale.rotateSymbolByDegree rotation sym
                    let orientedSym = RotateScale.flipSymbolInBlock flip pos rotatedSym
                    let updatedSymModel =
                        { symModel with Symbols = Map.add symId orientedSym symModel.Symbols }
                    let updatedModel =
                        model
                        |> Optic.set symbolModel_ updatedSymModel
                        |> SheetUpdateHelpers.updateBoundingBoxes
                    Ok updatedModel
            | _, _ -> failwithf "placeAndOrientSymbol: addSymbol failed -- Never happens"

        /// Place a new symbol onto the Sheet with given position and scaling (use default scale if this is not specified).
        /// The ports on the new symbol will be determined by the input and output components on some existing sheet in project.
        /// Return error if symLabel is not unique on sheet, or ccSheetName is not the name of some other sheet in project.
        let placeCustomSymbol
            (symLabel: string)
            (ccSheetName: string)
            (project: Project)
            (scale: XYPos)
            (position: XYPos)
            (model: SheetT.Model)
            : Result<SheetT.Model, string>
            =
            let symbolMap = model.Wire.Symbol.Symbols
            if caseInvariantEqual ccSheetName project.OpenFileName then
                Error "Can't create custom component with name same as current opened sheet"
            elif
                not
                <| List.exists
                    (fun (ldc: LoadedComponent) -> caseInvariantEqual ldc.Name ccSheetName)
                    project.LoadedComponents
            then
                Error "Can't create custom component unless a sheet already exists with smae name as ccSheetName"
            elif
                symbolMap
                |> Map.exists (fun _ sym -> caseInvariantEqual sym.Component.Label symLabel)
            then
                Error "Can't create custom component with duplicate Label"
            else
                let canvas = model.GetCanvasState()
                let ccType: CustomComponentType =
                    { Name = ccSheetName
                      InputLabels = Extractor.getOrderedCompLabels (Input1(0, None)) canvas
                      OutputLabels = Extractor.getOrderedCompLabels (Output 0) canvas
                      Form = None
                      Description = None }
                placeSymbol symLabel (Custom ccType) position model

        /// Add a (newly routed) wire, source specifies the Output port, target the Input port.
        /// Return an error if either of the two ports specified is invalid, or if the wire duplicates and existing one.
        /// The wire created will be smart routed but not separated from other wires: for a nice schematic
        /// separateAllWires should be run after  all wires are added.
        /// source, target: respectively the output port and input port to which the wire connects.
        let placeWire (source: SymbolPort) (target: SymbolPort) (model: SheetT.Model) : Result<SheetT.Model, string> =
            let symbols = model.Wire.Symbol.Symbols
            let getPortId (portType: PortType) symPort =
                mapValues symbols
                |> Array.tryFind (fun sym -> caseInvariantEqual sym.Component.Label symPort.Label)
                |> function
                    | Some x -> Ok x
                    | None -> Error "Can't find symbol with label '{symPort.Label}'"
                |> Result.bind (fun sym ->
                    match portType with
                    | PortType.Input -> List.tryItem symPort.PortNumber sym.Component.InputPorts
                    | PortType.Output -> List.tryItem symPort.PortNumber sym.Component.OutputPorts
                    |> function
                        | Some port -> Ok port.Id
                        | None -> Error $"Can't find {portType} port {symPort.PortNumber} on component {symPort.Label}")

            match getPortId PortType.Input target, getPortId PortType.Output source with
            | Error e, _
            | _, Error e -> Error e
            | Ok inPort, Ok outPort ->
                let newWire =
                    BusWireUpdate.makeNewWire (InputPortId inPort) (OutputPortId outPort) model.Wire
                if
                    model.Wire.Wires
                    |> Map.exists (fun wid wire ->
                        wire.InputPort = newWire.InputPort
                        && wire.OutputPort = newWire.OutputPort)
                then
                    // wire already exists
                    Error
                        "Can't create wire from {source} to {target} because a wire already exists between those ports"
                else
                    model
                    |> Optic.set (busWireModel_ >-> BusWireT.wireOf_ newWire.WId) newWire
                    |> Ok

        /// Run the global wire separation algorithm (should be after all wires have been placed and routed)
        let separateAllWires (model: SheetT.Model) : SheetT.Model =
            model
            |> Optic.map
                busWireModel_
                (BusWireSeparate.updateWireSegmentJumpsAndSeparations (model.Wire.Wires.Keys |> Seq.toList))

        /// Copy testModel into the main Issie Sheet making its contents visible
        let showSheetInIssieSchematic (testModel: SheetT.Model) (dispatch: Dispatch<Msg>) =
            let sheetDispatch sMsg = dispatch (Sheet sMsg)
            dispatch
            <| UpdateModel(Optic.set sheet_ testModel) // set the Sheet component of the Issie model to make a new schematic.
            sheetDispatch <| SheetT.KeyPress SheetT.CtrlW // Centre & scale the schematic to make all components viewable.

        /// 1. Create a set of circuits from Gen<'a> samples by applying sheetMaker to each sample.
        /// 2. Check each ciruit with sheetChecker.
        /// 3. Return a TestResult record with errors those samples for which sheetChecker returns false,
        /// or where there is an exception.
        /// If there are any test errors display the first in Issie, and its error message on the console.
        /// sheetMaker: generates a SheetT.model from the random sample
        /// sheetChecker n model: n is sample number, model is the genrated model. Return false if test fails.
        let runTestOnSheets
            (name: string)
            (sampleToStartFrom: int)
            (samples: Gen<'a>)
            (sheetMaker: 'a -> SheetT.Model)
            (sheetChecker: int -> SheetT.Model -> string option)
            (dispatch: Dispatch<Msg>)
            : TestResult<'a>
            =
            let generateAndCheckSheet n = sheetMaker >> sheetChecker n
            let result =
                { Name = name
                  Samples = samples
                  StartFrom = sampleToStartFrom
                  Assertion = generateAndCheckSheet }
                |> runTests
            match result.TestErrors with
            | [] -> // no errors
                printf $"Test {result.TestName} has PASSED."
            | (n, first) :: _ -> // display in Issie editor and print out first error
                printf $"Test {result.TestName} has FAILED on sample {n} with error message:\n{first}"
                match catchException "" sheetMaker (samples.Data n) with
                | Ok sheet -> showSheetInIssieSchematic sheet dispatch
                | Error mess -> ()
            result

    //------------------------------------------------------------------------------------------------//
    //-------------------------Example assertions used to test sheets---------------------------------//
    //------------------------------------------------------------------------------------------------//

    module Asserts =
        open DrawModelType.SymbolT
        open DrawModelType.SheetT
        open Operators
        open System

        (* Each assertion function from this module has as inputs the sample number of the current test and the corresponding schematic sheet.
           It returns a boolean indicating (true) that the test passes or 9false) that the test fails. The sample numbr is included to make it
           easy to document tests and so that any specific sampel schematic can easily be displayed using failOnSampleNumber. *)

        /// Ignore sheet and fail on the specified sample, useful for displaying a given sample
        let failOnSampleNumber (sampleToFail: int) (sample: int) _sheet =
            if sampleToFail = sample then
                Some $"Failing forced on Sample {sampleToFail}."
            else
                None

        /// Fails all tests: useful to show in sequence all the sheets generated in a test
        let failOnAllTests (sample: int) _ = Some <| $"Sample {sample}"

        /// Fail when sheet contains a wire segment that overlaps (or goes too close to) a symbol outline
        let failOnWireIntersectsSymbol (sample: int) (sheet: SheetT.Model) =
            let wireModel = sheet.Wire
            wireModel.Wires
            |> Map.exists (fun _ wire ->
                BusWireRoute.findWireSymbolIntersections wireModel wire
                <> [])
            |> (function
            | true -> Some $"Wire intersects a symbol outline in Sample {sample}"
            | false -> None)

        let failOnSymbolIntersectsSymbol (sample: int) (sheet: SheetT.Model) =
            let wireModel = sheet.Wire
            let boxes =
                mapValues sheet.BoundingBoxes
                |> Array.toList
                |> List.mapi (fun n box -> n, box)
            List.allPairs boxes boxes
            |> List.exists (fun ((n1, box1), (n2, box2)) -> (n1 <> n2) && overlap2DBox box1 box2)
            |> (function
            | true -> Some $"Symbol outline intersects another symbol outline in Sample {sample}"
            | false -> None)

        /// <summary>
        /// Checks if two wire segments intersect by evaluating their horizontal and vertical overlap.
        /// </summary>
        /// <param name="seg1">The first wire segment to check for intersection.</param>
        /// <param name="seg2">The second wire segment to check for intersection.</param>
        /// <returns>
        /// True if the segments intersect; otherwise, false.
        /// </returns>
        let checkWireCrossing (seg1: BusWireT.ASegment) (seg2: BusWireT.ASegment) : bool =
            let horizontalOverlap =
                overlap1D (seg1.Start.X, seg1.End.X) (seg2.Start.X, seg2.End.X)
            let verticalOverlap =
                overlap1D (seg1.Start.Y, seg1.End.Y) (seg2.Start.Y, seg2.End.Y)
            horizontalOverlap && verticalOverlap

        /// <summary>
        /// Counts the number of wire crossings in a given sheet and reports if any are found.
        /// </summary>
        /// <param name="sample">The sample number being tested, for identification in test results.</param>
        /// <param name="sheet">The sheet model containing the wires to be analyzed for crossings.</param>
        /// <returns>
        /// A string option containing a message if crossings are found, specifying the number of crossings and the sample number; otherwise, None if no crossings are detected.
        /// </returns>
        /// <remarks>
        /// This function evaluates all wire segments within the sheet to determine if any intersect with others. It only counts intersections between different wires or non-adjacent segments of the same wire, ignoring overlaps of directly connected segments. The purpose is to identify potential schematic layout issues where wires cross each other, which could indicate design or routing inefficiencies.
        /// </remarks>
        let countCrossingsInSheet (sample: int) (sheet: SheetT.Model) : string option =
            let allWires = sheet.Wire.Wires |> Map.toList |> List.map snd
            let allAbsSegments = allWires |> List.collect getAbsSegments

            let segmentPairs = List.allPairs allAbsSegments allAbsSegments

            let crossings =
                segmentPairs
                |> List.filter (fun (seg1, seg2) ->
                    seg1.Segment.WireId <> seg2.Segment.WireId
                    || seg1.Segment.Index <> seg2.Segment.Index)
                |> List.filter (fun (seg1, seg2) -> checkWireCrossing seg1 seg2)

            let numberOfCrossings = crossings.Length / 2

            if numberOfCrossings > 0 then
                Some $"Sample {sample} has {numberOfCrossings} wire crossings"
            else
                None

        /// <summary>
        /// Determines the edge placement of ports on a symbol based on the symbol's rotation.
        /// </summary>
        /// <param name="rotation">The rotation of the symbol (Degree0, Degree90, Degree180, Degree270).</param>
        /// <returns>The edge (Left, Top, Right, Bottom) on which ports are placed given the symbol's rotation.</returns> 
        let edgeBasedOnRotation rotation =
            match rotation with
            | Degree0 -> Left
            | Degree90 -> Top
            | Degree180 -> Right
            | Degree270 -> Bottom

        /// <summary>
        /// Shuffles a list of items into a random order.
        /// </summary>
        /// <param name="list">The list of items to shuffle.</param>
        /// <returns>
        /// A new list containing the same items as the input list but in a random order.
        /// </returns>
        /// <remarks>
        /// This function applies a functional approach to shuffling. Each item in the input list is paired with a random number. The list is then sorted by these random numbers, effectively shuffling the items. Finally, the random numbers are discarded, leaving a shuffled list of the original items. This method ensures that the shuffling process is both stateless and deterministic, given a fixed seed for the random number generator.
        /// </remarks>
        let functionalShuffle list =
            let rng = new Random()
            list
            |> List.map (fun item -> (rng.Next(), item))
            |> List.sortBy fst
            |> List.map snd

        /// <summary>
        /// Shuffles the order of ports on specified edge of a component's port maps.
        /// </summary>
        /// <param name="portMaps">The port maps of a component, detailing the order and orientation of ports.</param>
        /// <param name="rotation">The rotation of the component, which determines the edge placement of ports.</param>
        /// <returns>
        /// A new PortMaps instance where the order of ports on specified edge is shuffled, while the orientation of ports remains unchanged.
        /// </returns>
        /// <remarks>
        /// This function targets each edge defined in the component's port maps and applies a shuffle to the list of ports on that edge.
        /// </remarks>
        let shufflePortMapsOrderOnRotationState (portMaps: PortMaps) (rotation: Rotation) : PortMaps =
            let relevantEdge = edgeBasedOnRotation rotation
            let shuffledOrder =
                portMaps.Order
                |> Map.map (fun edge portList ->
                    if edge = relevantEdge then functionalShuffle portList
                    else portList) // Only shuffle ports on the relevant edge
            { portMaps with Order = shuffledOrder }

        /// <summary>
        /// Updates the order of ports for all Mux2 type symbols in a model, shuffling the port orders on the edge containing the inputs.
        /// </summary>
        /// <param name="model">The current sheet model containing symbols and their port maps.</param>
        /// <returns>
        /// A new SheetT.Model instance with the port orders of all Mux2 symbols shuffled.
        /// </returns>
        let updateMux2PortOrder (model: SheetT.Model) : SheetT.Model =
            let symbols = model.Wire.Symbol.Symbols
            let updatedSymbols = 
                symbols
                |> Map.map (fun _ sym ->
                    match sym.Component.Type with
                    | Mux2 ->
                        let rotation = sym.STransform.Rotation // Assuming we have rotation info in sym.STransform
                        let shuffledPortMaps = shufflePortMapsOrderOnRotationState sym.PortMaps rotation
                        { sym with PortMaps = shuffledPortMaps }
                    | _ -> sym)

            let updatedSymbolModel = { model.Wire.Symbol with Symbols = updatedSymbols }
            { model with Wire = { model.Wire with Symbol = updatedSymbolModel } }

        /// <summary>
        /// Counts the number of straightened wire segments within a given sheet model.
        /// </summary>
        /// <param name="sample">The sample number being analyzed.</param>
        /// <param name="sheet">The sheet model containing wire data.</param>
        /// <returns>
        /// A string option containing a message about the number of straight wire segments in the sample if any exist; otherwise, None.
        /// </returns>
        /// <remarks>
        /// This function evaluates each wire segment in the sheet model to determine if it is straight (aligned either horizontally or vertically).
        /// </remarks>
        let countWireStraightInSheet (sample: int) (sheet: SheetT.Model) : string option =
            let isSegmentStraight (seg: BusWireT.ASegment) =
                seg.Start.X = seg.End.X || seg.Start.Y = seg.End.Y

            let allWires = sheet.Wire.Wires |> Map.toList |> List.map snd
            let allAbsSegments = allWires |> List.collect getAbsSegments

            let straightenedSegmentsCount =
                allAbsSegments
                |> List.filter isSegmentStraight
                |> List.length

            if straightenedSegmentsCount > 0 then
                Some $"Sample {sample} has {straightenedSegmentsCount} straightened wire segments."
            else
                None

    //--------------------------------------------------------------------------------------------------//
    //----------------------------------------Example Test Circuits using Gen<'a> samples---------------//
    //--------------------------------------------------------------------------------------------------//

    open Builder
    open Asserts
    /// Sample data based on 11 equidistant points on a horizontal line
    let horizLinePositions =
        fromList [ -100..20..100 ]
        |> map (fun n -> middleOfSheet + { X = float n; Y = 0. })

    let randomRotation () =
        let rotations = [| Degree0; Degree90; Degree180; Degree270 |]
        rotations.[random.Next(rotations.Length)]

    let randomFlipType () =
        let flips = [| SymbolT.FlipType.FlipHorizontal; SymbolT.FlipType.FlipVertical |]
        flips.[random.Next(flips.Length)]
    
    let baseCircuit1 (andPos: XYPos) =
        initSheetModel
        // The position of the AND gate is specified by hand, as requested in the deliverable.
        |> placeAndOrientSymbol "AND1" (GateN(And, 2)) { X = 100.0; Y = 100.0 } (Degree0) (randomFlipType ())
        |> Result.bind (placeAndOrientSymbol "MUX1" Mux2 { X = 200.0; Y = 100.0 } (Degree0) (randomFlipType ()))
        |> Result.bind (placeWire (portOf "AND1" 0) (portOf "MUX1" 0))
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "AND1" 0))
        |> Result.map updateMux2PortOrder
        |> getOkOrFail

    let baseCircuit2 (andPos: XYPos) =
        initSheetModel
        |> placeAndOrientSymbol "OR1" (GateN(And, 2)) { X = 100.0; Y = 100.0 } (Degree0) (randomFlipType ())
        |> Result.bind (placeAndOrientSymbol "MUX1" Mux2 { X = 200.0; Y = 100.0 } (Degree0) (randomFlipType ()))
        |> Result.bind (placeWire (portOf "OR1" 0) (portOf "MUX1" 0))
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "OR1" 0))
        |> Result.map updateMux2PortOrder
        |> getOkOrFail

    //---------------------------------------------------------------------------------------//
    //-----------------------------Demo tests on Draw Block code-----------------------------//
    //---------------------------------------------------------------------------------------//
    module Tests =
        let recordPositionInTest (testNumber: int) (dispatch: Dispatch<Msg>) (result: TestResult<'a>) =
            dispatch
            <| UpdateDrawBlockTestState(fun _ ->
                match result.TestErrors with
                | [] ->
                    printf "Test finished"
                    None
                | (numb, _) :: _ ->
                    printf $"Sample {numb}"
                    Some { LastTestNumber = testNumber; LastTestSampleIndex = numb })

        let testWireCrossingD2 testNum firstSample dispatch =
            runTestOnSheets
                "Test with manually generated circuits and check for wire crossings"
                firstSample
                horizLinePositions
                baseCircuit1
                countCrossingsInSheet
                dispatch
            |> recordPositionInTest testNum dispatch
        let testWireStraightD2 testNum firstSample dispatch =
            runTestOnSheets
                "Test with manually generated circuits and check for wires straightened"
                firstSample
                horizLinePositions
                baseCircuit2
                countWireStraightInSheet
                dispatch
            |> recordPositionInTest testNum dispatch

        /// List of tests available which can be run ftom Issie File Menu.
        /// The first 3 tests can also be run via Ctrl-n accelerator keys as shown on menu
        let testsToRunFromSheetMenu: (string * (int -> int -> Dispatch<Msg> -> Unit)) list =
            // Change names and test functions as required
            // delete unused tests from list
            [ "Test1", testWireCrossingD2
              "Test2", testWireStraightD2
              "Test3", (fun _ _ _ -> printf "Test8")
              "Next Test Error",
              fun _ _ _ -> printf "Next Error:" ] // Go to the nexterror in a test

        /// Display the next error in a previously started test
        let nextError (testName, testFunc) firstSampleToTest dispatch =
            let testNum =
                testsToRunFromSheetMenu
                |> List.tryFindIndex (fun (name, _) -> name = testName)
                |> Option.defaultValue 0
            testFunc testNum firstSampleToTest dispatch

        /// common function to execute any test.
        /// testIndex: index of test in testsToRunFromSheetMenu
        let testMenuFunc (testIndex: int) (dispatch: Dispatch<Msg>) (model: Model) =
            let name, func = testsToRunFromSheetMenu[testIndex]
            printf "%s" name
            match name, model.DrawBlockTestState with
            | "Next Test Error", Some state ->
                nextError testsToRunFromSheetMenu[state.LastTestNumber] (state.LastTestSampleIndex + 1) dispatch
            | "Next Test Error", None ->
                printf "Test Finished"
                ()
            | _ -> func testIndex 0 dispatch
