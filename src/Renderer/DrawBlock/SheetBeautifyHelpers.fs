module SheetBeautifyHelpers

open Optics
open DrawModelType
open CommonTypes
open Operators


//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team

// Function 1 : The dimensions of a custom component symbol
let getCustomComponenttDimensionB1R (customComponent : SymbolT.Symbol) : {|H:float;W:float|} =
    let comp = Optic.get SymbolT.component_ customComponent

    {|H=comp.H;W=comp.W|}

let setCustomComponenttDimensionB1W (newDims : {|H:float;W:float|}) (customComponent : SymbolT.Symbol) : SymbolT.Symbol =
    let comp = Optic.get SymbolT.component_ customComponent

    customComponent 
    |> Optic.set SymbolT.component_ {comp with H = newDims.H; W = newDims.W}

let componentDimension_ = Lens.create getCustomComponenttDimensionB1R setCustomComponenttDimensionB1W


// Function 2 : The position of a symbol on a sheet
let setSymbolPositionB2W (symID : ComponentId) (sheet : SheetT.Model) (newPosition : XYPos) = 
    let positionLens = (SheetT.symbolOf_ symID) >-> SymbolT.posOfSym_
    Optic.set positionLens newPosition sheet

// Function 3 : Read/write the order of ports on a specified side of a symbol
let orderOfPortsBySide_B3W (side : Edge) =  
    let orderOfEdge_ = 
        ((fun (orderMap : Map<Edge, string list>) -> Map.find side orderMap),
        (fun (newOrder : string list) orderMap -> 
        orderMap 
        |> Map.map (fun e oldOrder -> 
            match e with 
            | s when s = side -> newOrder
            | _ -> oldOrder
        )))
        ||> Lens.create  
    
    SymbolT.portMaps_ 
    >-> SymbolT.order_ 
    >-> orderOfEdge_