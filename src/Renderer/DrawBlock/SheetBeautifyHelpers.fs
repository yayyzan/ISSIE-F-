module SheetBeautifyHelpers
open CommonTypes
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open Optics
open Optics.Operators
open BlockHelpers

//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team

type Lens<'A,'B> = ('A -> 'B) * ('B -> 'A -> 'A)

// dimensions in form (height * width)
let CustomComponentDimensionsLens : Lens<Symbol, (float*float)> =
    
    let getCCDimensions (symbol: Symbol) = 
        let h = symbol.Component.H
        let w = symbol.Component.W
        (h, w)
    
    let setCCDimensions (newDimensions: (float * float)) (symbol: Symbol) = 
        let newHeight, newWidth = newDimensions
        setCustomCompHW newHeight newWidth symbol

    (getCCDimensions, setCCDimensions)

// let model' = Optic.set (symbolOf_ symbol.Id) scaledSymbol wModel