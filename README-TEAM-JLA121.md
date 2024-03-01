 ## Team-phase work contributions

 ---------------------

 ### Asserts
  - Added `failOnBeautifyIncreasesRightAngles` function which takes two sheet models, one before and one after beautify. Test fails if number right angles greater in post-beautification sheet.


 - Included `failOnSymbolIntersectsSymbol` since this is something we never want to happen after beautification.


 ### Improving test circuit generation

  - I have added `simpleSymbol` type which contains

    -   symLabel : string
    -   compType : ComponentType
    -   position: XYPos
    -   sTransform: STransform
    
    So I can take list of these 'simpleSymbols' and generate a sheet model with them in it.

 - To do this I have added two functions `placeSimpleSymbol` & `placeSimSymbolList`

  - This is beneficial since I can place all the symbols for a testcase by simply defining a list and running one function `placeSimSymbolList`
