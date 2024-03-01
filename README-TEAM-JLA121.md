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

  - Also added function `getSimSymbolList` which gets a list of simple symbols from a sheet model. Using this I can 'draw' a testcase I want in issie, then run the function to get the list, which I can use later to create sheet models to run tests on.

 - The idea is to soon do this for wires as well as symbols, then to begin creating suitable tests for D3 and beyond.

 -------------

 - Finally, I have included code that generates random rotations and flips and also code which can create a `Gen<XYPos * Rotation * FlipType>` which contains an XYPos, and a random rotation and flip. This can be found in function `gridPositionsWithFlipAndRotation`.

