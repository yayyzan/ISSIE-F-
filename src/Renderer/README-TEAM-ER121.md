## D2T Module Overview

The `D2T` module in `TestDrawBlockD2.fs` enhances circuit layout testing by introducing randomness in gate and MUX orientations and input connections. This approach aims to simulate a variety of manually generated test circuits layout scenarios to evaluate the effectiveness of wire routing algorithms. Key functionalities include:

- **Randomizing Symbol Orientations**: Utilizes `randomRotation` and `randomFlipType` to randomly rotate and flip gates and MUXes, mimicking diverse circuit configurations.
- **Shuffling MUX Inputs**: The `shufflePortMapsOrder` and `updateMux2PortOrder` functions randomly swap inputs on 2-MUX symbols, testing the routing adaptability to changes in input connections.
- **Evaluating Layout Quality**: Implements `checkWireCrossing` and `countCrossingsInSheet` to identify wire intersections, alongside `countWireStraightInSheet` to count straightened wire segments. These metrics assess the layout's clarity and organisation.
- **Generating Test Circuits**: `baseCircuit1` and `baseCircuit2` functions construct test circuits with specific components and connections, subjected to layout tests (`testWireCrossingD2` and `testWireStraightD2`) to verify the routing outcomes.

    The tests check for wire crossings and straight wires to see how these changes affect the layout.
