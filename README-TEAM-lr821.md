# Summary of Individual Contributions to the group

Having been tasked with the build section of D2, I implemented an initial algorithm to reduce the number of wire bends by exhaustively searching through all MUX permutations. This most basic approach works by performing the following steps:

- Obtain all MUXes in the sheet
- For each MUX obtain all permutations by:
  - Flipping vertically (to move the select port)
  - Applying all rotations
  - Permutating all input ports
- Once all permutations for each MUX symbol were obtained, these are combined to get all possible combinations of MUX permutations with each other.
- At this point the optimal configuration is found by minimising over the number of segment intersections (using the `T3R (countVisibleSegmentIntersection)` function from the `SheetBeautifyHelpers`)


To complete this first implementation, a number of helper functions were defined in order to obtain the permutations of a given MUX symbol, most of these were constructed in such a way that renders them adaptable to different component types.

## Future work

In order to verify whether this implementation works as intended, some more work is needed for the evalution step; currently the sheet is updated by simply changing the `Symbols` map, however this does not cause any update to the rest of the sheet, and means that running the `countVisibleSegmentIntersection` function will yield the same result regardless of the permutation, since all wires are unchanged. Looking into the code from `SheetUpdate` and `BusWireUpdate` reveals that wire re-reouting post flip is handled by messages within the `MVU` model, so some work has to be done to perform these operations in a separate context.

The exhaustive search approach is not scalable at all, as it is factorial in the number of input ports to a component and exponential in the number of permutations between each component. A more scalable approach could be achieved by treating the problem as having "Optimal Substructure", hence assuming that, within a given sheet configuration, minimising over all permutations of a single component, is optimal for all possible sheet configurations. Therefore all components could be ranked by how many segment intersections each wire they are connected to causes, and then optimise each component starting from the component that causes the most intersections. With this approach the number of permutations to test is only factorial in the number of input ports.