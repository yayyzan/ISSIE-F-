# Summary

I will be in charge of testing D1, which means I will be working closely with the D1B. As a starter, I have created a simple near-straight wire test which will fail when a single wire
is not straightened. This is a simple test case to ensure that the main functionality of D1B is working. 

As D1B grows in complexity (taking into account more corner cases) so will the test suite. The test suite makes good use of the SheetBeautifyHelpers created. Specifically making use of the gitVisibleSegmentsOfWire function. This function is very useful as it provides only the visible
segments seen by the user. The construction of the visible segments list is specified in SheetBeautifyHelpers.

# Team Contribution Starter
`TestDrawBlockD1.fs`
