My team work is in SheetBeautifyD3 module.

I have created a way of measuring the length of an individual net.

This will be useful because, by comparing the length of the net with either the
overall length (from T4R) or a threshold, we will be able to determine which wires
are considered "Long". These are the wires which will be subject to labelling

I have thought about how to serialise the labeling state of a wire.

I am making an assumption that whole nets will be labelled rather than just wires.

By that assumption, it would make sense that each output port will "know" whether its wires are labeled or not. That way, when rendering the wire, we can lookup this new property in the OutputPort and if it is labelled, render accordingly.

To accomplish this, I began setting up a lens which will allow easy setting of this new
flag, which I am calling `IsWireLabeled` (need to decide between labelled and labeled).
By providing the OuputPortId, we will be able to access or set this property.