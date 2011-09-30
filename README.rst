``vselect`` is a simple utility for visually selecting a subset from a list
using a curses-like interface. Inputs are taken from ``stdin`` and output is
sent to ``stdout``. 

Each line in the input is one possible selection. One or more of these can be
selected, and the output is printed one item per line.

For example, to count the number of lines a few files in the current directory::
	
	ls -1 | vselect | xargs wc -l

The ``vselect`` stage of the pipeline will bring up a list view of the inputs
from the first stage. After exiting from ``vselect``, its outputs are fed into
the next stage.

Key bindings
------------

- Enter: exit with current selection
- q: exit without a selection
- x: mark current item as selected
- j: move down
- k: move up

Selected items
--------------

The current selection is either:

1. the set of all marked items;
2. or, if there are no marked items the currently active item in the list
