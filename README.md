Buddhabrot
==========

A Buddhabrot image generator.

Principle
---------

The Buddhabrot is yet another way of turning the well-known Mandelbrot
set into pretty pictures. Here is a
[good description by Paul Bourke](http://paulbourke.net/fractals/buddhabrot/).

One of the pioneers of this technique was
[Melinda Green](http://www.superliminal.com/fractals/bbrot/bbrot.htm).

This program focuses on rendering Buddhabrot images as described by
[Johann Korndoerfer](http://erleuchtet.org/2010/07/ridiculously-large-buddhabrot.html).

Usage
-----

The buddhabrot program accepts several commands, each has specific
options:

  - `compute` samples a number of random points in the complex plane,
    and selects those for which the suite is known to diverge after a
    number of iterations comprised between a minimum and a maximum
    value. The selected points are saved in a document in JSON format.
  - `render` reads the output of `compute` and renders a PNG image.
  - `showcells` generates an animated GIF that shows to the developer
    (or the curious user) the set of "cells" in the complex plane
    where points are picked. These cells are chosen to overlap the
    border of the Mandelbrot set.

All commands list their options with `--help`.

Examples:

	$ buddhabrot compute
	Sampling 300K points...
	Selected cells: 67315
	Writing cache /tmp/buddhabrot-300K-100K_200K.bbc ...
	Done!

	$ buddhabrot render -c /tmp/buddhabrot-300K-100K_200K.bbc
	Loading cache /tmp/buddhabrot-300K-100K_200K.bbc ...
	selected points: 40
	done plotting
	img points: 5472163
	value range: 0-9447
	Writing /tmp/buddhabrot-300K-100K_200K.bbc.png ...
	Done!
