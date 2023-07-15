# squarezoom

A neat little expanding square world.

![Screenshot](screenshot.png)

Requires [Futhark](http://futhark-lang.org) and SDL2 and SDL2-ttf
libraries with associated header files.


## Building and running

First run `futhark pkg sync` once.

Then run `make && ./squarezoom -R -w 1024 -h 1024` (or other powers of
two) to build and run in a window.

## Controls

  - `ESC`: Exit the program.
  - `F1`: Toggle the text in the upper-left corner.
  - `r`: Generate a new world.
  - `h`: View in the HSV color space (default).
  - `o`: View in the Oklab color space.
  - `g`: View as grayscale.
