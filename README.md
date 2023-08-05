# squarezoom

Zoom in and out of a neat little random square world.

![Screenshot](screenshot.png)

Requires [Futhark](http://futhark-lang.org) and SDL2 and SDL2-ttf
libraries with associated header files.


## Building and running

First run `futhark pkg sync` once.

Then run `make && ./squarezoom-random/squarezoom-random -R -w 1024 -h 1024` (or other powers of
two) to build and run in a window.

## Controls

  - Mouse wheel scroll: Zoom in/out.
  - Left click and hold: Move around.
  - Right click: Automatic zoom.
  - Right click and mouse wheel scroll: Adjust automatic zoom speed.
  - `ESC`: Exit the program.
  - `F1`: Toggle the text in the upper-left corner.
  - `h`: View in the HSV color space (default).
  - `o`: View in the Oklab color space.
  - `g`: View as grayscale.
  - `r`: Generate a new world.
