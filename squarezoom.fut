import "lib/github.com/diku-dk/lys/lys"
import "lib/github.com/diku-dk/cpprandom/random"

import "general"
import "zoomable"
import "oklab"
import "hsv"

module rnge = xorshift128plus
type rng = rnge.rng
module dist = uniform_real_distribution f32 rnge

type c = (i64, i64)
type p = ((f32, c), f32, rng)

type approach = #hsv | #oklab | #grayscale

module mk_zoomable_input (state: { type~ state }) = {
  type~ state = state.state

  type screen_calculations = {precision: i64,
                              xy_factor_inv: f32,
                              center_offset: vec2_f32.vector,
                              zoom_factor: f32,
                              offset_viewport_scaled: vec2_f32.vector}

  def make_screen_calculations (height: i64) (width: i64) (viewport: viewport): screen_calculations =
    let precision_scale = 2**i64.max 0 (i64.f32 (f32.ceil (f32.log2 viewport.zoom)))

    let size = i64.min height width

    let precision = size * precision_scale

    let xy_factor = f32.i64 size
    let xy_factor_inv = 1 / xy_factor

    let center_offset = vec2_f32.dup (f32.i64 precision / xy_factor / 2)

    let zoom_factor = xy_factor * viewport.zoom / f32.i64 precision_scale

    let offset = {y=f32.i64 (i64.max 0 (width - height)) / xy_factor,
                  x=f32.i64 (i64.max 0 (height - width)) / xy_factor}
    let offset = vec2_f32.(scale 0.5 offset + dup 0.5)
    let viewport_center_scaled = vec2_f32.scale viewport.zoom viewport.center
    let offset_viewport_scaled = vec2_f32.(scale xy_factor (offset - viewport_center_scaled))

    in {precision,
        xy_factor_inv,
        center_offset,
        zoom_factor,
        offset_viewport_scaled}

  local def to_vector ((y, x): (i64, i64)): vec2_f32.vector =
    {y=f32.i64 y, x=f32.i64 x}

  local def to_tuple ({y, x}: vec2_f32.vector): (i64, i64) =
    (i64.f32 y, i64.f32 x)

  def to_screen_coordinate (c: screen_calculations): (i64, i64) -> (i64, i64) =
    to_vector
    >-> vec2_f32.scale c.xy_factor_inv
    >-> (vec2_f32.- c.center_offset)
    >-> vec2_f32.scale c.zoom_factor
    >-> (vec2_f32.+ c.offset_viewport_scaled)
    >-> to_tuple
}

def split4_index ((y, x): c): [4]c =
  let (y, x) = (y * 2, x * 2)
  in [(y, x), (y, x + 1), (y + 1, x), (y + 1, x + 1)]

module mk_auxiliary_functions (zoomable_input: zoomable_input) (splitter: { val split4_value: p -> [4]p }) = {
  def expand (height: i64) (width: i64) (c: zoomable_input.screen_calculations) ((blocks, size): ([]p, i64)): ([]p, i64) =
    let size' = size / 2

    let rect_outside_window (y_ul, x_ul) (y_lr, x_lr) =
      y_lr < 0 || y_ul >= height || x_lr < 0 || x_ul >= width

    let in_bounds (((_, (y0, x0)), _, _): p): bool =
      let (y0, x0) = (y0 * size', x0 * size')
      let sc = zoomable_input.to_screen_coordinate c
      in !rect_outside_window
          (sc (y0, x0))
          (sc (y0 + size', x0 + size'))

    let new = map splitter.split4_value blocks
              |> flatten
              |> filter in_bounds
    in (new, size')
}
