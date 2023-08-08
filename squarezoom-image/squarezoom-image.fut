import "../lib/github.com/diku-dk/lys/lys"
import "../lib/github.com/diku-dk/cpprandom/random"

import "../general"
import "../zoomable"
import "../oklab"
import "../hsv"

module rnge = xorshift128plus
type rng = rnge.rng
module dist = uniform_real_distribution f32 rnge

type c = (i64, i64)
type p = ((f32, c), f32, rng)

type approach = #hsv | #oklab | #grayscale

module zoomable_input = {
  type~ state = {time: f32,
                 rng: rng,
                 approach: approach,
                 image: [][]argb.colour}

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

module zoomable = mk_zoomable zoomable_input

def split4_index ((y, x): c): [4]c =
  let (y, x) = (y * 2, x * 2)
  in [(y, x), (y, x + 1), (y + 1, x), (y + 1, x + 1)]

def split4_value (((v, c), t, rng): p): [4]p =
  let t' = f32.abs (f32.sin t) * 0.1
  let (rng, v0) = dist.rand (0, 1) rng
  let (rng, v1) = dist.rand (0, 1) rng
  let (rng, v2) = dist.rand (0, 1) rng
  let (rng, v3) = dist.rand (0, 1) rng
  let vs = [v0, v1, v2, v3]
  let vs = map (+ t') vs
  let v_avg = reduce_comm (+) 0 vs / 4
  let v_factor = v / v_avg
  let vs = map (* v_factor) vs
  let cs = split4_index c
  let ts = replicate 4 (t * 0.9)
  let rngs = rnge.split_rng 4 rng
  in zip3 (zip vs cs) ts rngs

def create_ps [h][w] (image: [h][w]argb.colour) (time: f32) (rng: rng): [h * w]p =
  let indices =
    tabulate_2d h w (\y x -> (y, x))
    |> flatten
  let hues =
    image
    |> map (map (\c ->
                   let (r, g, b, _a) = argb.to_rgba c
                   let lab = linear_srgb_to_oklab {r, g, b}
                   let lch = to_LCh lab
                   in lch.h))
    |> flatten
  let rngs = rnge.split_rng (h * w) rng
  in zip3 (zip hues indices) (replicate (h * w) time) rngs

def expand_to (height: i64) (width: i64) (c: zoomable_input.screen_calculations) (inits: []p): []p =
  let expand ((blocks, size): ([]p, i64)): ([]p, i64) =
    let size' = size / 2

    let rect_outside_window (y_ul, x_ul) (y_lr, x_lr) =
      y_lr < 0 || y_ul >= height || x_lr < 0 || x_ul >= width

    let in_bounds (((_, (y0, x0)), _, _): p): bool =
      let (y0, x0) = (y0 * size', x0 * size')
      let sc = zoomable_input.to_screen_coordinate c
      in !rect_outside_window
          (sc (y0, x0))
          (sc (y0 + size', x0 + size'))

    let new = map split4_value blocks
              |> flatten
              |> filter in_bounds
    in (new, size')

  let prec = c.precision / height -- fixme: also handle width
  let (r, _) =
    loop blocks = (inits, prec)
    for _i < log2 prec
    do expand blocks
  in r

type text_content = (i32, i32, f32, f32, f32, i32)
module lys = {
  type~ state = zoomable.state

  local def render_pixel_hsv (v: f32): argb.colour =
    hsv_to_rgb (360 * v, 1, 1)

  local def render_pixel_oklab (v: f32): argb.colour =
    let c = oklab_to_linear_srgb (from_LCh {L=1, C=1, h=2 * f32.pi * v})
    in argb.from_rgba c.r c.g c.b 1

  local def render_pixel_grayscale (v: f32): argb.colour =
    argb.from_rgba v v v 1

  local def new_rng (s: state): state =
    -- Hack: Spice up the rng with a poor source.
    let (_, seed) = rnge.rand s.base.rng
    let rng = rnge.rng_from_seed [t32 (3007 * s.base.time) ^ i32.u64 seed]
    in s with base.rng = rng

  local def step (td: f32) (s: state): state =
    s with base.time = s.base.time + td

  local def keydown (key: i32) (s: state): state =
    if key == SDLK_r
    then new_rng s
    else if key == SDLK_h
    then s with base.approach = #hsv
    else if key == SDLK_o
    then s with base.approach = #oklab
    else if key == SDLK_g
    then s with base.approach = #grayscale
    else s

  def grab_mouse = false

  def init (seed: u32) (h: i64) (w: i64) (image: [h][w]argb.colour): state =
    let rng = rnge.rng_from_seed [i32.u32 seed]
    in zoomable.init h w {time=0, rng, approach=#oklab, image}

  def resize (h: i64) (w: i64) (s: state): state =
    zoomable.resize h w s

  def event (e: event) (s: state): state =
    let s = zoomable.event e s
    in match e
       case #step td -> step td s
       case #keydown {key} -> keydown key s
       case _ -> s

  def render (s: state): [][]argb.colour =
    let inits = create_ps s.base.image s.base.time s.base.rng
    let values = expand_to s.height s.width s.screen_calculations inits
                 |> map (.0)
                 |> zoomable.to_screen_coordinates s
    let render_with_approach render_pixel = map (map render_pixel) values
    in match s.base.approach
       case #hsv -> render_with_approach render_pixel_hsv
       case #oklab -> render_with_approach render_pixel_oklab
       case #grayscale -> render_with_approach render_pixel_grayscale

  type text_content = text_content

  def text_format () =
    "FPS: %d\nView: %[hsv|oklab|grayscale]\n"
    ++ zoomable.text_format ()

  def text_content (render_duration: f32) (s: state): text_content =
    let approach = match s.base.approach
                   case #hsv -> 0
                   case #oklab -> 1
                   case #grayscale -> 2
    let z = zoomable.text_content s
    in (t32 render_duration, approach, z.0, z.1, z.2, z.3)

  def text_colour (s: state) =
    match s.base.approach
    case #hsv -> argb.black
    case #oklab -> argb.black
    case #grayscale -> argb.green
}
