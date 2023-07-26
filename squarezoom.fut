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
type p = ((f32, c), rng)

def split4_index ((y, x): c): [4]c =
  let (y, x) = (y * 2, x * 2)
  in [(y, x), (y, x + 1), (y + 1, x), (y + 1, x + 1)]

def split4_value (((v, c), rng): p): [4]p =
  let (rng, v0) = dist.rand (0, 1) rng
  let (rng, v1) = dist.rand (0, 1) rng
  let (rng, v2) = dist.rand (0, 1) rng
  let (rng, v3) = dist.rand (0, 1) rng
  let vs = [v0, v1, v2, v3]
  let v_avg = reduce_comm (+) 0 vs / 4
  let v_factor = v / v_avg
  let vs = map (* v_factor) vs
  let rngs = rnge.split_rng 4 rng
  let cs = split4_index c
  in zip (zip vs cs) rngs

def expand ((blocks, size): ([]p, i64)): ([]p, i64) =
  (flatten (map split4_value blocks), size / 2)

def expand_to (size: i64) (init: p): []p =
  let (r, _) =
    loop blocks = ([init], size)
    for _i < t32 (f32.log2 (f32.i64 size)) + 1
    do expand blocks
  in r

type approach = #hsv | #oklab | #grayscale

module base = {
  type~ state = {time: f32,
                 rng: rng,
                 approach: approach}
}

module zoomable = mk_zoomable base

type text_content = (i32, i32, f32, f32, f32, i32)
module lys: lys with text_content = text_content = {
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

  def init (seed: u32) (h: i64) (w: i64): state =
    let rng = rnge.rng_from_seed [i32.u32 seed]
    in zoomable.init h w {time=0, rng, approach=#hsv}

  def resize (h: i64) (w: i64) (s: state): state =
    zoomable.resize h w s

  def event (e: event) (s: state): state =
    let s = zoomable.event e s
    in match e
       case #step td -> step td s
       case #keydown {key} -> keydown key s
       case _ -> s

  def render (s: state): [][]argb.colour =
    let size = i64.min s.height s.width
    let values = expand_to size ((1, (0, 0)), s.base.rng)
                 |> map (.0)
                 |> zoomable.to_screen_coordinates s -- (size * 2)
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
