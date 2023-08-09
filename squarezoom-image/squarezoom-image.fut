import "../lib/github.com/diku-dk/lys/lys"
import "../lib/github.com/diku-dk/cpprandom/random"

import "../general"
import "../zoomable"
import "../oklab"
import "../hsv"

import "../squarezoom"

module zoomable_input = mk_zoomable_input {
  type~ state = {time: f32,
                 rng: rng,
                 approach: approach,
                 image: [][]argb.colour}
}

module zoomable = mk_zoomable zoomable_input

open mk_auxiliary_functions zoomable_input {
  def split4_value (((v, c), t, rng): p): [4]p =
    let t' = f32.abs (f32.sin t) * 0.1
    let (rng, v0) = dist.rand (-0.1, 0.1) rng
    let (rng, v1) = dist.rand (-0.1, 0.1) rng
    let (rng, v2) = dist.rand (-0.1, 0.1) rng
    let (rng, v3) = dist.rand (-0.1, 0.1) rng
    let vs = [v0, v1, v2, v3]
    let vs = map (+ v + t') vs
    let v_avg = reduce_comm (+) 0 vs / 4
    let v_factor = v / v_avg
    let vs = map (* v_factor) vs
    let cs = split4_index c
    let ts = replicate 4 (t * 0.9)
    let rngs = rnge.split_rng 4 rng
    in zip3 (zip vs cs) ts rngs
}

def expand_to (height: i64) (width: i64) (c: zoomable_input.screen_calculations) (inits: []p): []p =
  let prec = c.precision / height -- fixme: also handle width
  let (r, _) =
    loop t = (inits, prec)
    for _i < log2 prec
    do expand height width c t
  in r

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
