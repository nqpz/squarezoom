import "lib/github.com/diku-dk/lys/lys"
import "lib/github.com/diku-dk/cpprandom/random"
import "oklab"
import "hsv"

module rnge = xorshift128plus
type rng = rnge.rng
module dist = uniform_real_distribution f32 rnge

type c = (i64, i64)
type p = (f32, rng)

def split4_index ((y, x): c): [4]c =
  let (y', x') = (y * 2, x * 2)
  in [(y', x'), (y', x' + 1), (y' + 1, x'), (y' + 1, x' + 1)]

def split4_value ((v, rng): p): [4]p =
  let (rng, v0) = dist.rand (0, 1) rng
  let (rng, v1) = dist.rand (0, 1) rng
  let (rng, v2) = dist.rand (0, 1) rng
  let (rng, v3) = dist.rand (0, 1) rng
  let avg = (v0 + v1 + v2 + v3) / 4
  let factor = v / avg
  let (v0', v1', v2', v3') = (v0 * factor, v1 * factor, v2 * factor, v3 * factor)
  let rngs = rnge.split_rng 4 rng
  in zip [v0', v1', v2', v3'] rngs

def expand [h][w] (blocks: [h][w]p): [h * 2][w * 2]p =
  let indices = flatten_3d (tabulate_2d h w (curry split4_index))
  let values = flatten_3d (map (map split4_value) blocks)
  let dest = replicate (h * 2) (replicate (w * 2) (0, rnge.rng_from_seed [0]))
  in scatter_2d dest indices values

def expand_to (size: i64) (init: p) =
  loop blocks = [[init]]
  for _i < t32 (f32.log2 (f32.i64 size))
  do expand blocks

type text_content = (i32, i32)
module lys: lys with text_content = text_content = {
  type approach = #hsv | #oklab | #grayscale

  type~ state = {time: f32, rng: rng, h: i64, w: i64,
                 render: [][]argb.colour,
                 approach: approach}

  local def render_pixel_hsv (v: f32): argb.colour =
    hsv_to_rgb (360 * v, 1, 1)

  local def render_pixel_oklab (v: f32): argb.colour =
    let c = oklab_to_linear_srgb (from_LCh {L=1, C=1, h=2 * f32.pi * v})
    in argb.from_rgba c.r c.g c.b 1

  local def render_pixel_grayscale (v: f32): argb.colour =
    argb.from_rgba v v v 1

  local def make_render (h: i64) (w: i64) (rng: rng) (approach: approach): [][]argb.colour =
    let blocks = expand_to (i64.min h w) (1, rng)
    let render_with_approach render_pixel = map (map (render_pixel <-< (.0))) blocks
    in match approach
       case #hsv -> render_with_approach render_pixel_hsv
       case #oklab -> render_with_approach render_pixel_oklab
       case #grayscale -> render_with_approach render_pixel_grayscale

  local def with_new_render (s: state): state =
    s with render = make_render s.h s.w s.rng s.approach

  local def new_rng (s: state): state =
    -- Hack: Spice up the rng with a poor source.
    let (_, seed) = rnge.rand s.rng
    let rng = rnge.rng_from_seed [t32 (3007 * s.time) ^ i32.u64 seed]
    in s with rng = rng

  local def step (td: f32) (s: state): state =
    s with time = s.time + td

  local def keydown (key: i32) (s: state): state =
    if key == SDLK_r
    then with_new_render (new_rng s)
    else if key == SDLK_h
    then with_new_render (s with approach = #hsv)
    else if key == SDLK_o
    then with_new_render (s with approach = #oklab)
    else if key == SDLK_g
    then with_new_render (s with approach = #grayscale)
    else s

  def grab_mouse = false

  def init (seed: u32) (h: i64) (w: i64): state =
    let rng = rnge.rng_from_seed [i32.u32 seed]
    let approach = #hsv
    in {time=0, rng, approach, h, w, render=make_render h w rng approach}

  def resize (h: i64) (w: i64) (s: state): state =
    s with h = h with w = w

  def event (e: event) (s: state): state =
    match e
    case #step td -> step td s
    case #keydown {key} -> keydown key s
    case _ -> s

  def render (s: state): [][]argb.colour =
    s.render

  type text_content = text_content

  def text_format () = "FPS: %d\nView: %[hsv|oklab|grayscale]"

  def text_content (render_duration: f32) (s: state): text_content =
    (t32 render_duration, match s.approach
                          case #hsv -> 0
                          case #oklab -> 1
                          case #grayscale -> 2)

  def text_colour (s: state) =
    match s.approach
    case #hsv -> argb.black
    case #oklab -> argb.black
    case #grayscale -> argb.green
}
