import "lib/github.com/diku-dk/lys/lys"
import "lib/github.com/athas/vector/vspace"

import "general"

module vec2_f32 = {
  open mk_vspace_2d f32

  def dup (t: f32): vector = {x=t, y=t}
}

type viewport = {zoom: f32,
                 center: vec2_f32.vector}

module type zoomable_input = {
  type state

  type screen_calculations

  val make_screen_calculations: i64 -> i64 -> viewport -> screen_calculations

  val to_screen_coordinate: screen_calculations -> (i64, i64) -> (i64, i64)
}

module mk_zoomable (zoomable_input: zoomable_input) = {
  type auto_zoom = {enabled: bool,
                    factor: f32}

  type state = {base: zoomable_input.state,
                width: i64,
                height: i64,
                viewport: viewport,
                auto_zoom: auto_zoom,
                mouse: {y: i32, x: i32},
                screen_calculations: zoomable_input.screen_calculations}

  def init (height: i64) (width: i64) (base: zoomable_input.state): state =
    let viewport = {center={x=0, y=0}, zoom=1}
    let screen_calculations = zoomable_input.make_screen_calculations height width viewport
    in {base,
        height,
        width,
        viewport,
        auto_zoom={enabled=false, factor=1.01},
        mouse={x=0, y=0},
        screen_calculations}

  local def update_screen_calculations (s: state): state =
    s with screen_calculations = zoomable_input.make_screen_calculations s.height s.width s.viewport

  def resize (h: i64) (w: i64) (s: state): state =
    let s = s with height = h
              with width = w
    in update_screen_calculations s

  def to_screen_coordinates (s: state) (values: [](f32, (i64, i64))): [s.height][s.width]f32 =
    let values' = map (.0) values
    let indices' = map (zoomable_input.to_screen_coordinate s.screen_calculations) (map (.1) values)

    -- let merge (v0: f32) (v1: f32): f32 =
    --   if v0 < 0 then v1
    --   else if v1 < 0 then v0
    --   else (v0 + v1) / 2
    -- in reduce_by_index_and_spread_2d s.height s.width merge (-1) indices' values'
    in spread_2d s.height s.width 0 indices' values'

  def text_content (s: state) = (s.viewport.center.x,
                                 s.viewport.center.y,
                                 s.viewport.zoom,
                                 i32.bool s.auto_zoom.enabled)

  def text_format () =
    "Viewport: center (x=%.03le, y=%.03le); zoom %.03le\n"
    ++ "Auto mode: %[disabled|enabled]\n"

  def zoom_at_mouse (zoom_factor: f32) (s: state): state =
    let xy_factor = f32.i64 (i64.min s.height s.width) * s.viewport.zoom
    let xb = r32 (s.mouse.x - i32.i64 s.width / 2)
    let xd = xb / xy_factor - xb / (xy_factor * zoom_factor)
    let yb = r32 (s.mouse.y - i32.i64 s.height / 2)
    let yd = yb / xy_factor - yb / (xy_factor * zoom_factor)
    let s = s with viewport.zoom = s.viewport.zoom * zoom_factor
              with viewport.center.x = s.viewport.center.x + xd
              with viewport.center.y = s.viewport.center.y + yd
    in update_screen_calculations s

  def event (e: event) (s: state): state =
    match e
    case #step _td ->
      if s.auto_zoom.enabled
      then zoom_at_mouse s.auto_zoom.factor s
      else s
    case #mouse {buttons, x, y} ->
      let x_diff = s.mouse.x - x
      let y_diff = s.mouse.y - y
      let s = s with mouse = {x, y}

      let xy_factor = f32.i64 (i64.min s.height s.width) * s.viewport.zoom

      let s = if buttons & 1 == 1 || buttons & 4 == 4
              then let s = s with viewport.center.x = s.viewport.center.x + r32 x_diff / xy_factor
                             with viewport.center.y = s.viewport.center.y + r32 y_diff / xy_factor
                   in update_screen_calculations s
              else s
      let s = if buttons & 4 == 4
              then s with auto_zoom.enabled = true
              else s with auto_zoom.enabled = false
      in s
    case #wheel {dx=_, dy} ->
      if s.auto_zoom.enabled
      then s with auto_zoom.factor = s.auto_zoom.factor + r32 dy * 0.01
      else let zoom_factor = 1 + r32 dy * 0.01
           in zoom_at_mouse zoom_factor s
    case _ -> s
}
