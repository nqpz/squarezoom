import "lib/github.com/diku-dk/lys/lys"
import "lib/github.com/athas/vector/vspace"

import "general"

module vec2_f32 = {
  open mk_vspace_2d f32

  def dup (t: f32): vector = {x=t, y=t}
}

module type base = {
  type~ state
}

module mk_zoomable (base: base) = {
  type viewport = {zoom: f32,
                   center: vec2_f32.vector}

  type auto_zoom = {enabled: bool,
                    factor: f32}

  type~ state = {base: base.state,
                 width: i64,
                 height: i64,
                 viewport: viewport,
                 auto_zoom: auto_zoom,
                 mouse: {y: i32, x: i32}}

  def init (h: i64) (w: i64) (base: base.state): state =
    {base,
     height=h,
     width=w,
     viewport={center={x=0, y=0}, zoom=1},
     auto_zoom={enabled=false, factor=1.01},
     mouse={x=0, y=0}}

  def resize (h: i64) (w: i64) (s: state): state =
    s with height = h
      with width = w

  def to_screen_coordinates [hw] (s: state) (values: [hw][hw]f32): [s.height][s.width]f32 =
    let xy_factor = f32.i64 (i64.min s.height s.width)
    let center_offset = vec2_f32.dup (f32.i64 hw / xy_factor / 2)
    let offset = {y=f32.i64 (i64.max 0 (s.width - s.height)) / xy_factor,
                  x=f32.i64 (i64.max 0 (s.height - s.width)) / xy_factor}
    let offset = vec2_f32.(scale 0.5 offset + dup 0.5)

    let viewport_center_scaled = vec2_f32.scale s.viewport.zoom s.viewport.center
    let offset_viewport_scaled = vec2_f32.(scale xy_factor (offset - viewport_center_scaled))

    let to_screen_coordinate {y: i64, x: i64}: vec2_f32.vector =
      let p = {y=f32.i64 y, x=f32.i64 x}
              |> vec2_f32.scale (1 / xy_factor)
              |> (vec2_f32.- center_offset)
              |> vec2_f32.scale s.viewport.zoom
      in vec2_f32.(scale xy_factor p + offset_viewport_scaled)

    let make_index (y: i64) (x: i64): (i64, i64) =
      let {y, x} = to_screen_coordinate {y, x}
      in (i64.f32 y, i64.f32 x)

    let indices = tabulate_2d hw hw make_index
    let indices' = flatten indices
    let values' = flatten values

    let merge (v0: f32) (v1: f32): f32 =
      if v0 < 0 then v1
      else if v1 < 0 then v0
      else (v0 + v1) / 2
    in reduce_by_index_and_spread_2d s.height s.width merge (-1) indices' values'
    -- in spread_2d s.height s.width 0 indices' values'

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
    in s with viewport.zoom = s.viewport.zoom * zoom_factor
         with viewport.center.x = s.viewport.center.x + xd
         with viewport.center.y = s.viewport.center.y + yd

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
              then s with viewport.center.x = s.viewport.center.x + r32 x_diff / xy_factor
                     with viewport.center.y = s.viewport.center.y + r32 y_diff / xy_factor
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
