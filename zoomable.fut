import "lib/github.com/diku-dk/lys/lys"
import "lib/github.com/athas/vector/vspace"

module vec2_f32 = {
  open mk_vspace_2d f32

  def dup (t: f32): vector = {x=t, y=t}
}

module type base = {
  type~ state
}

module mk_zoomable (base: base) = {
  type viewport = {
      zoom: f32,
      center: vec2_f32.vector
  }

  type auto_zoom = {
      enabled: bool,
      factor: f32
  }

  type~ state = {
      base: base.state,
      width: i32,
      height: i32,
      viewport: viewport,
      auto_zoom: auto_zoom,
      mouse: {x: i32, y: i32}
  }

  def init (h: i64) (w: i64) (base: base.state): state =
    {base,
     height=i32.i64 h,
     width=i32.i64 w,
     viewport={center={x=0, y=0}, zoom=1},
     auto_zoom={enabled=false, factor=1.01},
     mouse={x=0, y=0}}

  def resize (h: i64) (w: i64) (s: state): state =
    s with height = i32.i64 h
      with width = i32.i64 w

  def to_screen_coordinate (s: state) {x: i32, y: i32}: vec2_f32.vector =
    let xy_factor = r32 (i32.min s.height s.width)
    -- let x_offset_base = r32 (i32.max 0 (width s - height s)) / xy_factor
    -- let y_offset_base = r32 (i32.max 0 (height s - width s)) / xy_factor
    -- let x_offset = x_offset_base / 2
    -- let y_offset = y_offset_base / 2

    let offset = {y=r32 (i32.max 0 (s.width - s.height)) / xy_factor,
                  x=r32 (i32.max 0 (s.height - s.width)) / xy_factor}
    let offset = vec2_f32.(scale 0.5 offset + dup 0.5)

    let viewport_center_scaled = vec2_f32.scale s.viewport.zoom s.viewport.center

    let offset_viewport_scaled = vec2_f32.(scale xy_factor (offset - viewport_center_scaled))

    -- let x = xy_factor * ((r32 x + 0.5 + x_offset) - s.viewport.center.x * s.viewport.zoom)
    -- let y = xy_factor * ((r32 y + 0.5 + y_offset) - s.viewport.center.y * s.viewport.zoom)

    -- let x = xy_factor * ((r32 x + offset.x) - s.viewport.center.x * s.viewport.zoom)
    -- let y = xy_factor * ((r32 y + offset.y) - s.viewport.center.y * s.viewport.zoom)

    -- let p = {x=r32 x, y=r32 y}

    -- let p = vec2_f32.(scale xy_factor ({x=r32 x, y=r32 y} + offset - viewport_center_scaled))

    in vec2_f32.(scale xy_factor {x=r32 x, y=r32 y} + offset_viewport_scaled)

    -- let p = vec2_f32.(scale xy_factor {x=r32 x, y=r32 y} + offset_viewport_scaled)

    -- in {x, y}
    -- in p

  def text_content (s: state) = (s.viewport.center.x,
                                 s.viewport.center.y,
                                 s.viewport.zoom,
                                 i32.bool s.auto_zoom.enabled)

  def text_format () =
    "Viewport: center (%.03le, %.03le); zoom %.03le\n"
    ++ "Auto mode: %[disabled|enabled]\n"

  def zoom_at_mouse (zoom_factor: f32) (s: state): state =
    let xy_factor = r32 (i32.min s.height s.width) * s.viewport.zoom
    let xb = r32 (s.mouse.x - s.width / 2)
    let xd = xb / xy_factor - xb / (xy_factor * zoom_factor)
    let yb = r32 (s.mouse.y - s.height / 2)
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

      let xy_factor = r32 (i32.min s.height s.width) * s.viewport.zoom

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
