###
The MIT License

Copyright (c) 2016 Jason S. Ku

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

TO DO
###

EPS = Math.pow(10,-13)

TAB    =  9
RETURN = 13
LEFT   = 37
UP     = 38
RIGHT  = 39
DOWN   = 40

class Point

  @free: 0

  constructor: (@x = 0, @y = 0, @z = 0) -> @id = Point.free++

  @from_spherical: (@theta = 0, @phi = 0) ->
    new Point(
      Math.cos(@phi)*Math.cos(@theta)
      Math.cos(@phi)*Math.sin(@theta)
      Math.sin(@phi)
    )

  add:  (v) -> new Point(@x + v.x, @y + v.y, @z + v.z)
  plus: (s) -> new Point(@x + s, @y + s, @z + s)
  mul:  (s) -> new Point(@x * s, @y * s, @z * s)
  div:  (s) -> if s isnt 0 then @mul(1.0 / s) else null
  sub:  (v) -> @add(v.mul(-1))
  dot:  (v) -> (@x * v.x) + (@y * v.y) + (@z * v.z)
  dist: (v) -> Math.sqrt(@dist_sq v)
  dist_sq: (v) -> @sub(v).mag_sq()
  is_zero: () -> @mag() is 0
  neg_y:  () -> new Point(@x, -@y, @z)
  mag_sq: () -> @dot @
  mag: () -> Math.sqrt @mag_sq()
  dir: () -> @div @mag() if @mag() > EPS

  cross: (v) ->
    new Point(
      (@y * v.z) - (@z * v.y)
      (@z * v.x) - (@x * v.z)
      (@x * v.y) - (@y * v.x)
    )

  angle: (u, v) ->
    u = u.sub(@).dir()
    v = v.sub(@).dir()
    Math.acos(u.dot v)

  rotate: (axis, ang) ->
    @mul(Math.cos(ang)).add(
      axis.cross(@).mul(Math.sin(ang))).add(
      axis.mul(@dot(axis)).mul(1-Math.cos(ang)))

  intersects: (q, u, v) ->
    n1 = u.cross(v)
    n2 = @.cross(q)
    x = n1.cross(n2).dir()
    x = x.mul(-1) if @.dot(x) < 0
    return u.cross(x).dot(n1) > 0 and x.cross(v).dot(n1) > 0 and
           @.cross(x).dot(n2) > 0 and x.cross(q).dot(n2) > 0

  @outside_point: (ps) ->
    return (ps.reduce (a,b) -> a.add(b)).add(Point.noise(0.01)).dir().mul(-1)

  is_inside: (ps, q) ->
    count = 0
    for p1, i in ps[0..ps.length-2]
      p2 = ps[i + 1]
      if @intersects(q,p1,p2)
        count++
    return (count % 2) != 0

  @noise: (s) ->
    return (new Point(Math.random(),Math.random(),Math.random())).mul(s)

  @center: (ps) ->
    max = (new Point 1, 1, 1).div(-EPS)
    min = (new Point 1, 1, 1).div(EPS)
    for p in ps
      max[c] = Math.max p[c], max[c] for c in ['x', 'y', 'z']
      min[c] = Math.min p[c], min[c] for c in ['x', 'y', 'z']
    max.add(min).div 2

class View
  
  constructor: (@id) ->
    @center = null
    @scale = null
    @dx = new Point 1, 0, 0
    @dy = new Point 0, 1, 0
    @inset_factor = 0.9
    @view_size = 150
    @svg = SVG(@id)
    @svg.viewbox 0, 0, @view_size, @view_size

  clear: () -> @svg.clear()

  scale_points: (ps) ->
    @center = Point.center ps
    @scale = EPS
    @scale = Math.max @scale, @center.sub(p).mag() for p in ps
    @scale = @view_size * @inset_factor / @scale / 2

  set_frame: (dx, dz) ->
    @dx = dx
    dz = @dx.perp() unless dz?
    @dy = dz.cross(@dx)

  location: (p) ->
    q = new Point(
      p.sub(@center).dot(@dx),
      p.sub(@center).dot(@dy)
    )
    q.mul(@scale).neg_y().plus(0.5 * @view_size)

  draw_point: (p) ->
    q = @location p
    @svg.circle(2).center(q.x, q.y)
    
  draw_point_text: (p, label) ->
    q = @location p
    @svg.text(label).move(q.x, q.y)

  draw_polyline: (ps) ->
    qs = (@location p for p in ps)
    @svg.polyline ([q.x, q.y] for q in qs)

  draw_polygon: (ps) ->
    qs = (@location p for p in ps)
    @svg.polygon ([q.x, q.y] for q in qs)

class Rotator

  constructor: (@view, @draw) ->
    @yaw = Math.PI/3
    @pitch = Math.PI/4
    @pitch_lim = 0.42
    @ang_scale = 0.002
    @set_frame @yaw, @pitch
    @start = null
    @current = null

    @view.svg.on 'contextmenu', (e) -> e.preventDefault()
    @view.svg.on 'selectstart', (e) -> e.preventDefault()
    @view.svg.on 'dragstart',   (e) -> e.preventDefault()
    @view.svg.mousedown (e) => @start_spin(e)
    @view.svg.mouseup   (e) => @stop_spin()
    @view.svg.mousemove (e) => @spin(e)
    @view.svg.mouseout  (e) =>
      top = if e.target.id? and e.target.id is @view.id then e.relatedTarget else e.target
      top = top.parentNode while top? and (top.id != @view.id) and (top.nodeName != 'BODY')
      @stop_spin() if not top? or top.nodeName is 'BODY'

  refresh: () -> @set_frame(@yaw, @pitch); @draw()

  set_frame: (yaw, pitch) ->
    dz = new Point(
      Math.cos(pitch) * Math.cos(yaw)
      Math.cos(pitch) * Math.sin(yaw)
      Math.sin(pitch)
    )
    dx = dz.cross(new Point(0, 0, -1)).dir()
    @view.set_frame dx, dz

  stop_spin: () ->
    if @start? and @current?
      @yaw = @get_yaw()
      @pitch = @get_pitch()
      @refresh()
    @start = null
    @current = null

  start_spin: (e) -> @start = @screen_pt(e)

  step_yaw: (d) -> (@yaw + d * @ang_scale * Math.PI) % (2 * Math.PI)

  step_pitch: (d) ->
    pitch = @pitch - d * @ang_scale * Math.PI
    if not (Math.abs(pitch) < @pitch_lim * Math.PI)
      pitch *= @pitch_lim * Math.PI / Math.abs(pitch)
    pitch

  get_yaw:   () -> @step_yaw(@start.x - @current.x)
  get_pitch: () -> @step_pitch(@start.y - @current.y)
  increment_yaw: () -> @yaw = @step_yaw( 10); @refresh()
  decrement_yaw: () -> @yaw = @step_yaw(-10); @refresh()
  increment_pitch: () -> @pitch = @step_pitch( 10); @refresh()
  decrement_pitch: () -> @pitch = @step_pitch(-10); @refresh()

  spin: (e) ->
    if @start?
      @current = @screen_pt(e)
      @set_frame @get_yaw(), @get_pitch()
      @draw()

  screen_pt: (e) ->
    p = @view.svg.node.createSVGPoint()
    p.x = e.clientX
    p.y = e.clientY
    p.matrixTransform @view.svg.node.getScreenCTM().inverse()
    new Point p.x, p.y

class AppHandler

  TURN_STEP = Math.PI/100
  TURN_LIM = Math.cos(TURN_STEP)

  constructor: () ->
    @view = new View 'view'
    @view.scale_points (new Point(x,y,z) for [x,y,z] in [[-1,-1],[1,1]])
    @rotator = new Rotator @view, () => @draw()
    @ps = null
    @p = Point.from_spherical(0,0)
    @q = null
    @set_ps()
    @draw()

    $(document).on 'keydown', (e) =>
      if [UP, DOWN, LEFT, RIGHT, RETURN, TAB].indexOf(e.which) isnt -1
        e.preventDefault()
      switch e.which
        when UP     then @p_up()
        when DOWN   then @p_down()
        when LEFT   then @p_left()
        when RIGHT  then @p_right()
        when RETURN then @p_evaluate()
        when TAB    then @set_ps()
      @draw()

  p_up: () ->
    if @p.dot(new Point(0,0,1)) < TURN_LIM
      n = @p.cross(new Point(0,0,1))
      @p = @p.rotate(n.dir(),TURN_STEP)
      @q = null
      @p.state = 0

  p_down: () ->
    if @p.dot(new Point(0,0,-1)) < TURN_LIM
      n = @p.cross(new Point(0,0,-1))
      @p = @p.rotate(n.dir(),TURN_STEP)
      @q = null
      @p.state = 0
    
  p_left: () ->
    @p = @p.rotate(new Point(0,0,1),-TURN_STEP)
    @q = null
    @p.state = 0

  p_right: () ->
    @p = @p.rotate(new Point(0,0,1),TURN_STEP)
    @q = null
    @p.state = 0

  p_evaluate: () ->
    @q = Point.outside_point @ps
    @p.state = (if @p.is_inside(@ps, @q) then 1 else 2)

  set_ps: () ->
    cs = ((Math.PI*(Math.random() - 0.5) for j in [0,1]) for i in [1..10])
    cs = cs.concat([cs[0]])
    scale = 0.6
    @ps = (Point.from_spherical(c[0]*scale,c[1]*scale) for c in cs)
    @q = null
    @p.state = 0

  draw: () ->
    @view.clear()
    @draw_sphere()
    @draw_great_arc_chain @ps
      .fill('none')
      .stroke(width: 0.4)
    if @q?
      @draw_great_arc_chain [@p,@q]
        .fill('none')
        .stroke(width: 0.4, color: 'cyan')
    @draw_point(@p).fill(switch @p.state
      when 0 then 'black'
      when 1 then 'lightgreen'
      when 2 then 'orangered')

  great_arc: (a, b, option = 0) ->
    ang = (if option is 2 then 2*Math.PI else
      Math.acos(a.dot(b)) - (if option is 0 then 0 else 2*Math.PI))
    n = a.cross(b).dir()
    m = Math.ceil(ang/Math.PI*40)
    return (a.rotate(n, ang*i/m) for i in [0..m])

  draw_great_arc_chain: (ps, option = 0) ->
    return @view.draw_polyline((
      @great_arc(ps[i],ps[i+1],option) for i in [0..ps.length-2]).reduce(
        (a, b) -> a.concat b))
      .stroke(color: 'black', linecap: 'round', linejoin: 'round', width: 0.2)
      .fill('none')
      
  draw_sphere: () ->
    sphere_path = [[ 1, 0, 0], [ 0,-1, 0], [ 0, 0,-1], [ 1, 0, 0]
                   [ 0, 1, 0], [ 0, 0,-1], [-1, 0, 0], [ 0, 1, 0]
                   [ 0, 0, 1], [-1, 0, 0], [ 0,-1, 0], [ 0, 0, 1]
                   [ 1, 0, 0]]
    return @draw_great_arc_chain(
      new Point(x,y,z) for [x,y,z] in sphere_path)
      .stroke(color: 'gray')

  draw_point: (p) ->
    return @view.draw_point(p).fill('black').stroke('none')

window?.onload = () ->
  app_handler = new AppHandler()
  return
