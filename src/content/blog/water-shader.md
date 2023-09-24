While browsing the LÖVE2D forums,
I came across a game called [potions](https://alexjgriffith.itch.io/potions)
where you brew potions and save your cat.
The first thing I noticed after booting the game was the reflection effect in the game's water surfaces:

![Water reflections in potions](https://img.itch.zone/aW1nLzEzNDMwMjQ4LmdpZg==/original/P7lZvp.gif)

There are two noteworthy effects that sell the presence of water:
real-time reflections of in-game objects close to the edges, and
the wavy disorted appearance of the reflected image. 

I attempted a recreation of this effect, and was fairly successful:

![My recreation of the water effect from Potions](/assets/img/water-shader/final-water-reflection.gif)

Here, I'll document the entire process of getting a reflection system up and running.
I'm using the Love game framework, and Lua as its scripting language (the original game is in [Fennel](https://fennel-lang.org/)).

If you only want to see the shader logic, skip to [this section](#reflection-effect-with-displacement-shaders).

## Project setup

I'm using some hand-drawn sprites for this demo.
If you're going to follow along, download all images from [here](https://github.com/srijan-paul/shader-blog/tree/main/assets),
and put them under an `assets` directory in your project root.

Every love project starts with `main.lua`.
Since we will be using low-res pixel-art sprites for this demo, the scaling filter is set to `nearest`:

```lua
-- file: main.lua
love.graphics.setDefaultFilter("nearest", "nearest")
```

In another file, `constants.lua`, we initialize some global constants that will be accessed throughout the game:

```lua
-- file: constants.lua
return {
    -- size of each tile in pixels.
    TileSize = 16,
    -- the scale to which each tile is drawn
    TileScale = 5,
    -- #rows of tiles in the game world
    RowCount = 25,
    -- #columns of tiles in the game world
    ColCount = 25,

    -- "enums" for tile kinds
    TileWater  = 1,
    TileGround = 0
}
```

Import the constants in the main script:

```diff
love.graphics.setDefaultFilter("nearest", "nearest")
+ local const = require("constants")
```

The surface of our game-world is drawn using tiles which are drawn every frame.
We can store all tiles in a 2D table, and fill the entire array with `TileGround`.

```lua
-- file: main.lua:

-- contains all tiles in our game-world.
local grid = {}
for i = 1, const.RowCount, 1 do
  grid[i] = {}
  for j = 1, const.ColCount, 1 do
      grid[i][j] = const.TileGround
  end
end
```

We could loop over this table every frame and draw each tile individually with `love.graphics.draw`.
For larger game worlds, however, this will result in way too many unnecessary draw calls.
If we pre-render the world's surface to an off-screen canvas instead,
we can draw that canvas every frame instead and get away with just `draw` call per frame:

```lua
-- file: main.lua
-- @returns a canvas representing the surface of our game-world.
local function get_ground_tiles_layer(screen_width, screen_height)
  -- create an empty canvas
  local canvas = love.graphics.newCanvas(screen_width, screen_height)
  local dirt_tile = love.graphics.newImage("assets/dirt-tile.png")
    
  -- render all ground tiles on the canvas.
  canvas:renderTo(function()
    for i = 0, const.RowCount - 1, 1 do
      for j = 0, const.ColCount - 1, 1 do
        local x = j * (const.TileSize * const.TileScale)
        local y = i * (const.TileSize * const.TileScale)
        local tile_kind = grid[i + 1][j + 1]
        if tile_kind == const.TileGround then
          love.graphics.draw(dirt_tile, x, y, 0, const.TileScale, const.TileScale)
        end
      end
    end
  end)

  return canvas
end


```

Now, we can initialize the canvas at boot time, and then draw it every frame:

```lua
-- file: main.lua
local ground_tiles
function love.load()
  local screen_width  = love.graphics.getWidth()
  local screen_height = love.graphics.getHeight()
  ground_tiles = get_ground_tiles_layer(screen_width, screen_height)
end

function love.draw()
  love.graphics.draw(ground_tiles)
end
```

If we launch what we have so far using the `love` command, it should look like this:

![Ground tiles](/assets/img/water-shader/all-ground-tiles.png)

## Adding Water

In a real game, bodies of water are either defined in a large table called tile-map,
or generated procedurally. For a small demo, we can just store the bounds of our water-body in a table:

```lua
-- file: main.lua
local lake = {
  begin = { row = 4, col = 3 },
  width = 5,
  height = 3 
}
```

Then, modify the existing grid-generation code to account for water-tiles:

```diff
 for i = 1, const.RowCount, 1 do
   grid[i] = {}
   for j = 1, const.ColCount, 1 do
+    if i >= lake.begin.row and i < lake.begin.row + lake.height and
+        j >= lake.begin.col and j < lake.begin.col + lake.width then
+      grid[i][j] = const.TileWater
+    else
       grid[i][j] = const.TileGround
+    end
   end
```

If we run our program now, we'll see a black void in the center of our map: 

![Water-less void](/assets/img/water-shader/waterless-void.png)

This happens because the `ground_tiles` canvas only draws a tile when there is a `TileGround`
in the current grid cell.

I wanted my water to be animated, so I drew a simple sprite with four frames:

<img src="/assets/img/water-shader/water-tile.gif" alt="Water tile sprite" width="200px" height="200px" />

In the project directory, I have each frame saved as `assets/water-tile1.png`, `assets/water-tile2.png`, etc.

To get this water on to the screen, we'll place use another canvas that is drawn *under* the ground canvas.
The code for this animated canvas can be put into its own module:

```lua
-- file: water.lua
local const = require("constants")

-- helper function to load each frame in the animation as an Image object`
local function load_water_tile(index)
  assert(index >= 1 and index <= 4)
  local file_path = "assets/water-tile" .. index .. ".png"
  local water_tile = love.graphics.newImage(file_path)
  return water_tile
end

-- water_tile_imgs[i] = i-th frame in the animation
local water_tile_imgs = {
  load_water_tile(1),
  load_water_tile(2),
  load_water_tile(3),
  load_water_tile(4)
}
```

Just as before, we can either iterate over all water tiles and draw each one separately,
or use one canvas for each frame that will draw all tiles. 
Using the second approach, the water animation will cycle through 4 canvases.
Each canvas will display one sprite repeated enough times to cover the entire screen.

```lua
-- file: water.lua

-- create a canvas that draws `tile_img` in a repeating pattern.
local function make_water_layer(tile_img, n_rows, n_cols)
  local canvas = love.graphics.newCanvas(800, 600)
  love.graphics.setColor(1, 1, 1)
  canvas:renderTo(function()
    for i = 1, n_rows do
      for j = 1, n_cols do
        local x = (j - 1) * (const.TileSize * const.TileScale)
        local y = (i - 1) * (const.TileSize * const.TileScale)
        love.graphics.draw(tile_img, x, y, 0, const.TileScale, const.TileScale)
      end
    end
  end)
  return canvas
end

-- Make 4 canvases, one for each frame of the water animation.
local function make_water_layers()
  local water_layers = {}
  for i = 1, 4 do
    water_layers[i] = make_water_layer(water_tile_imgs[i], const.RowCount, const.ColCount)
  end
  return water_layers
end
```

Now that we have these helpers,
we can manage the state for all water tiles in one object:

```lua
-- file: water.lua
local Water = {}

function Water:new()
  local w =  {}
  setmetatable(w, self)
  self.__index = self
  self.frames = make_water_layers()
  self.current_frame = 1
  self.frame_duration = 0.15
  self.current_frame_time = 0
  return w
end

function Water:draw()
  local frame = self.frames[self.current_frame]
  love.graphics.draw(frame)
end

function Water:update_water(dt)
  self.current_frame_time = self.current_frame_time + dt
  if self.current_frame_time >= self.frame_duration then
    self.current_frame_time = 0
    self.current_frame = (self.current_frame + 1)
    if self.current_frame > #self.frames then
      self.current_frame = 1
    end
  end
end

return Water
```

We could have kept the ground and water tiles on the same layer using a single canvas.
The reason for not doing this should become clear once we write the distortion shader.

For now, we can initialize the water surface in `love.load` (`main.lua`):

```diff
-local ground_tiles
+local ground_tiles, water_tiles
 function love.load()
   local screen_width  = love.graphics.getWidth()
   local screen_height = love.graphics.getHeight()
   ground_tiles = get_ground_tiles_layer(screen_width, screen_height)
+  water_tiles  = Water:new()
 end
```

Modify the draw function to render the water surface (`main.lua`):

```diff
function love.draw()
+ water_tiles:draw()
  love.graphics.draw(ground_tiles)
end
```

And update the water tiles on every time-step:
```lua
-- file: main.lua
function love.update(dt)
  water_tiles:update_water(dt)
end
```

Now, the void in our map should be filled with animated water:

![Animated water](/assets/img/water-shader/bland-water.gif)

The borders of our water body look too straight and unnatural right now, and there is no sense of depth.
We can fix that by drawing borders on top any water tile that is adjacent to a ground tile.

For that, we need to draw some sprites for each border-type (left/right/top-left/top-right), and render them on top of the right tiles.
[The code](https://github.com/srijan-paul/shader-blog/blob/e3aa7be34abbaf1512004987edbaf2232d23ee1e/main.lua#L72) for this is boring, and not the point of this post, so I will not explain it here. 

After adding borders, the water should look like this:

![Water body with borders](/assets/img/water-shader/water-with-border.gif)

## Drawing reflections

We could, if we wanted to, have an isometric pixel-art game and do fancy ray-tracing to show reflections.
However, there is a much simpler trick used by many games before ray-tracing was feasible –
drawing a copy of every entity on reflective surfaces.

First, we'll need a class that represents a drawable sprite:
```lua
-- file: sprite.lua

local Sprite = {}
Sprite.__index = Sprite

function Sprite:new(quad, scale)
  scale = scale or 1
  local sprite = {
    quad = quad,
    scale_x = scale,
    scale_y = scale,
    w = quad:getWidth() * scale,
    h = quad:getHeight() * scale,
    rot = 0
  }
  setmetatable(sprite, Sprite)
  return sprite
end
```

A sprite object stores an image to render, and its scale, width, and height.
Using this information, we can write two `draw` methods, one to draw the image itself,
and another to draw its reflection.

A reflection looks exactly the same as the image itself, just flipped vertically.

```lua
-- file: sprite.lua
function Sprite:draw(x, y)
  love.graphics.draw(
    self.quad,
    x - self.w / 2,
    y - self.h / 2,
    self.rot,
    self.scale_x,
    self.scale_y)
end

function Sprite:draw_reflection(x, y)
  love.graphics.draw(
    self.quad,
    x - self.w / 2,
    y + self.h * 1.5,
    self.rot,
    self.scale_x,
    -- notice the negative scale:
    -self.scale_y)
end

-- export the Sprite class
return Sprite
```

Now, we can import the sprite class in `main.lua`:

```diff
local const = require("constants")
local Water = require("water")
+local Sprite = require("sprite")
```

and define a tree object with X-Y coordinates and a sprite:

```lua
-- file: main.lua

local tree_image = love.graphics.newImage("assets/tree.png")
local tree = {
  sprite = Sprite:new(tree_image, 4),
  x = 300,
  y = 135
}

-- function love.draw()
-- ...
```

We then update the `love.draw` function to draw the tree and its reflection:

```diff
function love.draw()
  water_tiles:draw()
  love.graphics.draw(ground_tiles)
+ tree.sprite:draw(tree.x, tree.y)
+ tree.sprite:draw_reflection(tree.x, tree.y)
end
```

Launching the game again, we see:

![Incorrect reflection](/assets/img/water-shader/incorrect-reflection.png)

Hmm, there's something wrong with the reflection.
Try looking closer, and you'll notice that the reflection is drawn *over* the ground,
as if it were above the lake.

To restrict its sprite to the lake's bounds, we simply change the order of drawing so that
the water is drawn first, then all the reflections, and then the ground.
This way, tiles from the ground canvas will hide parts of the reflection image:

```diff
 function love.draw()
   water_tiles:draw()
+  tree.sprite:draw_reflection(tree.x, tree.y)
   love.graphics.draw(ground_tiles)
   tree.sprite:draw(tree.x, tree.y)
-  tree.sprite:draw_reflection(tree.x, tree.y)
 end
```

With that change, the reflection at least looks somewhat believable:

![Correct reflection](/assets/img/water-shader/reflection-no-shader.gif)

And yet, it doesn't look very convincing.
For one, the color of the reflection is exactly the same as that of the object.
The surface of a lake isn't fully reflective, and will absorb more of red/green than blue.
We want the reflected image to have a bit of a blue tint.

Moreover, the water surface is moving, but the reflection is idle.
Ideally, moving water should distort the image. 

## Reflection effect with displacement shaders 

Right now, our render loop is pretty straightforward:

1. Fill the entire screen with water tiles.
2. Draw reflections of all in game objects (`scale = scale * -1`).
3. Draw ground tiles.
4. Draw all objects in our game world.

Separating each layer into its own canvas allows us to apply a shader globally,
draw a layer while that effect is active, and then remove the shader so
that other objects are drawn normally.

A shader is code that runs on the GPU, and applies an effect to every pixel in parallel (*).
[This post](https://blogs.love2d.org/content/beginners-guide-shaders) is a good introduction to shaders in Love2D.
I recommend going through it before reading ahead.

Assuming you know the very basics of shaders, this one should be straightforward:

```lua
local reflection_shader = love.graphics.newShader [[
    vec4 effect(vec4 color, Image texture, vec2 uv, vec2 pixel_coords) { 
      vec4 pixel_color = Texel(texture, uv);
      pixel_color.b = clamp(pixel_color.b + 0.2, 0.0, 1.0);
      return pixel_color;
    }
]]
```

The `effect` function receives the current color set using `love.graphics.setColor`,
the image being drawn to the screen (`texture`),
coordinates of the current pixel inside the image normalized to [0, 1] (`uv`),
and absolute coordinates of the current pixel inside the window. (`pixel_coords`).
It's purpose is to return a modified color for the pixel at `uv` in `texture`.

`Texel(texture, uv)` gives us the color of the pixel at coordinate `uv` inside `texture`.
We then add `0.2` to the blue channel of the pixel and return  it.

We can apply this shader before drawing our reflections layer like so:

```diff
function love.draw()
  water_tiles:draw()
+ love.graphics.setShader(reflection_shader)
  tree.sprite:draw_reflection(tree.x, tree.y)
+ love.graphics.setShader()
  love.graphics.draw(ground_tiles)
  tree.sprite:draw(tree.x, tree.y)
end
```

Now, our reflections have a blue tint:

![Reflection with a blue tint](/assets/img/water-shader/reflection-blue-tint.gif)

We also want the reflected image to twist and distort as if there are waves underneath.
The final reflection should look like this:

![Final result](/assets/img/water-shader/final-water-reflection.gif)

This effect can be realized with one simple trick – for every `(X, Y)` coordinate in the image,
instead of drawing the the pixel at `(X, Y)`, draw the pixel at `(X + dx, Y + dy)`.
Tweaking the values of `dx` and `dy` will lead us to various kinds of displacements.

Remember that our normalized `(X, Y)` coordinates are in the range `[0, 1]`,
so we must not budge them too much, `dx` and `dy` must be small numbers.

To sell the effect of a smooth water surface, the values of `dx` and `dy`
must change smoothly as we go in one direction along the canvas.

Moreover, the distortion must update with time, and it should do so smoothly without any jitter.
When you want a smoothly varying wave over time, sine waves can be a good bet.

To compute `dx`, I will be using the following equation:

```
dx = wave_height 
   * sin(frequency*(X + speed * time))
   * cos(frequency*(X + speed * time))
```

`wave_height` is the *amount* of distortion.
The higher the `height`, the more streching/shrinking there is in the waves.
`frequency` determines how far apart two waves are spaced.
`time` always increases monotonically by itself.
`speed` tells us how fast the distortion effect updates.

It might not be intuitive immediately.
I recommend that you open [this graph](https://www.desmos.com/calculator/k8qkzkijab) and play around with the sliders on the left
to see how each parameter changes the shape of the graph.

Parameters like `wave_height`, `time`, etc. are uniforms that will be sent to the shader from our Lua program.
The updated shader is:

```c++
//  These are passed to the shader from the Lua script.
uniform float time;
uniform float wave_height;
uniform float wave_speed;
uniform float wave_freq;


vec4 effect(vec4 color, Image texture, vec2 uv, vec2 pixel_coords) { 
  // Displace the `x` coordinate. 
  uv.x +=
    sin((uv.y + time * wave_speed) * wave_freq)
    * cos((uv.y + time * wave_speed) * wave_freq * 0.5)
    * wave_height;

  // Displacement in `y` is half that of `x`.
  // Displacing `x` and `y` equally looks unnatural
  uv.y +=
    sin((uv.x + time * wave_speed) * wave_freq)
    * cos((uv.x + time * wave_speed) * wave_freq * 0.5)
    * wave_height * 0.5;

  vec4 pixel = Texel(texture, uv);
  // apply a blue tint to the reflection
  pixel.b += 0.2;
  return pixel;
}
```

We update the `love.draw` function yet again to send these parameters to the shader.

```diff
function love.draw()
  water_tiles:draw()
+
+ local time = love.timer.getTime()
+ reflection_shader:send("time", time)
+ reflection_shader:send("wave_height", 0.02)
+ reflection_shader:send("wave_speed", 0.1)
+ reflection_shader:send("wave_freq", 45.0)
+
  love.graphics.setShader(reflection_shader)
... 
```

With that change, the reflection looks a lot more natural:

![Reflection with displacement](/assets/img/water-shader/final-water-reflection.gif)

I'm happy with this re-creation, so I'll stop here.
The only thing that is probably missing is the shadow around the edges of the lake.
That can also be achieved by adding drawing some borders on top of water tiles near the edges,
and then applying the same shader with different parameters:

![Shadows around the water body](/assets/img/water-shader/water-shadows.png)

## Backmatter

I recently found out about [this write-up](https://alexjgriffith.itch.io/potions/devlog/588358/the-one-where-water-is-made) by Alex Griffith – author of Potions.
Interestingly, their approach to the shader was very different from the one I took.
The shader used in the game uses a mask and a noise-map to decide the amount of distortion on each pixel.

