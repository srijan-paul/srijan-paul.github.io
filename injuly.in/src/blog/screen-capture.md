---
template: "post"
tags: ["zig", "software"]
title: "Frametap: decoding screen capture with Zig"
date: "2024-05-02"
meta: "A guide to using OS native screen capture APIs"
is_blog_post: true
---

<video autoplay muted loop="true">
	<source  src="/assets/video/screencap.webm" />
</video>

*<div style="font-size: calc(var(--font-size) - 1px)">A screen capture tool made with frametap</div>*

I've seen a lot of praise for [Shottr](https://shottr.cc/) and [CleanShotX](https://cleanshot.com/) – two beautiful MacOS apps that capture your screen.
And I've wondered, why aren't there any alternatives that support all devices?[^1]

As it turns out, to make robust screen capture apps we need robust screen capture libraries.
So, I've spent some time figuring out cross-platform screen capture and image encoding. The bulk of my effort has gone into two projects:

1. [Frametap](https://github.com/srijan-paul/frametap): A cross platform screen capture library in Zig.
2. Snapper: An app that can record GIFs and capture screenshots.

With frametap, I want to establish the plumbing atop which anyone can create applications dealing with pixels on a screen – think OBStudio, ShareX, LightShot, EpicPen, etc.
To that end, I'm writing this to document the different screen recording methods on each OS and windowing system.

## Recording frames

The Operating System controls the color of every on-screen pixel.
It sends draw commands to the GPU, which then computes the color for every pixel, and writes this data into a *framebuffer*. Finally, a video card sends the contents of this buffer to a monitor.

The GPU framebuffer cannot be accessed directly[^2], so the only way to get pixel data from the screen is to ask the OS. Overall, it looks something like this:

1. Ask the OS for a list of available displays.
2. Select the display you want to use.
3. Set up filters, like the region of screen to capture and windows to exclude.
4. Ask the OS for pixel data for the specified region.

Straightforward, right?
The devil is in the details. <br>
For every OS (and sometimes OS version) there's a different API you'll have to call.

### MacOS

Newer versions of MacOS have an API called [ScreenCaptureKit](https://developer.apple.com/documentation/screencapturekit/capturing_screen_content_in_macos), which lets you easily capture a video or audio stream. To capture a single frame, I'd recommend using the [CoreGraphics](https://developer.apple.com/documentation/coregraphics) API since it is much simpler to take static screenshots with.

The Objective-C bindings for SCKit are not as well documented, but I was able to find [this patch](https://github.com/obsproject/obs-studio/pull/5875/files) submitted by apple to OBS Studio, and use it as a reference to fit my usecase.
Right now, frametap's SCKit wrapper is a [mere ~350 lines](https://github.com/srijan-paul/frametap/blob/2ea1c095e09b2328e69fdffc403d8725f85a681f/native/screencap.m).

To support older MacOS versions, however, you'll want to use the [AVFoundation](https://developer.apple.com/av-foundation/) framework.

### Windows

For Windows 10 and above there's a well documented [Screen capture API](https://learn.microsoft.com/en-us/windows/uwp/audio-video-camera/screen-capture) for C# and Visual Basic applications.  If you're using C (or Zig, like me), you'll want to use the DirectX APIs (or GDI, if you only want static screenshots). Here are some examples I found:

- [wcap](https://github.com/mmozeiko/wcap) – C, uses DirectX.
-  [DaramCam](https://github.com/daramkun/DaramCam) – C++, supports both GDI and DX.
- [Useful stackoverflow example](https://stackoverflow.com/questions/5069104/fastest-method-of-screen-capturing-on-windows) - C++
- [ScreenCapturer](https://github.com/0x2E757/ScreenCapturer) – C#, DXGI.

### Linux

On linux, the operating system is not directly responsible for deciding whats drawn to the screen.
Instead, a process called the [windowing system](https://en.wikipedia.org/wiki/Windowing_system) renders windows and handles the user's interaction with GUIs.
All programs that render to the screen do so by communicating with the window system.

A run of the mill linux machine will use either X11 or Wayland – the two most popular window systems.

For X11 based linux devices, you can request a process called the `X-Server`  for the screen's image data  using the [`XShmGetImage`](https://linux.die.net/man/3/xshmgetimage) call. Here is a [usage example](https://stackoverflow.com/questions/43442675/how-to-use-xshmgetimage-and-xshmputimage) on stackoverflow.

On Wayland linux, you'll want this library called `pipewire` that can fetch screen image data from the *compositor* – a substitute for the X-server on Wayland. Lucky for us, pipewire has an official [tutorial](https://docs.pipewire.org/page_tutorial5.html) on capturing video frames.

## Encoding frames to image or video

Typically, the OS will give you raw frames in a format like `RGBA` or `BGRA`, where each frame is a byte buffer (`uint8_t*`), and every pixel is represented using 4 bytes – one for each color channel.

To encode these frames into a standard format like PNG, WebP, or GIF, you'll have to look for an encoding library like [fpng](https://github.com/richgel999/fpng), [lodepng](https://lodev.org/lodepng/), or libwebp. 

 Frametap uses lodepng under the hood, and lets you capture a screenshot in just 5 lines of code:

```js
var capture = try core.Capture.create(allocator, null);
defer capture.destroy();

const frame = try capture.screenshot(null);
frame.writePng("screenshot.png");
```


Although experimental, GIF export[^3] is also supported.
I use [cgif](https://github.com/dloebl/cgif/) for GIF encoding, along with [a hand-written color quantizer](https://github.com/srijan-paul/frametap/blob/2ea1c095e09b2328e69fdffc403d8725f85a681f/src/quantize.zig).

WebP support is still on the roadmap, and won't be available for quite some time.

## Drawing on screen

In apps like EpicPen and LightShot you can annotate any region of the screen,
a nifty feature when teaching or streaming your screen.

The trick behind it is surprisingly simple – transparent windows.

When you "draw" on a screen you're really just clicking and moving the cursor on a transparent window. 
Some applications will have two windows – a moveable toolbar from where you can select shapes and brushes; and the transparent canvas on which to draw.
The toolbar can be "hidden" from the capture by asking the OS (or compositor) to mask it away when returning frame data for the screen.

The exact steps to make a window transparent are different for every platform, but most GUI libraries will abstract it away behind a simple configuration flag.


## Backmatter

[^1]: [Flameshot](https://flameshot.org/) deserves a mention – it was the only app I could find that works on all 3 major OSes. Its a little clunky on MacOS, but a very nifty app nonetheless.
[^2]: While windows and MacOS forbid direct access to the GPU, Linux lets you acess the framebuffer via the `/dev/fb0` file. You can even change whats rendered on the screen by writing to it. Try `cat /dev/random > /dev/fb0`.
[^3]: The GIF format is terribly outdated today, and while WebP is a much better format, its adoption is not as widespread. Prefer using `.webp` if you want to share videos on an online platform. No browser will have any trouble rendering a webp file, and its much more efficient.