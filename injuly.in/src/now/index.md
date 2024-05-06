---
date: 6th May 2024
template: now
---
# 6th May, 2024

*previous entry: [9th April, 2024](/now/apr-2024)*

I was supposed to write this update in April, but ended up falling sick in the last week.
Tied to my bed no longer, I will spend the month of May writing Zig libraries, and mentoring in Google Summer of Code.
## Making OSS screen capture tools

Last month, I was hacking on a screen-capture library in Zig
<sup>([post](/blog/screen-capture))</sup>.
I ran some experiments trying to take screenshots and record my screen, then encoding the captured frames into GIFs.

Since then, I:
1. Implemented a screenshot app with Rust, Tauri, and SolidJS.
2.  Profiled the app and realized JS is the wrong choice for "native" apps.
3. Ditched JavaScript frameworks and re-wrote the whole thing in C++.

And so, the latest build of ***blink***—a nimble screenshot app—now consumes 10x less memory, and is much snappier.

Down the road, I plan to add GIF and WebP support as well.
For this, I've been exploring quantization and dithering techniques to get the best out of the dated GIF format (This research may or may not result in a blog post).

## Mentoring in Google Summer of Code

Earlier this year, [Hugo](https://www.inf.puc-rio.br/~hgualandi/)—erm, *Professor* Hugo (congratulations!)—emailed me about GSoC.
I'm more than happy to help students get into OSS research, and will thus be mentoring contributors to [the Pallene compiler](https://github.com/pallene-lang/pallene).

Pallene is a research project that compiles a typed dialect of Lua down to native code.
It's hero feature is Lua interop:
libraries written in Pallene, which are usually much faster, can be directly imported into Lua programs.

In 2021, I too was a student developer working on Pallene.
I dug up [my report](blog/gsoc/) on this blog from three years ago, and despite the rusty writing, it was a fun read.
## Reading

- Mistborn: The Hero of Ages – Brandon Sanderson.
- The Devil's Flute Murders – Seishi Yokomizo.

