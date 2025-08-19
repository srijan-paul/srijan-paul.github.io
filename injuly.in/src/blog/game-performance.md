---
tags:
  - personal
  - miscellaneous
  - games 
template: post
date: 2025-08-19
is_blog_post: true
title: "Why are AAA games made for non-existent hardware?"
---

<div class="note" style="float: left; width: 100%; margin-bottom: 20px;">
     <img
         src="/assets/img/oshiro-serious.png" width="100px"
         class="self-image"
         style="float: left;"
     />
     <div style="padding-top: 3px; line-height: 1.5;">
        <b style="font-size: 17px;">injuly:</b>
        Unlike my other posts where the title is rhetorical,
        I don't actually know why AAA games are under-optimized.
        I'm not an industry insider. All research behind this 
        is incidental, and all conclusions speculative.
     </div>
</div>


About a week ago I finished a playthrough of the
Silent Hill 2 Remake; and while the game itself is great, 
I just don't understand who the hyper-realistic graphics are for.

On consoles, the game has two settings in the graphics
tab: 'Performance' and 'Quality'.
The game obviously performs well on the former,
but is almost annoying to play on quality mode even on a PS5:
The FPS is rarely stable, and tops out at 25-28 when it is,
while the lows go as low as 5-10 FPS in many areas.
Whatever, I played it for the story anyway.

In the weekend that followed, my friend came to visit, and
we downloaded [Black Myth: Wukong](https://www.heishenhua.com/) only to
find the same issues as SH2: frame stutters and low FPS on both 'Balanced' and
'Quality' modes.
Once again, the only playable setting is the one that's lowest.
If you've played AAA titles from the last few years,
you'll know that this problem isn't specific to these two games.
It seems to almost a trend in the big-budget studios to make games
that are drop-dead gorgeous, but target hardware that either uses upscalers,
or simply doesn't exist as far as most of the playerbase
is concerned[^1].

<details>
<summary>GPU Upscalers</summary>
Modern GPUs have special hardware to run games at
lower resolutions (say, 960p) and then 'upscale' to 1440p by 
filling in the gaps. PS5 Pro calls it PSSR, NVIDIA calls theirs DLSS, and AMD's
is called FSR (the only one to shaders rather than dedicated AI hardware).
This is an oversimplification, mostly because I myself don't understand how this tech really works.
</details>

There could be a myriad of reasons for this phenomenon, I speculate:

1. Long development-cycles leading studios to have wrong baseline expectations
about the average PC hardware when the game launches.
2. Having to show cutting-edge graphics in gameplay trailers to boost sales,
but falling behind on optimizations because of development crunch.
3. Relying on Unreal engine's nanite (auto-LoD scaling) and Lumen (dynamic ray-traced lighting)
for graphical fidelity, then treating the fallback for lower-end hardware as a second-class use-case

Whatever those reasons may be, I'm miffed to own a current-gen console and
still play games on "performance" mode like I had to back on my 
low-end laptop during high-school.

## Backmatter

[^1]: To clarify, this doesn't take much away from the gameplay. Both SH2:R and Wukong
shine where it matters most – gameplay. Even still, I'd prefer to not be shown graphics
settings that are simply impractical, no matter the hardware.
