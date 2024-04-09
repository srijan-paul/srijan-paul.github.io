---
template: now
date: 04-02-2024
---
# 9<sup>th</sup> April, 2024

*previous entry: [February 27, 2024](/now/feb-2024)*

During the covid lockdown, I was developing an RPG game.
Back then, I used to code on a windows machine,
and would often share gameplay GIFs on discord servers.
For this, I would use a nifty app called ScreenToGif.
That was four years ago.
Eventually, the lockdown passed.
I abandoned my project, graduated college,
got a job, moved cities, switched to a MacBook,
and wrote an NES emulator.
Once again, I wanted to share GIFs of my progress on the project.

I reached for the same tool I'd used years ago.
ScreenToGif – a GIF recorder with an integrated editor (yay!)... *for windows*.
Bummer.
I ended up recording videos with MacOS's native recorder, then converting them into GIFs with an online tool. 

This wasn't the first time I had to jump through hoops to share a screengrab.
[LightShot](https://app.prntscr.com/en/index.html)—another app I'd used to capture and edit screenshots—is not available on Linux.
[GreenShot](https://getgreenshot.org/) doesn't support MacOS, and [Shottr](https://shottr.cc/) *only* supports MacOS. 
I like using them. But why do I know how to use *four* different apps to take screenshots!?

Why aren't there any decent cross platform screen capture apps?
Not OBS Studio or ShareX,
just something small that lets me make a PNG, GIF or WebP from my screen;
and perhaps let me edit the resulting media.

It turns out that there are some hurdles that make OS agnostic screen-capture annoying to deal with for an app developer, but nothing that can't be overcome with a little grunt work.
Humans can send cars to space.
Surely *this* shouldn't be too hard?

Only one way to find out.
## Meet Frametap 

As of this month, I'm working on [frametap](https://github.com/srijan-paul/frametap/):
A cross platform screen capture library in Zig.
My hope is for Frametap to be the underpinning for a no-nonsense screen grabber.
Features on the roadmap are: 
1. Raw **RGBA frame capture** – in progress, works on MacOS.
2. **PNG** encoding – implemented using lodepng.
3. **GIF** encoding – in progress, uses CGIF.
4. **WEBP** encoding – planned.

Once satisfied with the MVP, I'll attempt making a screengrab tool that hopefully just works.

# Reading

More Sanderson and shin honkaku.

- **Finished**:
	- Mistborn: The Final Empire
- **Currently reading**:
	- Mistborn: The Well of Ascension
	- The Tokyo Zodiac Murders

