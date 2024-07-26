---
date: 6th May 2024
template: now
---

# 24th July 2024

*previous entry: [May 2024](/now/may-2024)*

I've been making decent progress on some of my hobby projects,
and taking care not to burn out in the process.

To cope with my growing backlog,
I've become comfortable with this idea of managing a stack of on-going projects.
I maintain a pile of three projects that are "active".
At any given time, I can only be working on one of these.
When work gets too mundane, or if I feel I'm risking burnout,
I put the project back in the pile and draw another one. 
The pile must not grow – I get to add a new project *only* when an active
one is finished.

Here's the two projects I last drew from the pile:

## Projects

### [nez](https://github.com/srijan-paul/nez) – NES Emulator

![FCEUX (top) and nez (bottom)](/assets/img/jul-2024/fceux_nez_comparison_1200.png)

Nez can now run games like Final Fantasy 2, Journey to Silius, and Robocop 3.
The emulator is also more accurate than before,
passing more PPU test ROMs than [FCEUX](https://fceux.com/web/home.html), as pictured above.

Currently, I'm working on emulating the APU to finally have some audio.
This turned out to be much harder than I expected, but it wouldn't be so rewarding if it wasn't hard.

As fun as this project is, I wish the Famicom had a more diverse game library.
Most games simply don't hold up well today, though there are some notable exceptions to this. 
To challenge myself some more, I wish to try my hand at a Game Boy Advance 
or PlayStation emulator sometime.
Maybe then I'll have a virtual console that I'll enjoy using myself :)

### Frametap – Screen capture library with GIF support

The screen capture library hasn't seen as much progress as I had hoped for.
I've managed to cobble together a CLI tool that can quantize PNG images and record a GIF 
from any region of the screen.
Some problems still remain – like large GIF sizes and flickering colors.

When I get back to it, I'll be adding WebP support, along with the ability to record on Windows.

## Work

### Sniper – Language agnostic call stack analysis

I usually don't write about my full time job here.
This is a *personal* website, and I like to think that my personhood goes beyond a payslip,
but that doesn't mean my job is boring.
This past week, [Tushar](https://tushar.lol) and I have been hacking on a language agnostic call stack analyzer. 

In simpler terms: for any codebase, it generates a graph of all function calls made inside an expression.
As an example, take this little program:

```py
def gcd(a, b):
  return a if b == 0 else gcd_helper(a, b)

def gcd_helper(a, b):
  return gcd(b, a % b)

def main():
  gcd(48, 18)

# perform stack analysis for this call expression
main()
```

For this Python snippet, the analyzer emits a graph like this:

<img src="/assets/img/jul-2024/d2_graph_gcd.png" width="200">


Not so impressive when you run it for a trivial program,
but this tool proves to be very useful when scanning vulnerable code.

## Other

I recently bought a Switch, and have been enjoying my time playing [Breath of the Wild](https://en.wikipedia.org/wiki/The_Legend_of_Zelda:_Breath_of_the_Wild)
and [Overcooked](https://en.wikipedia.org/wiki/Overcooked).
As for reading, I'm currently working my way through [Safe Houses](https://danfesperman.com/novels/safe-houses/).

Until next time o/

