Try drawing something on the first canvas, and watch two sets of mechanical alien arms retrace your sketch:

<div style="display: flex; gap: 3rem; flex-direction: row;">
   <canvas id="draw-canvas" width="350" height="350" style="border: 1px solid black;"></canvas>
   <canvas id="redraw-canvas" width="350" height="350" style="border: 1px solid black;"></canvas>
</div>

Once you're done with this introduction to Fourier analysis, you'll be capable of making this (and much more) yourself.

The satisfying animation is made possible by the subject of this post - an infinite sum called [the Fourier series](https://en.wikipedia.org/wiki/Fourier_series).
The formula is short, and memorizing it is easy.
And yet, unlike my boring maths professor, deft in the art of [giving terrible lectures](http://atharvaraykar.com/education/lectures.html) and bad quips, I implore you to understand where the concept comes from, and build deeper intuition for it.

To keep you from clicking off this page however, I'll defer the proof and origin of this equation to the second half, and thread some interactive animations through the body of this write-up.

## Adding functions

Surely, you're familiar with the addition of numbers, vectors and matrices.
Adding functions is not so different.
The addition of two functions \\(f\\) and \\(g\\) at input \\(x\\) is simply \\(f(x) + g(x)\\).

Put more formally - \\((f + g)(x) = f(x) + g(x)\\).

Let's visualize this by taking an example.
Assume `f` is \\(2sin(x)\\) and `g` is \\(cos(2x)\\).

Their sum then, can be given by a function \\(h(x) = 2sin(x) + cos(2x)\\).

The graph below plots \\(f\\) and \\(g\\) in shades of green, and their sum, \\(h\\), in red.

<div class="center">
   <canvas id="fun-sum" width="400" height="400"></canvas>
</div>

Note how in some places, the values of \\(f\\) and \\(g\\) are both positive, and their sum is therefore a larger positive number, while in other places, \\(f\\) and \\(g\\) have opposite signs, and their values cancel out to a smaller number.

Through the lens of physics, you could look at the functions as two electromagnetic waves, or just visible light rays oscillating in the domain of time.
The superposition (overlapping) of two such waves, results in a new wave that is the sum of both waves.

When two points in a wave supplement each other to result in a higher amplitude (the y - value), this interaction is termed "constructive interference".
When they cancel each other out, it's termed "destructive interference".

Go through the last two paragraphs again, and try to digest this idea.
Now, imagine if we had to work our way backwards.
Imagine we were given a list containing the (x, y) coordinates of all points along the curve of \\(h\\), where \\(x\\) is time and \\(y\\) is the corresponding output of \\(h\\) at that point in time.
Say we had to come up with two simpler periodic functions that when added, result in \\(h\\).

This is exactly what the Fourier series does.

There are several ways to interpret interference in the real world.
If \\(f\\) and \\(g\\) were sound waves, their constructive interference would make sharper and louder noise, and their destructive interference would lead to produce a quieter sound.
If they were light waves instead, their constructive interference would reveal bright spots on an reflective surface, and destructive would look like dim patches.

Applications of the Fourier series spill into almost every domain - signal processing, image compression, shape recognition, analog transmission, noise cancellation, studying thermodynamic systems, fitting equations to datasets and tracing ugly sketches.

## Decomposing periodic functions.

Imagine you had a machine that could take any food item on one end, and eject its the recipe out the other.
That is essentially what the Fourier series is, except for mathematical functions.

For any periodic function \\(f(x)\\) that has a frequency of \\(\omega_o\\), its Fourier series is defined as:

$$
f(x) = a_0/2 + \sum_{n=1}^{\infty}b_n sin(n\omega_0x) + \sum_{n=1}^{\infty}a_n cos(n\omega_0x)
$$

Meaning that for every periodic function \\(f\\), there exists a set of coefficients \\(a\\) and \\(b\\), such that \\(f(x)\\) can be expressed as an infinite sum of sine and cosine terms of increasing frequencies where the \\(nth\\) sine term has a coefficient of \\(b_n\\) and the \\(nth\\) cosine term has a coefficient of \\(a_n\\). The values of these coefficients are given by the following formula:

$$
a_n = \int_0^T{f(x)cos(nw_0x)}
$$

$$
b_n = \int_0^T{f(x)sin(nw_0x)}
$$

The interval of integration, \\(T\\), is the fundamental period of the function.
\\(T\\) and \\(\omega_0\\) are related by the following equation: \\(\omega_0 = 2\pi/T\\)

If that was too wordy and made little sense to you, that's okay.
We'll prove this equation and explore its origins later in the post.
Until then, an example will help understand this better.

Consider the square wave - a periodic signal that alternates between 1 and -1 depending on it's input.
Formally, it is described like so:

$$
f(t) = 4 \lfloor{t}\rfloor - 2\lfloor2t\rfloor + 1
$$

Here's how it looks when graphed out:

<div class="center">
   <canvas id="square-wave-graph" width="450" height="450"></canvas>
</div>

If we use the first few terms from the Fourier series, we can closely approximate the behavior of this function.
In the following graph, the black curve represents the the square wave and the green curve represents our approximation of it using the series.
You can play with the slider to alter the number of terms we take from the series and see how that changes our approximation.

<div class="center">
   <canvas id="square-wave-fourier-graph" width="450" height="450"></canvas>
      <div className="controls" style="display: 'flex'; gap: 2rem;">
        <input
          type="range"
          min="1"
          max="50"
          value="4"
          id="square-fs-slider"
          style="width: 400px"
        ></input>
      </div>
</div>

Clearly, the more terms we take from this infinite series, the better our approximation is. The Fourier series can be proven to [converge](https://en.wikipedia.org/wiki/Convergent_series)
This means that if we take an infinite number of terms from the series, we can get the _exact_ value of \\(f(x)\\) for any \\(x\\).

Of course, for practical purposes it is impossible to add up infinite terms.
Instead, we decide upon a fixed number of terms and that is generally good enough for a close approximation.

Whenever I say "Fourier series of a function", I mean a series of simple periodic functions that can be added at any given input to approximate the output of the original function at that input.

For the remainder of this post our goal with the Fourier series is to **approximate periodic functions with simpler sine/cosine functions**.

## Drawing with the Fourier series

If you wish to understand how the Fourier series works before seeing it in action, you can skip this section and read ahead to [the proof](#proof), then come back here.

So, How do we go from decomposing time domain functions to recreating sketches ? Imagine you're drawing a sketch on a square sheet of paper that is 1000 x 1000 millimeters in dimensions. You are to draw your sketch, start to finish, without lifting the nib of your pen from the surface of the paper. In other words, your sketch must be _continuous_, with no "breaks" in between.

Assume also that the bottom-left corner of the sheet is the origin. Once you start drawing, I can delineate the position of your pen's tip using a pair of coordinates \\((x, y)\\) at any given point in time.

The \\(x\\) coordinate represents the horizontal distance from the origin, and \\(y\\) vertical, just like a regular cartesian plane. Both the x and y coordinates change as the pen moves on the sheet's surface. Meaning, the position of the x-coordinate of your pen's tip can be written as a function of time.
Say you draw this figure:

<div class="center">
   <canvas id="rabbit-canvas" width="450" height="450"> </canvas>
</div>

If we plot the x and y-coordinates independently as a functions of time, they'll form curves that look like this:

<div class="center">
   <canvas id="rabbit-plot-canvas" width="450" height="450"> </canvas>
</div>

For each curve, we can find a Fourier series that approximates it.
The function \\(x(t)\\) returns the x-position of the pen's tip at time \\(t\\), and \\(y(t)\\) does the same for its y-position.

Let \\(f_x(t)\\) and \\(f_y(t)\\) be the Fourier approximations for \\(x(t)\\) and \\(y(t)\\) respectively.
Then recreating the sketch is as simple as computing the values returned by these functions for a range of values of t, and joining the (x, y) coordinates together with lines.

```typescript
// The "dt" is our time step.
// In the real world, a line is an infinitely long series of points.
// In computers, we take a "snapshot" of the pen's position
// every dt seconds and join these positions with straight lines to
// trace the curve. Smaller values of dt require more computation,
// and yield better results.
const dt = 0.01;
const f_x = fourier_series(x); // type of x is (t: number) => number
const f_y = fourier_series(y); // type of y is (t: number) => number
let prev_point = [f_x(0), f_y(0)];
for (let t = 0.01; t < 1; t += dt) {
  const current_point = [f_x(t), f_y(t)];
  draw_line(prev_point, current_point);
  prev_point = current_point;
}
```

<div class="center">
   <canvas id="rabbit-recreate-canvas" width="300" height="300"> </canvas>
</div>

Keep in mind that `f_x` and `f_y` are really just sums of simpler sine/cosine functions. They're calculated using the Fourier series, and

You may be wondering - the functions \\(x(t)\\) and \\(y(t)\\) aren't periodic, how come we can still decompose them into sine/cosine sums using the Fourier series?
One trick is to set the period to infinity, and compute the series at this limit.

In my code, I just set the period to 1 time unit, and assume that the pen just retraces the drawing again and again. Meaning that \\(x(t + 1) = x(0)\\) . This makes the math a lot easier, and certainly doesn't make a difference in the outcome.

To be more clear, when the pen touches the sheet of paper, the time is assumed to be 0, and when the sketch ends, the time is assumed to be 1 second. Every time point in between is scaled accordingly. This is not necessary of course, you could set the time period to however long it took to draw the first sketch, if that makes things simpler for you.

## Epicycles

The final caveat are the epicycles.
It is easy to just plot the values returned by \\(f_x\\) and \\(f_y\\) on the cartesian plane. But how do we animate this using revolving circles?

If you've followed the contents of this article so far, you already know how to recreate sketches. To animate them, you need to understand [The polar coordinate system](https://en.wikipedia.org/wiki/Polar_coordinate_system).

You can read the wikipedia article, or [this article](https://www.mathsisfun.com/polar-cartesian-coordinates.html) to build some intuition for conversion of points between cartesian and polar coordinates.

In the polar coordinate system, a periodic function is simply a vector that rotates around the origin, and completes one full rotation around itself every \\(T\\) time units.
Look at the graph of the function \\(sin(t)\\) in Polar form for example:

<div class="center">
   <canvas id="polar-sine" width="350" height="350"></canvas>
</div>

Similarly, you can plot any cosine function in the polar coordinate system.
To add two periodic functions together, you take one rotating vector and center it on the tip of the another rotating vector. The end result is something like this:

<canvas id="two-rotating-vectors"></canvas>

Now, we know that it is possible to visualize periodic functions and their sums in the polar coordinate system.
To convert a sketch into an epicycle animation then, all we need is to convert a term in the Fourier series from cartesian to polar coordinates.
Once we have that, we can add up the terms like in the animation above, and figure out the x and y-coordinates using two sets of epicycles, each representing the Fourier approximation for \\(x(t)\\) or \\(y(t)\\).

To do this conversion, we can use the [polar form of the Fourier series](https://en.wikipedia.org/wiki/Fourier_series#Amplitude-phase_form).
Precisely, these are the steps you need to follow:

1. Represent the sketch as a list of points drawn in a period of time.
2. Convert the list of points into a two separate lists, one containing the x-coordinates of the sketch, and other the y.
3. Convert each list into a function (I use [this simple helper](https://github.com/srijan-paul/fourier-sketch/blob/eb2be0f646f3097c6725ab621461ba59bfba4b6b/src/math/util.ts#L58)).
   Now, you have the two functions \\(x(t)\\) and \\(y(t)\\).
4. For each function, find it's Fourier series coefficients. [Here](https://github.com/srijan-paul/fourier-sketch/blob/eb2be0f646f3097c6725ab621461ba59bfba4b6b/src/math/fourier.ts#L16) is how I do it.
5. For each function, [convert the Fourier series coefficients into a set of polar functions](https://github.com/srijan-paul/fourier-sketch/blob/eb2be0f646f3097c6725ab621461ba59bfba4b6b/src/math/util.ts#L93).
6. Using a time-step of `dt`, find the final x and y positions of our approximation, and [draw them on a canvas](https://github.com/srijan-paul/fourier-sketch/blob/eb2be0f646f3097c6725ab621461ba59bfba4b6b/src/components/RedrawCanvas.tsx#L21).

There is a more novel approach to retracing sketches that involves using only one set of epicycles.
It uses [the complex Fourier Series](https://en.wikipedia.org/wiki/Fourier_series#Complex-valued_functions), and is also fewer lines of code.
When you're new to this concept however, it may throw you off balance, especially if you're not familiar with imaginary numbers and the Argand plane.

## Proof

When I set out to find an "intuitive" proof for the Fourier series, all I saw were proofs that begin by stating the equation, and then proving it by finding the coefficients \\(a_n\\) and \\(b_n\\) using integrals.
But where did the equation come from?

Did God whisper it to Joseph Fourier in his dreams?

Did he just happen to run into it by chance?

Surprisingly, the answer is "yes".
Of course, he had an unparalleled instinct for math that he cultivated with years of practice and exploration.
There has to be a certain train of thought that he boarded to arrive at this revelation that any periodic signal can be represented as a sum of simpler harmonics.
But that line of thinking was never publicized, and as you'll see in the next section, there have been people who've come thought of this even before Fourier himself did!

The important part, is that Fourier asked a question that was mocked as stupid and bizarre until he presented a proof. The proof does in fact begin by stating the following hypothesis:

$$
f_o(t) = \sum_{n = 0}^\infty{b_nsin(n\omega_0t)}
$$

Here, \\(f_o\\) is an odd function with a fundamental period of \\(w_0\\).
If we can derive a value for \\(b_n\\) from this equation, we can be convinced that **any odd function can be represented as a sum of sinusoids**.

Now, consider an even function \\(f_e\\) with a period of \\(w_0\\):

$$
f_e(t) = \sum_{n=0}^{\infty}a_n cos(n w_0 t)
$$

If we can derive a value for \\(a_n\\) from this equation, we can be convinced that **any even function can be represented as a sum of co-sinusoids**.

When you combine these two equations with the idea that [any periodic function can be represented as a sum of odd and an even function](https://en.wikipedia.org/wiki/Even_and_odd_functions#Even%E2%80%93odd_decomposition), you get:

$$
f_o(t) + f_e(t) = \sum_{n = 0}^\infty{b_nsin(n\omega_0t)} + \sum_{n=0}^{\infty}a_n cos(n w_0 t)
$$

We can turn the order of this proof, and first say that given any function \\(f(t)\\), we can find it's odd and even parts using the odd-even decomposition rule.
Then, we can represent the odd part as a sum of sinusoids, and the even part as a sum of co-sinusoids.

Now, all that's left is to derive the values for \\(a_n\\) and \\(b_n\\) using the two equations stated above.
This is where I save myself the trouble of writing more LaTeX, and defer you [this excellent proof](http://lpsa.swarthmore.edu/Fourier/Series/DerFS.html) by Swarthmore college.
I know I said I'd walk you through the proof, but I can't do a better job of it than the electronics professors at Swarthmore did already.
I'd hate to repeat their work and not give credit.
If you follow the page I linked, you'll realize that the proof only uses basic calculus and trigonometric identities taught in high school.

## Origins

You'll be surprised to learn that the idea behind the series predates Fourier himself.

2 centuries before Fourier, [Carl Friedrich Gauss](https://en.wikipedia.org/wiki/Carl_Friedrich_Gauss) created several algorithms for his study of astronomy, and the search for the dwarf planet Ceres.
One of these algorithms was the [Fast Fourier Transform](https://en.wikipedia.org/wiki/Fast_Fourier_transform) - a function that is very closely related with the Fourier Series.
However, he never published his work because he believed his method to be an unimportant detail in his method of estimating Ceres' position.

Euler had found applications for decomposing periodic functions with Fourier Series before Fourier was even born.

Half a century before fourier, [Bernoulli](https://en.wikipedia.org/wiki/Daniel_Bernoulli) was investigating the motion of a string.
He proposed the idea that functions can be represented as sums of harmonics.
Nobody at the time believed this to be a general method, and his ideas were left unexplored.

Things changed in 1807, when a french math wizard named Joseph Fourier found himself studying the heat equation in a metal plate.
In his search for a solution, he sought to ask a seemingly absurd question:

_Can we represent any periodic function as a sum of simple sine and cosine functions?_

Precisely, he sought to represent any periodic function \\(f(x)\\) with a frequency of \\(\omega_0\\) , in the following form:

$$
f(x) = (a_0 + a_1 cos(\omega_0 t) + a_2 cos(2\omega_0 t) + ... + a_n cos(n\omega_0t)) + (b_1 sin(\omega_0 t) + b_1 sin(2\omega_0 t) + ... + b_n sin(n\omega_0t)
$$

Revered mathematicians of the time, including Langrange and Laplace, rejected this idea as informal and hand-wavy.
The panel evaluating him said:

_"The manner in which the author arrives at these equations is not exempt of difficulties and...his analysis to integrate them still leaves something to be desired on the score of generality and even rigour."_

Perhaps this was because of a lack of reasoning as to _why_ one should even begin to think of periodic functions this way.

It's not unheard of for mathematical ideas to sprout into existence out of seemingly ridiculous places.
Ramanujan attributed some of his major findings to God, and dipped at the age of 32.

After the Fourier Series was accepted by the scientific populace, it spawned a new field of research, called Fourier analysis.
Developments in this field found everyday use in almost every science.

## Applications

## References

## Backmatter

<!-- mathjax -->
<script id="MathJax-script" async src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js">
</script>

<script src="https://polyfill.io/v3/polyfill.min.js?features=es6">
</script>

<!-- lodash -->
<script type = "text/javascript" 
   src = "https://cdn.jsdelivr.net/npm/lodash@4.17.20/lodash.min.js">
</script>

<!-- pts.js -->
<script type="text/javascript" src="https://cdn.jsdelivr.net/gh/williamngan/pts/dist/pts.js"></script>

<!-- script for this post -->
<script type="module" src="/js/fourier-series/index.js" ></script>
