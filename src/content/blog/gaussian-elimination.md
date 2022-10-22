To brush up some Linear Algebra, I've been following the book '[Linear Algebra and its Applications](https://www.goodreads.com/book/show/179699.Linear_Algebra_and_Its_Applications)'
by [Gilbert Strang](https://math.mit.edu/~gs/). Within it, the author writes:

> Gauss is recognized as the greatest of all mathematicians, but certainly not
> because of this invention, which probably took him ten minutes.
> Ironically, it is the most frequently used of all the ideas that bear his name.

[Gaussian Elimination](https://en.wikipedia.org/wiki/Gaussian_elimination)
is probably the most well known algorithm for solving a system of linear equations.
If you're reading this post, you've probably studied it.
While it's not the most challenging algorithm to code or work out on paper,
I never quite understood how arranging a set of equations in a matrix, then playing with its rows led to the solution.
Most linear algebra is closely related to shapes, curves and spaces.
Certainly there should be a way to visualize the process by which we find where some planes intersect? 
Furthermore, how do we know this works for *any* set of equations in an N-dimensional space?

While Strang's book has been excellent so far, it did not answer my question.
Nor did wikipedia, or mathisfun, or betterexplained.
To understand _why_ it works, and how the row transformations don't affect the final solution,
I had to piece together several ideas from different resources.
And in this page I attempt to explain the intuition behind Gaussian elimination.

First, we'll revisit Gaussian elimination and find the point of intersection of two lines.
Then, we'll attempt to visualize the transformations that each line goes through in our search for their point of intersection.
Finally, I'll try to convince you that this works not only for lines in 2D space,
but for planes in 3D space, and [hyperplanes](https://en.wikipedia.org/wiki/Hyperplane) in N-dimensional space.

## Gaussian elimination revisited

Let's apply Gaussian elimination to a simple problem like we would in a classroom.
The idea is to reduce a system of equations to a state where the solution can be obtained by simple substitution.
An example should help understand this better.
Consider the following system of equations in the 2D coordinate space:

$$
x - y = 0 \newline
2x + y = 3
$$

The example we're using is intentionally simple so we don't lose the forest for the trees.
To solve the system mentioned above, we first represent it as a matrix:

$$
\begin{bmatrix}
1 & -1 & 0 \newline
2 & 1 & 3
\end{bmatrix}
$$

The first column contains the coefficients for \\(x\\), the second for \\(y\\) and
the third contains the terms on the right hand side of each equation.

We're allowed the following operations on this matrix:

- Swapping two rows.
- Multiplying a row by a non-zero scalar.
- Adding a multiple of one row to another.

We begin by applying the operation \\(R_2 \leftarrow R_2 - 2R_1\\), thereby subtracting \\(2R_1\\) from \\(R_2\\).

$$
\begin{bmatrix}
1 & -1 & 0 \newline
0 & 3 & 3
\end{bmatrix}
$$

Now that the coefficient of \\(x\\) in the second equation has been zeroed out,
the system becomes:

$$
x - y = 0 \newline
3y = 3
$$

By back substitution, we get \\((1, 1)\\) as the point of intersection of the two lines.
Similarly, we can reduce systems of N equations and N unknowns for arbitrary choices of positive N.

This method would work for any system that has a solution.
Our choice of equations is simple because we'd like to avoid brain-numbing sensory overload from visualising 10-dimensional systems.
And because I want to muck about fewer lines of JavaScript for visuals in the next section.

## Visualising Gaussian elimination

We'll limit ourselves to 2-dimensions for all visuals.
I trust that you can extend this idea to finding the intersection of planes in 3D,
and thereby instil the belief that this method works for hyperlanes in N-dimensions.

Below I've plotted the same 2 lines we saw above on a graph.
Their point of intersection is shown by the dot.
We know from our previous solution that it is \\((1, 1)\\), but let's pretend that we don't.
Try moving the slider below to see how that affects the yellow line. 

<div class="center">
  <canvas id="canvas-1" width="350" height="350"> </canvas>
  <div className="controls" style="display: 'flex'; gap: 2rem;">
    <input
      type="range"
      min="0"
      max="100"
      value="0"
      id="graph-slider"
      style="width: 300px"
      ></input>
  </div>
  <div id="eqn-1"> 2x + y = 3 </div>
</div>

Moving the slider rotates the yellow line around its point of intersection with the blue line.
If you move the slider all the way to the right, you'll notice that the `x` component of the equation vanishes,
and we're left with a simple equality that tells us `y = 1`.

Since the solution to our system (coordinates of the dot) lies on this rotated yellow line,
it is clear that the Y-coordinate of our solution is `1` as well.

Now, if we substitute \\(y = 1\\) in the first equation (for the blue line), we get \\(x = 1\\),
thereby leaving us with \\((1, 1)\\) as the coordinate of the dot.
We just arrived at the same solution as earlier using a visual method.
In fact, Gaussian elimination does exactly the same thing as moving the slider.
We'll see what that is little later.

A system of 2 equations and 2 unknowns represents 2 lines that may intersect at some point.
Lets call these lines \\(l_1\\) and \\(l_2\\), and assume they intersect at point \\(p\\).
One of the lines, say \\(l_1\\), is rotated until it is parallel to one of the coordinate axes (X or Y).
This rotation is done about the point \\(p\\) to ensure that \\(p\\) still stays on \\(l_1\\) post-modification.
We use \\(l_1'\\) to denote the rotated version of \\(l_1\\).

If \\(l_1'\\) is now parallel to the X-axis like in the canvas above,
we immediately have the Y-coordinate of \\(p\\), since points on a line parallel to the X-axis have the same Y-coordinate.
As \\(p\\) lies on both \\(l_1'\\) and \\(l_2\\), we can replace the Y-coordinate of \\(p\\) in the equation of \\(l_2\\)
to obtain \\(p\\)'s X-coordinate.

Similarly, if \\(l_1'\\) was parallel to the Y-axis instead, we could have solved for the Y-coordinate of \\(p\\).

The only mystery now perhaps, is how Gauss's method achieves rotation by subtracting of rows in a matrix.
We'll circle back to this question later.
For now, let's see if we can avoid back substitution somehow.

So far we've only rotated one line until it was parallel to a basis vector,
and then solved the other equation by substitution.
What if we didn't stop at rotating just one line?
Could we simply rotate both the lines to be parallel to each basis vector and directly obtain the \\((x, y)\\) coordinates?
Unsurprisingly, the answer is yes.
Once again, we begin by visualizing this idea:

<div class="center">
  <canvas id="canvas-2" width="350" height="350"> </canvas>
  <div className="controls" style="display: 'flex'; gap: 2rem; flex-direction: column;">
    <input
    type="range"
      min="0"
      max="100"
      value="0"
      id="gj-slider-1"
      style="width: 300px" 
    ></input>
    <br>
    <input
    type="range"
      min="0"
      max="100"
      value="0"
      id="gj-slider-2"
      style="width: 300px" 
    ></input>

  </div>

  <div id="gj-eqn-1"> x - y = 0 </div>
  <div id="gj-eqn-2"> 2x + y = 3 </div>
</div>

The two sliders control the rotation of \\(l_1\\) (blue) and \\(l_2\\) (yellow).
To solve the system of equations, first move the second slider all the way to the right,
then do the same for the slider above.
In doing this, you first rotated \\(l_1\\) around \\(p\\) such that it was parallel to the X-axis,
and then rotated \\(l_2\\) such that it was parallel to the Y-axis.
The equations of \\(l_1'\\) and \\(l_2'\\) now directly tell you the coordinate of \\(p\\) - \\((1, 1)\\).

This method of tweaking all lines until they're parallel to a basis vector is called [Gauss-Jordan elimination](https://brilliant.org/wiki/gaussian-elimination/).
This variant of Gaussian elimination does not require a back substitution step.

## Rotation by subtraction

This section is a little wordy,
so feel free to take some time and work through each paragraph at a slower pace.

It turns out, that you can rotate \\(l_2\\) around its
point of intersection with \\(l_1\\) by subtracting a multiple of \\(l_1\\) from \\(l_2\\).
Meaning that for some non-zero \\(m\\), the formula for transforming \\(l_2\\) into into \\(l_2'\\) is really just:

$$
l_2' = l_2 - ml_1
$$

Work this out on paper for abitrary choices of \\(l_1\\), \\(l_2\\) and \\(m\\) to convince yourself.
But why is this true?
Consider for a moment what happens when you add two linear equations together.

Say we have a line \\(l1\\) that is described by the following equation:

$$
ax + by = c \ \ \ \ \ (l_1)
$$

The left and right hand sides are equal in value for all choices of \\((x, y)\\) that lie on \\(l_1\\).
If we subtract a number \\(k\\) from both sides of our equation, it changes to:

$$
ax + by - k = c - k \ \ \ \ \ \   (l_2)
$$

Since adding something to two sides of an equation has no effect on it,
**all points that previously satisfied the equation still continue to satisfy it**.

Now, imagine we have another line \\(l_2\\), described by the equation \\(dx + ey = k\\).
All points on \\(l_2\\) satisfy this equality.
If we substitute the \\(k\\) on \\(l_2\\)'s RHS in the LHS of \\(l_1\\),
we get the equation for a new line: 

$$
(a - d)x + (b - e)y = c - k
$$

Now that the coefficients of \\(x\\) and \\(y\\) have changed, it represents a new line altogether.
However, if there exists a point \\((x_p, y_p)\\) that satisfies both \\(l_1\\) and \\(l_2\\),
then we have \\(ax_p + by_p = c\\) and \\(dx_p + ey_p = k\\).

It follows then:

$$
ax_p + by_p - k = c - k \newline
\implies ax_p + by_p - dx_p - ey_p = c - k \newline
\implies (a - d)x_p + (b - e)y_p = (c - k)
$$

Clearly, \\((x_p, y_p)\\) all satisfies the equation for the new line.
Meaning that \\(p\\) lies on \\(l_1\\), \\(l_2\\),
*and* the line that is obtained by adding/subtracting \\(l_1\\) and \\(l_2\\).

We can conclude that it also works when we take a add or subtract multiples of \\(l_1\\) \\(l_2\\),
since multiplying both sides of a line's equation by a non-zero number does not alter the line.

Gaussian elimination arranges the coefficients and constants describing a system of equations in a matrix,
then performs rotation by subtracting rows of the matrix among each other. 
This is equivalent to adding linear equations in much the same way as we did above.

## Gaussian elimination in higher dimensions

At this point, you should be convinced that Gaussian elimination is a solid
approach to solving linear equations in 2-dimensions.
If you aren't, I would advise giving the last section one more look.

You've probably already made the connection that in 3-dimensions,
subtracting the equation of one plane from another, rotates the latter around the its line of
intersection with the former.
In fact, in a system with 3 equations and 3 unknowns, Gaussian elimination just 
tweaks the planes without disturbing their mutual point (or line) of intersection so that
they are parallel with one of the basis vectors (x = 0, y = 0 or z = 0).
We wiggle the planes until the matrix representing our system is in [upper triangular form](https://en.wikipedia.org/wiki/Triangular_matrix).
From there, we can easily backsubstitute the variables and find the solution.

What about an N-dimensional space?
Unfortunately, most three-dimensional creatures are terrible at imagining higher dimensional objects.
That includes me, and so I can't show you what it's like to apply Gaussian elimination in 10-dimensions.
But the idea remains the same: tilt the hyperplanes without disturbing the point where they all intersect.
We can stop once the matrix representing our set of hyperplanes is in upper triangular form, then back-substitute to find where they intersect.
Or, we can keep tilting them until all of them are parallel to a basis vector and obtain the solution like in Gauss-Jordan elimination.

Of course there is more to know about Gaussian elimination than just this, like the use of [elimination matrices](https://ocw.mit.edu/courses/18-06sc-linear-algebra-fall-2011/0903b4b404284cd14b66ecccea103fd4_MIT18_06SCF11_Ses1.2sum.pdf),
or checking for singularity of a system.
But you can find resources on those all over the internet.
My purpose was to merely paint a picture that gives you insight into your mathematical tools.
This just happened to be one that I couldn't find elsewhere.

<!-- lodash -->
<script type = "text/javascript" 
   src = "https://cdn.jsdelivr.net/npm/lodash@4.17.20/lodash.min.js">
</script>

<!-- pts.js -->
<script type="text/javascript" src="https://cdn.jsdelivr.net/gh/williamngan/pts/dist/pts.js"></script>

<!-- script for this post -->
<script type="module" src="/js/gaussian-elimination/index.js"> </script>
