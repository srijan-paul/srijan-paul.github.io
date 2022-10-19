To revise some Linear Algebra, I've been following the book '[Linear Algebra and its Applications](https://www.goodreads.com/book/show/179699.Linear_Algebra_and_Its_Applications)'
by [Gilbert Strang](https://math.mit.edu/~gs/).

[Gaussian Elimination](https://en.wikipedia.org/wiki/Gaussian_elimination)
is probably the most well known algorithm for solving a system of linear equations.
To understand _why_ it works, and how the row transformations don't affect the final solution,
I had to piece together several ideas from different resources.
In this page, I compose those ideas into a complete picture.

First, we'll revisit Gaussian elimination and find the point of intersection of two lines.
Then, we'll attempt to visualize the transformations that each line goes through at every step.
Finally, I'll try to convince you that this works not only for lines in 2D space,
but for planes in 3D space, and [hyperplanes](https://en.wikipedia.org/wiki/Hyperplane) in N-dimensional space.

## Gaussian elimination revisited

Let's first apply Gaussian elimination to a simple problem like we would in school.
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
the third contains the terms on the RHS.

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
Similarly, we can reduce systems of N equations and M unknowns for arbitrary choices of positive N and M.

## Visualising Gaussian elimination

We'll limit ourselves to a 2-dimensional picture throughout the body of this post.
I trust that you can extend this idea to finding the intersection of planes in 3D space,
and thereby instil the belief that this method works for hyperlanes in N-dimensions.

Below I've plotted the 2 lines on a graph.
Their point of intersection is shown by the green circle.
We know from our previous solution that it is \\((1, 1)\\), but we'll pretend that it is unknown to us.
Try playing around with the slider to see how that affects the line representing the second equation in our system.

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

Moving the slider rotates the yellow line around its point of intersection with the green line.
If you move the slider all the way to the right, you'll notice that the `x` component of the equation vanishes,
and we're left with a simple equality that tells us `y = 1`.

Since the solution to our system lies on this line (the blue dot),
it is clear that the Y-coordinate of our solution is `1` as well.

Now, if we substitute \\(y = 1\\) in the first equation, we get \\(x = 1\\),
thereby leaving us with \\((1, 1)\\) as the coordinate of the dot.
We just arrived at the same solution as earlier using a visual method.
In fact, Gaussian elimination does exactly the same thing that we did by moving the slider.
A system of equations consisting of 2 equations and 2 unknowns represents 2 lines that may intersect at some point.

Lets call these lines \\(l_1\\) and \\(l_2\\), and assume they intersect at point \\(p\\).
One of the lines, say \\(l_1\\), is rotated until it is parallel to one of the coordinate axes (X or Y).
This rotation is done about the point \\(p\\) to ensure that \\(p\\) still stays on \\(l_1\\) post-modification.
We use \\(l_1'\\) to denote the rotated version of \\(l_1\\).

If \\(l_1'\\) is now parallel to the X-axis like in the canvas above,
we immediately have the Y-coordinate of \\(p\\), since points on a line parallel to the X-axis have the same Y-coordinate.
As \\(p\\) lies on both \\(l_1'\\) and \\(l_2\\), we can replace the Y-coordinate of \\(p\\) in the equation of \\(l_2\\)
to obtain its X-coordinate.

Similarly, if \\(l_1'\\) was parallel to the Y-axis instead, we could have solved for the Y-coordinate of \\(p\\).

So far, we rotated only one the lines until it was parallel to a basis vector,
and then solved the other equations by substitution.
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

<!-- lodash -->
<script type = "text/javascript" 
   src = "https://cdn.jsdelivr.net/npm/lodash@4.17.20/lodash.min.js">
</script>

<!-- pts.js -->
<script type="text/javascript" src="https://cdn.jsdelivr.net/gh/williamngan/pts/dist/pts.js"></script>

<!-- script for this post -->
<script type="module" src="/js/gaussian-elimination/index.js"> </script>
