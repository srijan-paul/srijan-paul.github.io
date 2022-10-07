/**
 * Renders a graph onto a give HTML canvas
 */
export default class Graph {
  // The default scaling factor of '3' is chosen arbitrarily.
  xScale = 3;
  yScale = 3;
  // The list of functions to plot.
  // Currently, only R -> R functions are supported.
  funcs = [];
  // Maps a function the color in which it's curve should be drawn.
  // colorOfFunc[i] is the color of the `i`th function.
  colorOfFunc = [];
  // min-x, max-x
  domain = [0, 1];
  // min-y, max-y
  range = [0, 1];
  // This knob controls ow "granular" the graph is.
  dx = 0.01;
  // The position of the origin ((0, 0)) in the CanvasSpace.
  center;
  // A cached list of points in the curve
  // pts[i][j] is the `j`th point of the `i`th function in `this.funcs`
  pts = [];
  // width of the CanvasSpace in pixels
  width;
  // height of the CanvasSpace in pixels
  height;
  // `true` if the points in this graph have been calculated and cached
  isConstructed = false;
  defaultColor = "#3c6382";
  showGrid;
  showCoordinateAxes;
  /**
   * @param funcs A function, or { func, color }, or an array of such descriptors describing a function to be plotted.
   * @param config GraphConfig
   */
  constructor(funcs, config) {
    if (Array.isArray(funcs)) {
      funcs.map((funcPlot) => this.addFunc(funcPlot));
    } else {
      this.addFunc(funcs);
    }
    this.width = config.width;
    this.height = config.height;
    const { domain, range } = config;
    if (typeof domain !== "undefined") {
      this.domain = typeof domain == "number" ? [0, domain] : domain;
    }
    if (typeof range !== "undefined") {
      this.range = typeof range == "number" ? [0, range] : range;
    }
    const domainInterval = this.domain[1] - this.domain[0];
    const rangeInterval = this.range[1] - this.range[0];
    this.dx = config.dx || (this.domain[1] - this.domain[0]) / this.width;
    this.xScale = config.xScale || this.width / domainInterval;
    this.yScale = config.yScale || this.height / rangeInterval;
    this.center = config.center || [this.width / 2, (2 * this.height) / 3];
    this.showGrid = !!config.showGrid;
    this.showCoordinateAxes = !!config.showCoordinateAxes;
    if (config.curveColor) {
      this.defaultColor = config.curveColor;
    }
  }
  /**
   * Add a function to the list of functions plotted by this Graph.
   */
  addFunc(funcPlot) {
    if (typeof funcPlot === "function") {
      this.funcs.push(funcPlot);
      this.colorOfFunc.push(this.defaultColor);
    } else {
      this.funcs.push(funcPlot.fun);
      this.colorOfFunc.push(funcPlot.color);
    }
    // pts[i] stores the points in the `i`th funtion's curve.
    // `funcs`, `colorOfFunc` and `pts` are all arrays that
    this.pts.push([]);
    if (
      !(
        this.funcs.length === this.colorOfFunc.length &&
        this.funcs.length === this.pts.length
      )
    ) {
      throw new Error("Impossible code point reached.");
    }
  }
  /**
   * Reset the scale of the graph.
   * @param xScale scale on the x-axis
   * @param yScale scale on the y-axis
   */
  setScale(xScale, yScale) {
    this.xScale = xScale;
    this.yScale = yScale;
    this.isConstructed = false;
  }
  /**
   * Fill the `pts` array by calculating all points that lie on the curve of `func`.
   */
  construct() {
    const { domain, funcs, dx, xScale, yScale, center } = this;
    for (let i = 0; i < this.pts.length; ++i) {
      const ptsOfFunc = this.pts[i];
      const func = funcs[i];
      let ptIndex = 0;
      for (let x = domain[0]; x < domain[1]; x += dx) {
        const y = func(x);
        // We scale the (x, y) point to the space's dimensions, then
        // offset it by (center[0], center[1]) to make it lie on the accurate
        // position in our graph.
        // Note: (center[0], center[1]) is our origin whereas (0, 0) is the canvas's origin.
        const px = center[0] + x * xScale;
        // "-" instead of "+" because the Y axis is upside down in most
        // graphics libraries, including pts.js
        const py = center[1] - y * yScale;
        ptsOfFunc[ptIndex] = [px, py];
        ++ptIndex;
      }
    }
    this.isConstructed = true;
  }
  /**
   * Renders the X-Y axes.
   * @param form The canvas form to draw with.
   */
  renderXYAxes(form) {
    form.stroke(this.defaultColor);
    const [ox, oy] = this.center;
    form.line([
      [ox, 0],
      [ox, this.height],
    ]);
    form.line([
      [0, oy],
      [this.width, oy],
    ]);
  }
  /**
   * Render the graph on a ptsjs space.
   * @param form The form to draw with.
   */
  plot(form) {
    if (!this.isConstructed) {
      this.construct();
    }
    if (!this.showCoordinateAxes) {
      this.renderXYAxes(form);
    }
    for (let i = 0; i < this.pts.length; ++i) {
      const color = this.colorOfFunc[i];
      form.stroke(color, 2);
      for (let j = 1; j < this.pts[i].length; ++j) {
        form.line([this.pts[i][j - 1], this.pts[i][j]]);
      }
    }
  }
}
