import { evaluatePolarFunc, toPolarFuncs, vectorToFunc } from "./util.js";
import decompose, { approximateCurve, approximateFunc } from "./fourier.js";
import Graph from "./graph.js";
import rabbitCoords from "./rabbit-coords.js";
import rabbitPolarVectors from "./rabbit-epicycles.js";
import piegonCoords, { pigeonEpicycles } from "./piegon-coords.js";

const id = (x) => document.getElementById(x);

const intersectionObserver = new IntersectionObserver(
  (entries) => {
    for (const entry of entries) {
      if (entry.isIntersecting) {
        entry.target.space.resume();
      } else {
        entry.target.space.pause();
      }
    }
  },
  { threshold: 0.1 }
);

/**
 * Pause the space assosciated with [canvas] when it's not
 * visible in the browser viewport.
 * @param {HTMLCanvasElement} canvas
 * @param {CanvasSpace} space
 */
function pauseWhenOutOfViewport(canvas, space) {
  canvas.space = space;
  intersectionObserver.observe(canvas);
}

/**
 * @param {HTMLCanvasElement} canvas
 * @return {CanvasSpace}
 */
function newSpace(canvas) {
  const space = new CanvasSpace(canvas);
  const { width, height } = canvas;
  space.resize(new Bound(new Pt(width, height)));
  space.setup({ bgcolor: "#fafafa" });
  return space;
}

const sin = Math.sin;
const cos = Math.cos;

const RED = "#ED4C67";
const BLUE = "#1e3799";

Pts.namespace(window);

const drawCanvas = id("draw-canvas");
const redrawCanvas = id("redraw-canvas");

/**
 * @param {HTMLCanvasElement} canvasElement
 * @param {number} width
 * @param {number} height
 * @param {{ points: number[] }} sketchObj
 * @returns Pts.CanvasSpace
 */
function initDrawCanvas(canvasElement, width, height, sketchObj) {
  const space = new CanvasSpace(canvasElement);
  space.resize(new Bound(new Pt(width, height)));
  space.setup({ bgcolor: "#f5f6fa" });
  space.bindMouse();

  let isMousePressed = false;
  const startDraw = () => {
    isMousePressed = true;
    space.clear();
    sketchObj.points = [];
  }
  const endDraw = () => (isMousePressed = false);

  space.bindCanvas("mousedown", startDraw);
  space.bindCanvas("touchstart", startDraw);

  space.bindCanvas("touchend", endDraw);
  space.bindCanvas("mouseup", endDraw);

  let prevTime = -Infinity;
  const captureIntervalMs = 25;
  const captureMousePosition = (time) => {
    if (!time || !isMousePressed) return;
    const dt = time - prevTime;
    if (dt >= captureIntervalMs) {
      prevTime = time;
      const { pointer } = space;
      // If the cursor is EXACTLY at the center of the canvas, we ignore it's coords in the curve.
      // For some reason, regardless of where the user clicks on the canvas,
      // the first few coordinates are always the center of the canvas.
      // (TODO) - figure out the source of this bug.
      if (pointer[0] === space.width / 2 && pointer[1] === space.height / 2)
        return;
      sketchObj.points.push(pointer);
    }
  };

  const form = space.getForm();
  const renderCurve = () => {
    form.stroke(BLUE, 2);
    const pointsInCurve = sketchObj.points;
    for (let i = 1; i < pointsInCurve.length; ++i) {
      const pt = pointsInCurve[i];
      const prevPt = pointsInCurve[i - 1];
      form.line([prevPt, pt]);
    }
  };

  space.add(captureMousePosition);
  space.add(renderCurve);
  return space;
}

const X_AXIS = 0,
  Y_AXIS = 1;

/**
 * Initialiaze the canvas on which a 2D sketch is going to be retraced.
 * @param width Width of the CanvasSpace
 * @param height Height of the CanvasSpace
 * @param space The CanvasSpace to draw on
 * @param coeffs The fourier sine and cosine coefficients for the X and Y traces.
 * @param epicycles cached epicycles
 */
function initTrace(width, height, space, coeffs, epicycles) {
  space.setup({ bgcolor: "#fafafa" });
  const form = space.getForm();

  const xEpicycles = epicycles
    ? epicycles.x
    : toPolarFuncs(coeffs.x).sort((a, b) => b.radius - a.radius);
  const yEpicycles = epicycles
    ? epicycles.y
    : toPolarFuncs(coeffs.y).sort((a, b) => b.radius - a.radius);

  const updateCircles = (vectors, t, positionOnShiftAxis, offset, axis) => {
    // The epicycles that trace the Y-coordinate are offset on the X-axis
    // and vice versa. If I offset an epicycle set in both axes, the lines
    // connecting the tip of the vector-sum to the current coordinate in the
    // curve won't be aligned with one of the coordinate axes.
    // Just a small visual difference.
    const shiftAxis = axis === X_AXIS ? Y_AXIS : X_AXIS;

    const runningSum = [0, 0];
    for (const v of vectors) {
      const [oldx, oldy] = runningSum;
      const [x, y] = evaluatePolarFunc(runningSum, v, t);

      [runningSum[0], runningSum[1]] = [x, y];

      if (typeof offset[shiftAxis] !== "number") {
        offset[shiftAxis] = positionOnShiftAxis - runningSum[shiftAxis];

        // Unlike pen-paper math, the Y axis runs top-to-bottom in
        // most graphics libraries, so we multiply the offset by -1
        // since we want to shift it down.
        if (shiftAxis === X_AXIS) offset[shiftAxis] *= -1;
      }

      if (oldx === 0 && oldy === 0) continue;

      form.stroke("gray", 1);
      form.fill(false);

      const linePts = [
        [oldx, oldy],
        [x, y],
      ];

      if (axis == 1) {
        linePts[0] = [oldy, oldx];
        linePts[1] = [y, x];
      }

      const circleCenter = axis === 0 ? [oldx, oldy] : [oldy, oldx];

      linePts[0][shiftAxis] += offset[shiftAxis];
      linePts[1][shiftAxis] += offset[shiftAxis];
      circleCenter[shiftAxis] += offset[shiftAxis];

      form.circle([circleCenter, [v.radius]]);
      form.stroke("black", 1.5);

      form.line(linePts);
      form.circle([linePts[1], [2]]);
    }
    return runningSum;
  };

  let points = [];
  const xEpicycleOffset = [];
  const yEpicycleOffset = [];

  const xEpicycleCenter = height / 6;
  const yEpicycleCenter = 0.25 * width;
  const updateTrace = (t) => {
    if (t === 0) points = [];

    const xVec = updateCircles(
      xEpicycles,
      t,
      xEpicycleCenter,
      xEpicycleOffset,
      X_AXIS
    );
    const yVec = updateCircles(
      yEpicycles,
      t,
      yEpicycleCenter,
      yEpicycleOffset,
      Y_AXIS
    ).reverse();
    const currentPoint = [xVec[0], yVec[1]];

    // Draw 2 lines joining the tip of each epicycle vector sum to the current point
    // in the trace.
    form.dash();
    form.line([[xVec[0], xVec[1] + xEpicycleOffset[1]], currentPoint]);
    form.line([[yVec[0] + yEpicycleOffset[0], yVec[1]], currentPoint]);
    form.dash(false);

    points.push(currentPoint);
  };

  const dt = 0.002;
  let t = 0;
  const update = () => {
    updateTrace(t);
    t += dt;
    if (t > 1) t = 0;
  };

  space.add(update);
  space.add(() => {
    form.stroke(RED, 2);
    for (let i = 1; i < points.length; ++i) {
      form.line([points[i - 1], points[i]]);
    }
  });
}

const CANVAS_SIZE = 350;

const userSketch = { points: piegonCoords };
const drawSpace = initDrawCanvas(
  drawCanvas,
  CANVAS_SIZE,
  CANVAS_SIZE,
  userSketch
);
drawSpace.play();

let redrawSpace;
const callback = () => {
  const N = 42;
  const points = userSketch.points;
  const xs = points.map((pt) => pt[0]);
  const ys = points.map((pt) => pt[1]);

  const xFunc = vectorToFunc(xs);
  const yFunc = vectorToFunc(ys);

  if (redrawSpace) {
    redrawSpace.dispose();
    redrawSpace.pause();
    redrawSpace.removeAll();
  }

  redrawSpace = new CanvasSpace(redrawCanvas);
  redrawSpace.setup({ bgcolor: "#fafafa" });
  const coeffs = { x: decompose(xFunc, N, 1), y: decompose(yFunc, N, 1) };
  initTrace(CANVAS_SIZE, CANVAS_SIZE, redrawSpace, coeffs);
  redrawSpace.play();
};

drawCanvas.addEventListener("mouseup", callback);
redrawSpace = newSpace(redrawCanvas);
initTrace(CANVAS_SIZE, CANVAS_SIZE, redrawSpace, false, pigeonEpicycles);
redrawSpace.play();

function makeGraph(plots, { width, height, domain, range, center } = {}) {
  width = width || 400;
  height = height || 400;
  return new Graph(plots, {
    width: width,
    height: height,
    domain: domain || [-1, 1],
    range: range || [-2, 2],
    center: center || [width / 2, height / 2],
  });
}

function addGraphsToSpace(
  space,
  plots,
  { domain, range, center, width, height } = {}
) {
  domain = domain || [-1, 1];
  range = range || [-2, 2];
  center = center || [200, 200];
  const graphs = makeGraph(plots, {
    width: width || 400,
    height: height || 400,
    domain,
    range,
    center,
  });
  const form = space.getForm();
  space.add(() => graphs.plot(form));
  space.playOnce();
}

/**
 * Canvas showing the sum of two functions.
 */
function funcSumCanvas() {
  const canvas = id("fun-sum");
  const funcSumSpace = newSpace(canvas);

  const f = (x) => 2 * Math.sin(x);
  const g = (x) => Math.cos(2 * x);
  const h = (x) => f(x) + g(x);
  addGraphsToSpace(
    funcSumSpace,
    [
      { fun: f, color: "#ced6e0" },
      { fun: g, color: "#a4b0be" },
      { fun: h, color: RED },
    ],
    {
      domain: [-10, 10],
      range: [-6, 6],
      width: canvas.width,
      height: canvas.height,
    }
  );
}

funcSumCanvas();

/**
 * Square wave
 */

function squareWaveCanvas() {
  const squareWaveCanvas = id("square-wave-graph");
  const squareWaveSpace = newSpace(squareWaveCanvas);
  addGraphsToSpace(
    squareWaveSpace,
    [{ fun: (x) => Math.sign(Math.sin(x)), color: "#ee5253" }],
    {
      domain: [-10, 10],
      range: [-3, 3],
    }
  );
}

squareWaveCanvas();

/**
 * Fourier approximation of square wave
 */

function squareWaveFSCanvas() {
  const squareWave = (x) => Math.sign(Math.sin(2 * Math.PI * x));

  function computeApproximateFunction(func, n) {
    const approx = approximateFunc(decompose(func, n));
    const periodicApprox = (x) => {
      if (!(x >= 0 && x <= 1)) {
        x = x - Math.floor(x);
      }
      return approx(x);
    };
    return periodicApprox;
  }

  let squareWaveApprox = computeApproximateFunction(squareWave, 4);
  const squareWavePlots = [
    { fun: squareWave, color: "gray" },
    { fun: squareWaveApprox, color: "red" },
  ];

  const canvas = id("square-wave-fourier-graph");
  const space = newSpace(canvas);

  let graph = makeGraph(squareWavePlots);
  const form = space.getForm();
  space.add(() => graph.plot(form));
  space.playOnce();

  const slider = id("square-fs-slider");
  slider.onchange = () => {
    space.setup({ bgcolor: "#fafafa" });

    const n = slider.value;
    squareWaveApprox = computeApproximateFunction(squareWave, n);
    const squareWavePlots = [
      { fun: squareWave, color: "gray" },
      { fun: squareWaveApprox, color: RED },
    ];

    graph = makeGraph(squareWavePlots);
    space.playOnce();
  };
}

squareWaveFSCanvas();

function drawLineArt(form, xs, ys, color = RED) {
  form.stroke(color, 2);
  for (let i = 1; i < xs.length; ++i) {
    const x = xs[i];
    const y = ys[i];

    const x_prev = xs[i - 1];
    const y_prev = ys[i - 1];
    form.line([
      [x_prev, y_prev],
      [x, y],
    ]);
  }
}

const rabbitX = rabbitCoords.xs;
const rabbitY = rabbitCoords.ys;

function rabbitCanvas() {
  const rabbitCanvas = id("rabbit-canvas");
  const rabbitSpace = newSpace(rabbitCanvas);
  const form = rabbitSpace.getForm();
  rabbitSpace.add(() => drawLineArt(form, rabbitX, rabbitY));
  rabbitSpace.playOnce();
}

rabbitCanvas();

const rabbitXFunc = vectorToFunc(rabbitX);
const rabbitYFunc = vectorToFunc(rabbitY);

function rabbitPlotCanvas() {
  const canvas = id("rabbit-plot-canvas");
  const rabbitPlotSpace = newSpace(canvas);

  addGraphsToSpace(
    rabbitPlotSpace,
    [
      { fun: rabbitXFunc, color: BLUE },
      { fun: rabbitYFunc, color: RED },
    ],
    {
      domain: [0, 1],
      range: [0, 500],
      center: [0, canvas.height],
    }
  );
}

rabbitPlotCanvas();

function rabbitRecreateCanvas() {
  const canvas = id("rabbit-recreate-canvas");
  const space = newSpace(canvas);

  let N = 12;
  const trace = {
    x: approximateCurve(decompose(rabbitXFunc, N)),
    y: approximateCurve(decompose(rabbitYFunc, N)),
  };

  const form = space.getForm();

  const slider = id("rabbit-slider");
  slider.onchange = () => {
    N = slider.value;
    (trace.x = approximateCurve(decompose(rabbitXFunc, N))),
      (trace.y = approximateCurve(decompose(rabbitYFunc, N))),
      space.playOnce();
  };

  space.add(() => drawLineArt(form, rabbitX, rabbitY, "#8395a7"));
  space.add(() => drawLineArt(form, trace.x, trace.y));

  space.playOnce();
}

rabbitRecreateCanvas();

function drawPolarFuncs(space, width, height, funcs) {
  const form = space.getForm();
  const center = [width / 2, height / 2];

  const dt = 0.01;
  let t = 0;

  form.fill(false);
  let points = [];

  const updatePoints = () => {
    for (const pt of points) {
      pt[0] += 0.1;
    }

    if (points[0][0] >= space.width) {
      points.shift();
    }
  };

  space.add(() => {
    t += dt;

    const vectorTip = [center[0], center[1]];
    let sumOfRadii = 0;
    for (const { freq, radius } of funcs) {
      const newX = vectorTip[0] + radius * cos(freq * t);
      const newY = vectorTip[1] + radius * sin(freq * t);

      form.stroke("#2c3e50", 2);
      // draw the rotating vector
      form.line([vectorTip, [newX, newY]]);
      form.stroke("#8395a7", 2);
      // draw the circle
      form.circle([vectorTip, [radius]]);

      [vectorTip[0], vectorTip[1]] = [newX, newY];
      sumOfRadii += radius;
    }

    form.stroke("red", 2);
    form.circle([vectorTip, [5]]);
    const newPoint = [center[0] + sumOfRadii, vectorTip[1]];
    form.dash(true);
    form.line([vectorTip, newPoint]);
    form.dash(false);

    points.push(newPoint);
    updatePoints();

    for (let i = 1; i < points.length; ++i) {
      const prevPt = points[i - 1];
      const pt = points[i];
      form.line([prevPt, pt]);
    }
  });
}

function polarSineCanvas() {
  const canvas = id("polar-sine");
  const space = newSpace(canvas);
  pauseWhenOutOfViewport(canvas, space);

  drawPolarFuncs(space, canvas.width, canvas.height, [{ radius: 70, freq: 1 }]);

  space.play();
}

polarSineCanvas();

/**
 * Canvas that shows 2 epicycles
 */

function twoRotatingVectors() {
  const canvas = id("two-rotating-vectors");
  const space = newSpace(canvas);
  pauseWhenOutOfViewport(canvas, space);

  drawPolarFuncs(space, canvas.width, canvas.height, [
    { radius: 50, freq: 1 },
    { radius: 35, freq: 3 },
    { radius: 15, freq: 6 },
  ]);

  space.play();
}

twoRotatingVectors();

/**
 * Rabbit epicycle canvas
 */
function rabbitEpicycle() {
  const canvas = id("rabbit-epicycle");
  const space = newSpace(canvas);

  initTrace(canvas.width, canvas.height, space, false, rabbitPolarVectors);
  space.play();

  pauseWhenOutOfViewport(canvas, space);
}

rabbitEpicycle();
