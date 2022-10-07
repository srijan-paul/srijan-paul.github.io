import { evaluatePolarFunc, toPolarFuncs, vectorToFunc } from "./util.js";
import decompose, { approximateCurve, approximateFunc } from "./fourier.js";
import Graph from "./graph.js";
import RabbitCoords from "./rabbit-coords.js";
import rabbitCoords from "./rabbit-coords.js";

const id = (x) => document.getElementById(x);

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

  sketchObj.points = [];
  let isMousePressed = false;
  space.bindCanvas("mousedown", () => {
    isMousePressed = true;
    space.clear();
    sketchObj.points = [];
  });

  space.bindCanvas("mouseup", () => (isMousePressed = false));

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
    form.stroke("#40739e", 2);
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
 */
function initTrace(width, height, space, coeffs) {
  space.setup({ bgcolor: "#fafafa" });
  const form = space.getForm();

  const { x: xCoeffs, y: yCoeffs } = coeffs;

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
  const xEpicycles = toPolarFuncs(xCoeffs).sort((a, b) => b.radius - a.radius);
  const yEpicycles = toPolarFuncs(yCoeffs).sort((a, b) => b.radius - a.radius);

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
    form.stroke("#eb3b5a", 2);
    for (let i = 1; i < points.length; ++i) {
      form.line([points[i - 1], points[i]]);
    }
  });
}

const CANVAS_SIZE = 350;

const userSketch = { points: [] };
const drawSpace = initDrawCanvas(
  drawCanvas,
  CANVAS_SIZE,
  CANVAS_SIZE,
  userSketch
);
drawSpace.play();

let redrawSpace;
drawCanvas.addEventListener("mouseup", () => {
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
});

function addGraphsToSpace(
  space,
  plots,
  domain = [-1, 1],
  range = [-2, 2],
  center = [225, 225]
) {
  let graphs = new Graph(plots, {
    width: 450,
    height: 450,
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
  const funcSumCanvas = id("fun-sum");
  const funcSumSpace = new CanvasSpace(funcSumCanvas);
  funcSumSpace.setup({ bgcolor: "#fafafa" });

  const f = (x) => 2 * Math.sin(x);
  const g = (x) => Math.cos(2 * x);
  const h = (x) => f(x) + g(x);
  addGraphsToSpace(
    funcSumSpace,
    [
      { fun: f, color: "#b8e994" },
      { fun: g, color: "#78e08f" },
      { fun: h, color: "#b71540" },
    ],
    [-10, 10],
    [-6, 6]
  );
}

funcSumCanvas();

/**
 * Square wave
 */

function squareWaveCanvas() {
  const squareWaveCanvas = id("square-wave-graph");
  const squareWaveSpace = new CanvasSpace(squareWaveCanvas);
  squareWaveSpace.setup({ bgcolor: "#fafafa" });
  addGraphsToSpace(
    squareWaveSpace,
    [{ fun: (x) => Math.sign(Math.sin(x)), color: "#ee5253" }],
    [-10, 10],
    [-3, 3]
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

  const squareWaveApprox = computeApproximateFunction(squareWave, 4);
  const squareWavePlots = [
    { fun: squareWave, color: "blue" },
    { fun: squareWaveApprox, color: "red" },
  ];

  const squareFSCanvas = id("square-wave-fourier-graph");
  let squareFSSPace = new CanvasSpace(squareFSCanvas);
  squareFSSPace.setup({ bgcolor: "#fafafa" });
  addGraphsToSpace(squareFSSPace, squareWavePlots);

  const slider = id("square-fs-slider");
  slider.onchange = () => {
    squareFSSPace.removeAll();
    squareFSSPace.dispose();
    squareFSSPace.pause();
    squareFSSPace = new CanvasSpace(squareFSCanvas);
    squareFSSPace.setup({ bgcolor: "#fafafa" });

    const n = slider.value;
    const squareWaveApprox = computeApproximateFunction(squareWave, n);
    const squareWavePlots = [
      { fun: squareWave, color: "blue" },
      { fun: squareWaveApprox, color: "red" },
    ];
    addGraphsToSpace(squareFSSPace, squareWavePlots);
    squareFSSPace.playOnce();
  };
}

squareWaveFSCanvas();

function drawLineArt(form, xs, ys) {
  form.stroke("red", 2);
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
  const rabbitSpace = new CanvasSpace(rabbitCanvas);
  rabbitSpace.setup({ bgcolor: "#fafafa" });
  const form = rabbitSpace.getForm();
  rabbitSpace.add(() => drawLineArt(form, rabbitX, rabbitY));
  rabbitSpace.playOnce();
}

rabbitCanvas();

const rabbitXFunc = vectorToFunc(rabbitX);
const rabbitYFunc = vectorToFunc(rabbitY);

function rabbitPlotCanvas() {
  const rabbitPlotCanvas = id("rabbit-plot-canvas");
  const rabbitPlotSpace = new CanvasSpace(rabbitPlotCanvas);
  rabbitPlotSpace.setup({ bgcolor: "#fafafa" });

  addGraphsToSpace(
    rabbitPlotSpace,
    [
      { fun: rabbitXFunc, color: "#ee5253" },
      { fun: rabbitYFunc, color: "#2e86de" },
    ],
    [0, 1],
    [0, 500],
    [0, 450]
  );
}

rabbitPlotCanvas();

function rabbitRecreateCanvas() {
  const canvas = id("rabbit-recreate-canvas");
  const space = new CanvasSpace(canvas);
  space.setup({ bgcolor: "#fafafa" });
  const N = 12;
  const xPts = approximateCurve(decompose(rabbitXFunc, N));
  const yPts = approximateCurve(decompose(rabbitYFunc, N));
  const form = space.getForm();
  space.add(() => drawLineArt(form, xPts, yPts));
  space.playOnce();
}

rabbitRecreateCanvas();


function polarSineCanvas() {
  
}

polarSineCanvas();