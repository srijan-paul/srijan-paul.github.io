import { id, isInt, newSpace } from "../common/util.js";
import Graph from "../fourier-series/graph.js";

Pts.namespace(window);

const color1 = "#40739e";
const color2 = "#f0932b";

const f1 = (x) => x;
const f2 = (x) => 3 - 2 * x;

const coeffs1 = { a: 1, b: -1, c: 0 };
const coeffs2 = { a: 2, b: 1, c: 3 };

/**
 * @param {number} a coefficient of x
 * @param {number} b coefficient of y
 * @param {number} c constant term (intercept)
 * @returns {string} A string representing the equation in form "ax + by = c"
 */
function getEquationText(a, b, c) {
  a = isInt(a) ? a : a.toFixed(2);
  b = isInt(b) ? b : b.toFixed(2);
  c = isInt(c) ? c : c.toFixed(2);

  let ax = a + "x";
  let by = b + "y";
  if (a === 0) {
    if (b === c) {
      by = "y";
      c = 1;
    }

    return `${by} = ${c}`;
  } else if (b === 0) {
    if (a === c) {
      ax = "x";
      c = 1;
    }

    return `${ax} = ${c}`;
  }
  return `${ax} + ${by} = ${c}`;
}

function initGraphCanvas() {
  const canvas = id("canvas-1");
  const space = newSpace(canvas);
  const form = space.getForm();

  let graph = new Graph(
    [
      { fun: f1, color: color1 },
      { fun: f2, color: color2 },
    ],
    {
      width: 350,
      height: 350,
      domain: [-3, 3],
      range: [-5, 5],
    }
  );

  const intersectionPoint = graph.getScaledCoords([1, 1]);
  const update = () => {
    graph.plot(form);
    form.stroke(color1);
    form.fill(color1);
    form.circle([intersectionPoint, [4]]);
  };

  space.add(update);
  space.play();

  const eqnDiv = id("eqn-1");
  const slider = id("graph-slider");
  slider.addEventListener("input", () => {
    const { value } = slider;
    const k = value / 50;

    const xCoeff = 2 - k;
    const yCoeff = 1 + k;
    const rhs = 3;

    eqnDiv.innerText = getEquationText(xCoeff, yCoeff, rhs);
    graph.setFunc(1, (x) => {
      return (rhs - xCoeff * x) / yCoeff;
    });
  });
}

initGraphCanvas();

function initGraphCanvas2() {
  const canvas = id("canvas-2");
  const space = newSpace(canvas);
  const form = space.getForm();

  let graph = new Graph(
    [
      { fun: f1, color: color1 },
      { fun: f2, color: color2 },
    ],
    {
      width: 350,
      height: 350,
      domain: [-3, 3],
      range: [-3, 3],
    }
  );

  const intersectionPoint = graph.getScaledCoords([1, 1]);
  const update = () => {
    graph.plot(form);
    form.stroke(color1);
    form.fill(color1);
    form.circle([intersectionPoint, [4]]);
  };

  space.add(update);
  space.play();

  const eqnDiv1 = id("gj-eqn-1");
  const eqnDiv2 = id("gj-eqn-2");
  const slider1 = id("gj-slider-1");
  const slider2 = id("gj-slider-2");

  // coeffs for x - y = 0
  let a1 = coeffs1.a,
    b1 = coeffs1.b,
    c1 = coeffs1.c;

  // coeffs for 2x + y = 3
  let a2 = coeffs2.a,
    b2 = coeffs2.b,
    c2 = coeffs2.c;

  slider1.addEventListener("input", () => {
    const { value } = slider1;
    const k = value / 300;

    a1 = coeffs1.a + k * a2;
    b1 = coeffs1.b + k * b2;
    c1 = coeffs1.c + k * c2;

    eqnDiv1.innerText = getEquationText(a1, b1, c1);
    if (b1 === 0) b1 = 0.001;
    graph.setFunc(0, (x) => (c1 - a1 * x) / b1);
  });

  slider2.addEventListener("input", () => {
    const { value } = slider2;
    const k = value / 50;

    a2 = coeffs2.a - k * a1;
    b2 = coeffs2.b - k * b1;
    c2 = coeffs2.c - k * c1;

    eqnDiv2.innerText = getEquationText(a2, b2, c2);
    if (b2 === 0) return;
    graph.setFunc(1, (x) => (c2 - a2 * x) / b2);
  });
}

initGraphCanvas2();
