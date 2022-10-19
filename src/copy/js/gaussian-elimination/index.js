import { id, isInt, newSpace } from "../common/util.js";
import Graph from "../fourier-series/graph.js";

Pts.namespace(window);

const color1 = "#40739e";
const color2 = "#f0932b";

const f1 = (x) => x;
const f2 = (x) => 3 - 2 * x;

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

  const ax = a + "x";
  const by = b + "y";
  if (a === 0) {
    return `${by} = ${c}`;
  } else if (b === 0) {
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
