/**
 * A wrapper around document.getElementById
 * @param x The element id
 */
export function id(x) {
  return document.getElementById(x)
}

/**
 * @param {HTMLCanvasElement} canvas
 * @return {CanvasSpace}
 */
export function newSpace(canvas) {
  const space = new CanvasSpace(canvas);
  const { width, height } = canvas;
  space.resize(new Bound(new Pt(width, height)));
  space.setup({ bgcolor: "#ffffff" });
  return space;
}

/**
 * @param {number} x 
 * @returns {boolean} `true` if `x` is a integer with no decimal part.
 */
export function isInt(x) {
  return x === Math.floor(x);
}
