/**
 * Calculate the integral using [Trapezoidal rule](https://en.wikipedia.org/wiki/Trapezoidal_rule).
 * @param {(t: number) => number} f A time domain function to integrate (W -> R).
 * @param {[number, number]} interval Interval of integration ([to, from]).
 * @return {number} Area under the curve of `f` between interval[0] and interval[1].
 */
export function integrate(f, interval = [0, 1], dx = 0.01) {
  const [from, to] = interval;
  let areaUnderF = 0;
  for (let x = from; x < to; x += dx) {
    const y = f(x);
    const yNext = f(x + dx);
    areaUnderF += dx * ((yNext + y) / 2);
  }
  return areaUnderF;
}

/**
 * @param {number[]} f  A list of Y-coordinates. The indices are assumed to be X-coordinates.
 * @returns {(t: number) => number} A function that estimates the curve described by the vector f.
 * NOTE: This only works for functions with a domain of [0, 1].
 */
export function vectorToFunc(f) {
  return (t) => f[Math.floor(t * (f.length - 1))];
}

/**
 * Evaluate a polar vector rotating about a point at a given time.
 * @param center The point about which the vector rotates.
 * @param vector A polar vector rotating about [center].
 * @param t The argument at which to evaluate [vector].
 * @returns The (x, y) coordinates of the tip of the vector at time [t].
 */
export function evaluatePolarFunc(center, vector, t) {
  const { radius, freq, phase } = vector;
  const x = center[0] + radius * Math.cos(freq * t - phase);
  const y = center[1] + radius * Math.sin(freq * t - phase);
  return [x, y];
}
/**
 * @param {number} sineCoeff The sine coefficient for the fourier series term.
 * @param {number} cosineCoeff The cosine coefficinet for the fourier series term.
 * @param {number} i The index of the term in the fourier series (0-indexed).
 * @param {number} T The fundamental frequency of the original signal.
 * @returns { {radius: number, phase: number, freq: number} }  a vector rotating in the Polar coordinate space that corresponds to the `i`th signal in the fourier series.
 */
export function toPolarFunc(sineCoeff, cosineCoeff, i, T = 1) {
  const radius = Math.sqrt(sineCoeff ** 2 + cosineCoeff ** 2);
  const phase = Math.atan2(sineCoeff, cosineCoeff);
  return { radius, phase, freq: (i * (2 * Math.PI)) / T };
}

/**
 * Convert the (sine + cosine) terms in the Fourier series to polar vectors.
 * @param coeffs The fourier coefficients for a signal.
 */
export function toPolarFuncs(coeffs, T = 1) {
  return (0, _.zip)(coeffs.sine, coeffs.cosine).map(
    ([sinCoeff, cosineCoeff], i) => {
      if (!(typeof sinCoeff === "number" && typeof cosineCoeff === "number")) {
        throw new Error("toPolarFuncs: Impossible code point reached");
      }
      if (i === 0) return { radius: cosineCoeff / 2, freq: 0, phase: 0 };
      return toPolarFunc(sinCoeff, cosineCoeff, i, T);
    }
  );
}

