import { integrate, vectorToFunc } from "./util.js";

const { range } = _

/**
 * Returns the Fourier sine and cosine coefficients for a time domain curve.
 * @param f The function to estimate with Fourier analysis.
 * @param numHarmonics Number of terms to take from the fourier series. (4 by default)
 * @param T The time period of the signal. Assumed to be `1` if not provided.
 * @returns A list of fourier sine and cosine coefficents.
 */
export default function decompose(f, numHarmonics = 4, T = 1) {
  // f(t) = a0+∞∑n=1(ancos(nω0t)+bnsin(nω0t)
  const freq = (2 * Math.PI) / T;

  /**
   * @param fun Either `Math.cos` or `Math.sin`.
   * @param n Serial number of the fourier coefficient to calculate.
   * @return The `n`th fourier sine/cosine coefficient.
   */
  function fourierCoefficient(fun, n) {
    const integralTerm = (t) => f(t) * fun(n * freq * t);
    return (2 / T) * integrate(integralTerm, [0, T]);
  }

  const harmonicRange = range(0, numHarmonics);
  const as = harmonicRange.map((i) => fourierCoefficient(Math.cos, i));
  const bs = harmonicRange.map((i) => fourierCoefficient(Math.sin, i));
  return { sine: bs, cosine: as };
}

/**
 * @param coeffs Fourier sine and cosine coefficients of a curve.
 * @param T Time period of the original function represented by the coefficients.
 * @param dt Time step
 * @returns {number[]} A list of points where each point corresponds to each value of `t`.
 */
export function approximateCurve({ sine, cosine }, T = 1, dt = 0.01) {
  const f = (2 * Math.PI) / T;
  const approximation = [];

  for (let t = 0; t <= T; t += dt) {
    let cosineTerm = cosine[0] / 2;
    for (let i = 1; i < cosine.length; ++i) {
      cosineTerm += cosine[i] * Math.cos(i * f * t);
    }
    const sineTerm = sine.reduce(
      (acc, coeff, i) => acc + coeff * Math.sin(i * f * t),
      0
    );
    approximation.push(sineTerm + cosineTerm);
  }

  return approximation;
}

export function approximateFunc(coeffs, T = 1) {
  const pointsInCurve = approximateCurve(coeffs, T);
  return vectorToFunc(pointsInCurve);
}
