/* Shared tooltip/popup expression evaluator.
 *
 * Evaluates a GL-style expression (JSON array) against a feature's
 * properties object. This is mapgl's own mini-interpreter, not the GL
 * engine: it only ever sees feature.properties, so context-dependent
 * operators (zoom, feature-state, within) are not available.
 *
 * The per-widget evaluateExpression copies in mapboxgl.js, maplibregl.js,
 * both compare bindings, and flowmap.js delegate here; their local bodies
 * remain only as a minimal fallback if this dependency failed to load.
 *
 * Semantics notes (following the GL expression type system):
 * - case / match / coalesce / all / any are lazy: only the branches they
 *   select are evaluated, so an invalid expression in an unselected branch
 *   has no effect.
 * - Conditions (case / all / any / !) must evaluate to booleans; math
 *   operators require numeric operands (no implicit string coercion - use
 *   ["to-number", ...] explicitly); ordered comparisons require two numbers
 *   or two strings. round() rounds halfway values away from zero.
 * - String length / index-of / slice / in count Unicode code points (a
 *   surrogate pair is one position), and index-of supports from_index.
 * - match uses strict typed equality ("1" does not match 1); a label may
 *   be an array of values.
 * - coalesce returns the first non-null/undefined result (0, false, and ""
 *   count as present).
 * - A missing property reads as null for == / != ; ordered comparisons
 *   against null/missing are false.
 * - Any operator failure - type error, malformed form, or a non-finite
 *   numeric result (NaN / Infinity) - warns once per operator and yields ""
 *   so a popup handler never aborts mid-render.
 *
 * The file also runs under plain Node (no DOM) for contract tests: the
 * evaluator attaches to globalThis when window is absent.
 */
(function (root) {
  if (root._mapglEvaluateExpression) {
    return;
  }

  var warned = {};
  function warnOnce(key, message) {
    if (warned[key]) return;
    warned[key] = true;
    if (typeof console !== "undefined" && console.warn) {
      console.warn("[mapgl] " + message);
    }
  }

  function fail(message) {
    throw new Error(message);
  }

  function hasProp(properties, key) {
    return (
      properties != null &&
      Object.prototype.hasOwnProperty.call(properties, key)
    );
  }

  function escapeHTML(value) {
    return String(value)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;")
      .replace(/'/g, "&#39;");
  }

  // undefined (missing property) and null are the same "no value" for
  // equality, matching GL where get on a missing property returns null
  function normalizeNull(value) {
    return value === undefined ? null : value;
  }

  function requireNumber(value, what) {
    if (typeof value !== "number" || !isFinite(value)) {
      fail(
        what +
          " must be a finite number (got " +
          describe(value) +
          '); convert explicitly with ["to-number", ...] if needed',
      );
    }
    return value;
  }

  function requireBoolean(value, what) {
    if (typeof value !== "boolean") {
      fail(what + " must be a boolean (got " + describe(value) + ")");
    }
    return value;
  }

  function finiteResult(value, op) {
    if (typeof value !== "number" || !isFinite(value)) {
      fail('"' + op + '" produced a non-finite result');
    }
    return value;
  }

  // Argument counts (after the operator) for every supported operator:
  // [min, max], max null = unbounded. Checked centrally before dispatch;
  // operators with a pair/ordering grammar (case, match, step, interpolate)
  // do additional structural validation in their branches.
  var ARITY = {
    get: [1, 1],
    has: [1, 1],
    concat: [1, null],
    "to-string": [1, 1],
    "to-number": [1, null],
    "number-format": [1, 2],
    case: [3, null],
    match: [4, null],
    coalesce: [1, null],
    all: [1, null],
    any: [1, null],
    "!": [1, 1],
    "==": [2, 2],
    "!=": [2, 2],
    ">": [2, 2],
    ">=": [2, 2],
    "<": [2, 2],
    "<=": [2, 2],
    in: [2, 2],
    "index-of": [2, 3],
    length: [1, 1],
    slice: [2, 3],
    step: [4, null],
    interpolate: [4, null],
    "+": [2, null],
    "*": [2, null],
    "-": [1, 2],
    "/": [2, 2],
    "%": [2, 2],
    "^": [2, 2],
    abs: [1, 1],
    round: [1, 1],
    floor: [1, 1],
    ceil: [1, 1],
    min: [2, null],
    max: [2, null],
    upcase: [1, 1],
    downcase: [1, 1],
    literal: [1, 1],
    "mapgl-html-escape": [1, 1],
  };

  function checkArity(op, expr) {
    var arity = ARITY[op];
    if (!arity) return; // unknown operators warn in the default branch
    var n = expr.length - 1;
    if (n < arity[0] || (arity[1] !== null && n > arity[1])) {
      fail(
        '"' +
          op +
          '" expects ' +
          (arity[1] === null
            ? "at least " + arity[0]
            : arity[0] === arity[1]
              ? arity[0]
              : arity[0] + " to " + arity[1]) +
          " argument(s), got " +
          n,
      );
    }
  }

  function describe(value) {
    if (value === null || value === undefined) return "null";
    if (Array.isArray(value)) return "an array";
    return typeof value;
  }

  // Unicode code-point helpers: GL string positions count code points, so a
  // surrogate pair (e.g. an emoji) is one position, not two UTF-16 units
  function toCodePoints(s) {
    return Array.from(s);
  }

  function codePointIndexOf(haystack, needle, fromIndex) {
    var hay = toCodePoints(haystack);
    var nd = toCodePoints(String(needle));
    if (nd.length === 0) return -1;
    for (var i = Math.max(0, fromIndex); i + nd.length <= hay.length; i++) {
      var hit = true;
      for (var j = 0; j < nd.length; j++) {
        if (hay[i + j] !== nd[j]) {
          hit = false;
          break;
        }
      }
      if (hit) return i;
    }
    return -1;
  }

  function evaluate(expression, properties) {
    if (!Array.isArray(expression)) {
      return expression;
    }
    if (expression.length === 0 || typeof expression[0] !== "string") {
      // Not an operator form; preserve legacy pass-through for raw arrays
      return expression;
    }
    var operator = expression[0];
    try {
      return applyOperator(operator, expression, properties);
    } catch (e) {
      warnOnce(
        "error:" + operator,
        'tooltip/popup expression operator "' +
          operator +
          '" failed: ' +
          (e && e.message ? e.message : e),
      );
      return "";
    }
  }

  function applyOperator(op, expr, props) {
    var ev = function (x) {
      return evaluate(x, props);
    };
    var i, a, b, value;

    checkArity(op, expr);

    switch (op) {
      /* ---- original five operators (legacy-compatible semantics) ---- */
      case "get": {
        var key = ev(expr[1]);
        return hasProp(props, key) ? props[key] : undefined;
      }
      case "concat":
        return expr.slice(1).map(ev).join("");
      case "to-string":
        return String(ev(expr[1]));
      case "to-number": {
        // GL semantics: try each argument in turn, returning the first
        // value that converts to a finite number
        for (i = 1; i < expr.length; i++) {
          value = Number(ev(expr[i]));
          if (isFinite(value)) return value;
        }
        fail('"to-number" could not convert any argument to a finite number');
        return "";
      }
      case "number-format": {
        value = ev(expr[1]);
        var options = expr[2] || {};
        var locale = options.locale || "en-US";
        var formatOptions = {};
        if (options.style) formatOptions.style = options.style;
        if (options.currency) formatOptions.currency = options.currency;
        if (options.unit) formatOptions.unit = options.unit;
        if (hasProp(options, "min-fraction-digits")) {
          formatOptions.minimumFractionDigits = options["min-fraction-digits"];
        }
        if (hasProp(options, "max-fraction-digits")) {
          formatOptions.maximumFractionDigits = options["max-fraction-digits"];
        }
        if (hasProp(options, "min-integer-digits")) {
          formatOptions.minimumIntegerDigits = options["min-integer-digits"];
        }
        if (options.notation) formatOptions.notation = options.notation;
        if (options.compactDisplay) {
          formatOptions.compactDisplay = options.compactDisplay;
        }
        if (hasProp(options, "useGrouping")) {
          formatOptions.useGrouping = options.useGrouping;
        }
        return new Intl.NumberFormat(locale, formatOptions).format(value);
      }

      /* ---- conditionals (lazy) ---- */
      case "case": {
        // ["case", cond1, out1, ..., fallback]: pairs plus fallback
        if (expr.length < 4 || expr.length % 2 !== 0) {
          fail('"case" requires condition/output pairs plus a fallback');
        }
        for (i = 1; i + 1 < expr.length; i += 2) {
          if (requireBoolean(ev(expr[i]), '"case" condition')) {
            return ev(expr[i + 1]);
          }
        }
        return ev(expr[i]);
      }
      case "match": {
        // ["match", input, label1, out1, ..., fallback]
        if (expr.length < 5 || expr.length % 2 !== 1) {
          fail('"match" requires an input, label/output pairs, and a fallback');
        }
        var input = ev(expr[1]);
        for (i = 2; i + 1 < expr.length; i += 2) {
          var label = expr[i]; // literal (value or array of values), not evaluated
          var matched = Array.isArray(label)
            ? label.indexOf(input) !== -1
            : label === input;
          if (matched) return ev(expr[i + 1]);
        }
        return ev(expr[i]);
      }
      case "coalesce": {
        for (i = 1; i < expr.length; i++) {
          value = ev(expr[i]);
          if (value !== null && value !== undefined) return value;
        }
        return "";
      }

      /* ---- boolean (lazy) ---- */
      case "all": {
        for (i = 1; i < expr.length; i++) {
          if (!requireBoolean(ev(expr[i]), '"all" argument')) return false;
        }
        return true;
      }
      case "any": {
        for (i = 1; i < expr.length; i++) {
          if (requireBoolean(ev(expr[i]), '"any" argument')) return true;
        }
        return false;
      }
      case "!":
        return !requireBoolean(ev(expr[1]), '"!" argument');

      /* ---- comparison ---- */
      case "==":
        return normalizeNull(ev(expr[1])) === normalizeNull(ev(expr[2]));
      case "!=":
        return normalizeNull(ev(expr[1])) !== normalizeNull(ev(expr[2]));
      case ">":
      case ">=":
      case "<":
      case "<=": {
        a = normalizeNull(ev(expr[1]));
        b = normalizeNull(ev(expr[2]));
        if (a === null || b === null) return false;
        var bothNumbers = typeof a === "number" && typeof b === "number";
        var bothStrings = typeof a === "string" && typeof b === "string";
        if (!bothNumbers && !bothStrings) {
          fail(
            '"' +
              op +
              '" requires two numbers or two strings (got ' +
              describe(a) +
              " and " +
              describe(b) +
              ")",
          );
        }
        if (op === ">") return a > b;
        if (op === ">=") return a >= b;
        if (op === "<") return a < b;
        return a <= b;
      }

      /* ---- lookup / test ---- */
      case "has":
        return hasProp(props, ev(expr[1]));
      case "in": {
        a = ev(expr[1]);
        b = ev(expr[2]);
        if (typeof b === "string") {
          return codePointIndexOf(b, a, 0) !== -1;
        }
        if (Array.isArray(b)) {
          return b.indexOf(a) !== -1;
        }
        return false;
      }
      case "index-of": {
        a = ev(expr[1]);
        b = ev(expr[2]);
        var from =
          expr.length > 3
            ? requireNumber(ev(expr[3]), '"index-of" from_index')
            : 0;
        if (typeof b === "string") {
          return codePointIndexOf(b, a, from);
        }
        if (Array.isArray(b)) {
          return b.indexOf(a, from);
        }
        fail('"index-of" requires a string or array to search');
        return -1;
      }
      case "length": {
        value = ev(expr[1]);
        if (typeof value === "string") return toCodePoints(value).length;
        if (Array.isArray(value)) return value.length;
        fail('"length" requires a string or array');
        return 0;
      }
      case "slice": {
        value = ev(expr[1]);
        var start = requireNumber(ev(expr[2]), '"slice" start');
        var end =
          expr.length > 3
            ? requireNumber(ev(expr[3]), '"slice" end')
            : undefined;
        if (typeof value === "string") {
          var cps = toCodePoints(value);
          return (end === undefined ? cps.slice(start) : cps.slice(start, end)).join("");
        }
        if (Array.isArray(value)) {
          return end === undefined ? value.slice(start) : value.slice(start, end);
        }
        fail('"slice" requires a string or array');
        return "";
      }

      /* ---- ramps (numeric outputs only; property-driven input) ---- */
      case "step": {
        if (expr.length < 3 || (expr.length - 3) % 2 !== 0) {
          fail('"step" requires an input, a base output, and stop pairs');
        }
        var stepInput = requireNumber(ev(expr[1]), '"step" input');
        // stop inputs are numeric literals in strictly ascending order;
        // validate all of them before selecting (outputs stay lazy)
        var stepStops = [];
        for (i = 3; i + 1 < expr.length; i += 2) {
          stepStops.push(requireNumber(expr[i], '"step" stop input'));
        }
        for (i = 1; i < stepStops.length; i++) {
          if (!(stepStops[i] > stepStops[i - 1])) {
            fail('"step" stops must be strictly increasing numbers');
          }
        }
        var chosen = expr[2];
        for (i = 0; i < stepStops.length; i++) {
          if (stepInput >= stepStops[i]) {
            chosen = expr[4 + 2 * i];
          } else {
            break;
          }
        }
        return ev(chosen);
      }
      case "interpolate": {
        var type = expr[1];
        var kind = Array.isArray(type) ? type[0] : type;
        if (kind === "cubic-bezier") {
          fail(
            'cubic-bezier interpolation is not supported; use ["linear"] or ["exponential", base]',
          );
        }
        if (kind !== "linear" && kind !== "exponential") {
          fail('unsupported interpolation type "' + kind + '"');
        }
        var base = 1;
        if (kind === "exponential") {
          base = Number(type[1]);
          if (typeof type[1] !== "number" || !isFinite(base) || base <= 0) {
            fail('"exponential" interpolation requires a positive numeric base');
          }
        }
        if (expr.length < 5 || (expr.length - 3) % 2 !== 0) {
          fail('"interpolate" requires an input and stop pairs');
        }
        var x = requireNumber(ev(expr[2]), '"interpolate" input');
        var xs = [];
        for (i = 3; i + 1 < expr.length; i += 2) {
          xs.push(requireNumber(expr[i], '"interpolate" stop input'));
        }
        for (i = 1; i < xs.length; i++) {
          if (!(xs[i] > xs[i - 1])) {
            fail('"interpolate" stops must be strictly increasing numbers');
          }
        }
        var outAt = function (k) {
          return requireNumber(
            ev(expr[4 + 2 * k]),
            '"interpolate" output (popup/tooltip interpolation supports numeric outputs only)',
          );
        };
        // clamp outside the stop range
        if (x <= xs[0]) return outAt(0);
        if (x >= xs[xs.length - 1]) return outAt(xs.length - 1);
        var k = 0;
        while (x > xs[k + 1]) k++;
        var x0 = xs[k];
        var x1 = xs[k + 1];
        var t =
          base === 1
            ? (x - x0) / (x1 - x0)
            : (Math.pow(base, x - x0) - 1) / (Math.pow(base, x1 - x0) - 1);
        return finiteResult(outAt(k) + t * (outAt(k + 1) - outAt(k)), op);
      }

      /* ---- math (arity-checked; numeric operands required; non-finite
       *      results fail) ---- */
      case "+":
      case "*": {
        var acc = op === "+" ? 0 : 1;
        for (i = 1; i < expr.length; i++) {
          var operand = requireNumber(ev(expr[i]), '"' + op + '" operand');
          acc = op === "+" ? acc + operand : acc * operand;
        }
        return finiteResult(acc, op);
      }
      case "-": {
        return finiteResult(
          expr.length > 2
            ? requireNumber(ev(expr[1]), '"-" operand') -
                requireNumber(ev(expr[2]), '"-" operand')
            : -requireNumber(ev(expr[1]), '"-" operand'),
          op,
        );
      }
      case "/":
        return finiteResult(
          requireNumber(ev(expr[1]), '"/" operand') /
            requireNumber(ev(expr[2]), '"/" operand'),
          op,
        );
      case "%":
        return finiteResult(
          requireNumber(ev(expr[1]), '"%" operand') %
            requireNumber(ev(expr[2]), '"%" operand'),
          op,
        );
      case "^":
        return finiteResult(
          Math.pow(
            requireNumber(ev(expr[1]), '"^" operand'),
            requireNumber(ev(expr[2]), '"^" operand'),
          ),
          op,
        );
      case "abs":
        return Math.abs(requireNumber(ev(expr[1]), '"abs" operand'));
      case "round": {
        // GL rounds halfway values away from zero (Math.round rounds
        // toward positive infinity for negative halves)
        value = requireNumber(ev(expr[1]), '"round" operand');
        return Math.sign(value) * Math.round(Math.abs(value));
      }
      case "floor":
        return Math.floor(requireNumber(ev(expr[1]), '"floor" operand'));
      case "ceil":
        return Math.ceil(requireNumber(ev(expr[1]), '"ceil" operand'));
      case "min":
      case "max": {
        var nums = expr.slice(1).map(function (item) {
          return requireNumber(ev(item), '"' + op + '" operand');
        });
        return finiteResult(
          op === "min" ? Math.min.apply(null, nums) : Math.max.apply(null, nums),
          op,
        );
      }

      /* ---- strings ---- */
      case "upcase":
        return String(ev(expr[1])).toUpperCase();
      case "downcase":
        return String(ev(expr[1])).toLowerCase();

      /* ---- misc ---- */
      case "literal":
        return expr[1];
      case "mapgl-html-escape": {
        // mapgl-only operator (html_escape_expr() in R): expression results
        // are inserted as raw HTML, this escapes untrusted values. Missing
        // and null render as "" (matching brace-template behavior), not as
        // the strings "undefined"/"null".
        value = ev(expr[1]);
        return value == null ? "" : escapeHTML(value);
      }

      default:
        warnOnce(
          "unknown:" + op,
          'unknown tooltip/popup expression operator "' +
            op +
            '"; the value was rendered as an empty string.',
        );
        return "";
    }
  }

  root._mapglEvaluateExpression = function (expression, properties) {
    return evaluate(expression, properties);
  };
})(typeof window !== "undefined" ? window : globalThis);
