(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
const shapes = require('./shapes');

module.exports = (regl, size, detail, center) => {
	console.log(detail)
	const maxPitch = 1000;
	const extend = arr => Array(detail).fill(1)
		.map((x, i) => arr[i % arr.length]);	
	return regl({
	  vert: `
		  precision highp float;

		  attribute float que, pitch, freq;
		  attribute vec3 pos;
		  

			uniform mat4 projection, view;
		  uniform float beats[16];
		  uniform float tick, volume;

		  varying vec3 vpos;
		  varying float vpitch, vfreq, intensity;

		  void main () {
		    vpitch = pitch;
		    vfreq = freq;
		    vpos = pos;

		    intensity = 0.0;
				float bin = floor(8.0 * (1.0 + pos.x));

		    for (int i = 0; i < 16; ++i) {
		      intensity += beats[i];
		    }
		    gl_PointSize = 5.0;
		    gl_Position = projection * view * vec4(pos + pos * freq * 0.3 + pos * que + pos * intensity * 0.1, 1);
		  }
	  `,

	  frag: `
	    precision highp float;
	    uniform vec4 pitches;

	    varying vec3 vpos;
	    varying float vpitch, vfreq, intensity;

	    void main () {
	    	vec3 color = vec3(0.7 - abs(vpos * 2.0).x, 0.7 - abs(vpos * 2.0).y, 1);
	      gl_FragColor = vec4(color + color * vfreq * 0.1, 1);
	    }
	  `,

	  attributes: {
	  	pos: shapes.circle(size, detail).positions
	  		.map(([ x, y, z ]) => ([ x + center[0], y + center[1], z + center[2] ])),
	    que: ({ cepstrum }) => new Float32Array(extend(cepstrum)),
	    pitch: ({ pitches }) => new Float32Array(extend(pitches.map(x => x / maxPitch))),
	    freq: ({ freq }) => new Float32Array(extend(freq.map(x => x / 100)))
	  },
	  uniforms: {
	    pitches: ({ pitches }) => pitches.slice(0, 4).map(x => x / maxPitch),
	    tick: regl.context('tick'),
	    volume: regl.context('volume'),
	  },
	  elements: shapes.circle(1, detail).cells,
	  primitive: 'triangle strip'
	})
}
},{"./shapes":3}],2:[function(require,module,exports){
const glsl = require('glslify');
const regl = require('regl')()
const camera = require('regl-camera')(regl, { 
	distance: 1.5,
	theta: Math.PI * .5,
	mouse: true,
	center: [0, 0, 0]
})

const circle = require('./circle');

let drawCircle = circle(regl, .3, 1000, [0, 0, 0]);

require('regl-audio/microphone')({
  regl,
  beats: 16,
  name: '',
  maxPitch: 1000,
  minPitch: 500,
  pitches: 1000,
  // beatThreshold: .1,
  done: (microphone) => {

    regl.frame(({ tick }) => {	
			microphone(({ freqTexture, timeTexture, beats, pitches, volume, cepstrum }) => {
				camera(() => {
					regl.clear({ color: [0, 0, 0, 1], depth: true })
          drawCircle();
					// points,point,lines,line,triangles,triangle,line loop,line strip,triangle strip,triangle fan
				})
			})
		})
  }
})
},{"./circle":1,"glslify":8,"regl":17,"regl-audio/microphone":14,"regl-camera":16}],3:[function(require,module,exports){
const { cos, sin, max } = Math;

const circle = (radius, detail) => {
	const { x, y } = { x: 0, y: radius };
  const theta = i => (i / (detail - 1)) * 2 * Math.PI;

  let positions = [[0, 0, 0]];
  let cells = [];

  for (let i = 0; i < detail; i++) {
  	const point = [ 
      x * cos(theta(i)) - y * sin(theta(i)),
      y * cos(theta(i)) + x * sin(theta(i)),
      0
    ];

    if (i % 2 === 0) {  cells.push(0)  }
    if (i < detail - 1) { positions.push(point) }

  	cells.push(max((i + 1) % detail, 1))
  }

	return { positions, cells };
}

module.exports = { circle }
},{}],4:[function(require,module,exports){
/**
 * Real values fourier transform.
 *
 * @module  fourier-transform
 *
 */

module.exports = function rfft (input, spectrum) {
	if (!input) throw Error("Input waveform is not provided, pass input array.");

	var N = input.length;

	var k = Math.floor(Math.log(N) / Math.LN2);

	if (Math.pow(2, k) !== N) throw Error("Invalid array size, must be a power of 2.");

	if (!spectrum) spectrum = new Array(N/2);

	//.forward call
	var n         = N,
		x         = new Array(N),
		TWO_PI    = 2*Math.PI,
		sqrt      = Math.sqrt,
		i         = n >>> 1,
		bSi       = 2 / n,
		n2, n4, n8, nn,
		t1, t2, t3, t4,
		i1, i2, i3, i4, i5, i6, i7, i8,
		st1, cc1, ss1, cc3, ss3,
		e,
		a,
		rval, ival, mag;

	reverseBinPermute(N, x, input);

	for (var ix = 0, id = 4; ix < n; id *= 4) {
		for (var i0 = ix; i0 < n; i0 += id) {
			//sumdiff(x[i0], x[i0+1]); // {a, b}  <--| {a+b, a-b}
			st1 = x[i0] - x[i0+1];
			x[i0] += x[i0+1];
			x[i0+1] = st1;
		}
		ix = 2*(id-1);
	}

	n2 = 2;
	nn = n >>> 1;

	while((nn = nn >>> 1)) {
		ix = 0;
		n2 = n2 << 1;
		id = n2 << 1;
		n4 = n2 >>> 2;
		n8 = n2 >>> 3;
		do {
			if(n4 !== 1) {
				for(i0 = ix; i0 < n; i0 += id) {
					i1 = i0;
					i2 = i1 + n4;
					i3 = i2 + n4;
					i4 = i3 + n4;

					//diffsum3_r(x[i3], x[i4], t1); // {a, b, s} <--| {a, b-a, a+b}
					t1 = x[i3] + x[i4];
					x[i4] -= x[i3];
					//sumdiff3(x[i1], t1, x[i3]);   // {a, b, d} <--| {a+b, b, a-b}
					x[i3] = x[i1] - t1;
					x[i1] += t1;

					i1 += n8;
					i2 += n8;
					i3 += n8;
					i4 += n8;

					//sumdiff(x[i3], x[i4], t1, t2); // {s, d}  <--| {a+b, a-b}
					t1 = x[i3] + x[i4];
					t2 = x[i3] - x[i4];

					t1 = -t1 * Math.SQRT1_2;
					t2 *= Math.SQRT1_2;

					// sumdiff(t1, x[i2], x[i4], x[i3]); // {s, d}  <--| {a+b, a-b}
					st1 = x[i2];
					x[i4] = t1 + st1;
					x[i3] = t1 - st1;

					//sumdiff3(x[i1], t2, x[i2]); // {a, b, d} <--| {a+b, b, a-b}
					x[i2] = x[i1] - t2;
					x[i1] += t2;
				}
			} else {
				for(i0 = ix; i0 < n; i0 += id) {
					i1 = i0;
					i2 = i1 + n4;
					i3 = i2 + n4;
					i4 = i3 + n4;

					//diffsum3_r(x[i3], x[i4], t1); // {a, b, s} <--| {a, b-a, a+b}
					t1 = x[i3] + x[i4];
					x[i4] -= x[i3];

					//sumdiff3(x[i1], t1, x[i3]);   // {a, b, d} <--| {a+b, b, a-b}
					x[i3] = x[i1] - t1;
					x[i1] += t1;
				}
			}

			ix = (id << 1) - n2;
			id = id << 2;
		} while (ix < n);

		e = TWO_PI / n2;

		for (var j = 1; j < n8; j++) {
			a = j * e;
			ss1 = Math.sin(a);
			cc1 = Math.cos(a);

			//ss3 = sin(3*a); cc3 = cos(3*a);
			cc3 = 4*cc1*(cc1*cc1-0.75);
			ss3 = 4*ss1*(0.75-ss1*ss1);

			ix = 0; id = n2 << 1;
			do {
				for (i0 = ix; i0 < n; i0 += id) {
					i1 = i0 + j;
					i2 = i1 + n4;
					i3 = i2 + n4;
					i4 = i3 + n4;

					i5 = i0 + n4 - j;
					i6 = i5 + n4;
					i7 = i6 + n4;
					i8 = i7 + n4;

					//cmult(c, s, x, y, &u, &v)
					//cmult(cc1, ss1, x[i7], x[i3], t2, t1); // {u,v} <--| {x*c-y*s, x*s+y*c}
					t2 = x[i7]*cc1 - x[i3]*ss1;
					t1 = x[i7]*ss1 + x[i3]*cc1;

					//cmult(cc3, ss3, x[i8], x[i4], t4, t3);
					t4 = x[i8]*cc3 - x[i4]*ss3;
					t3 = x[i8]*ss3 + x[i4]*cc3;

					//sumdiff(t2, t4);   // {a, b} <--| {a+b, a-b}
					st1 = t2 - t4;
					t2 += t4;
					t4 = st1;

					//sumdiff(t2, x[i6], x[i8], x[i3]); // {s, d}  <--| {a+b, a-b}
					//st1 = x[i6]; x[i8] = t2 + st1; x[i3] = t2 - st1;
					x[i8] = t2 + x[i6];
					x[i3] = t2 - x[i6];

					//sumdiff_r(t1, t3); // {a, b} <--| {a+b, b-a}
					st1 = t3 - t1;
					t1 += t3;
					t3 = st1;

					//sumdiff(t3, x[i2], x[i4], x[i7]); // {s, d}  <--| {a+b, a-b}
					//st1 = x[i2]; x[i4] = t3 + st1; x[i7] = t3 - st1;
					x[i4] = t3 + x[i2];
					x[i7] = t3 - x[i2];

					//sumdiff3(x[i1], t1, x[i6]);   // {a, b, d} <--| {a+b, b, a-b}
					x[i6] = x[i1] - t1;
					x[i1] += t1;

					//diffsum3_r(t4, x[i5], x[i2]); // {a, b, s} <--| {a, b-a, a+b}
					x[i2] = t4 + x[i5];
					x[i5] -= t4;
				}

				ix = (id << 1) - n2;
				id = id << 2;

			} while (ix < n);
		}
	}

	while (--i) {
		rval = x[i];
		ival = x[n-i-1];
		mag = bSi * sqrt(rval * rval + ival * ival);
		spectrum[i] = mag;
	}

	spectrum[0] = Math.abs(bSi * x[0]);

	return spectrum;
}


function reverseBinPermute (N, dest, source) {
	var halfSize    = N >>> 1,
		nm1         = N - 1,
		i = 1, r = 0, h;

	dest[0] = source[0];

	do {
		r += halfSize;
		dest[i] = source[r];
		dest[r] = source[i];

		i++;

		h = halfSize << 1;

		while (h = h >> 1, !((r ^= h) & h));

		if (r >= i) {
			dest[i]     = source[r];
			dest[r]     = source[i];

			dest[nm1-i] = source[nm1-r];
			dest[nm1-r] = source[nm1-i];
		}
		i++;
	} while (i < halfSize);

	dest[nm1] = source[nm1];
};
},{}],5:[function(require,module,exports){
module.exports = identity;

/**
 * Set a mat4 to the identity matrix
 *
 * @param {mat4} out the receiving matrix
 * @returns {mat4} out
 */
function identity(out) {
    out[0] = 1;
    out[1] = 0;
    out[2] = 0;
    out[3] = 0;
    out[4] = 0;
    out[5] = 1;
    out[6] = 0;
    out[7] = 0;
    out[8] = 0;
    out[9] = 0;
    out[10] = 1;
    out[11] = 0;
    out[12] = 0;
    out[13] = 0;
    out[14] = 0;
    out[15] = 1;
    return out;
};
},{}],6:[function(require,module,exports){
var identity = require('./identity');

module.exports = lookAt;

/**
 * Generates a look-at matrix with the given eye position, focal point, and up axis
 *
 * @param {mat4} out mat4 frustum matrix will be written into
 * @param {vec3} eye Position of the viewer
 * @param {vec3} center Point the viewer is looking at
 * @param {vec3} up vec3 pointing up
 * @returns {mat4} out
 */
function lookAt(out, eye, center, up) {
    var x0, x1, x2, y0, y1, y2, z0, z1, z2, len,
        eyex = eye[0],
        eyey = eye[1],
        eyez = eye[2],
        upx = up[0],
        upy = up[1],
        upz = up[2],
        centerx = center[0],
        centery = center[1],
        centerz = center[2];

    if (Math.abs(eyex - centerx) < 0.000001 &&
        Math.abs(eyey - centery) < 0.000001 &&
        Math.abs(eyez - centerz) < 0.000001) {
        return identity(out);
    }

    z0 = eyex - centerx;
    z1 = eyey - centery;
    z2 = eyez - centerz;

    len = 1 / Math.sqrt(z0 * z0 + z1 * z1 + z2 * z2);
    z0 *= len;
    z1 *= len;
    z2 *= len;

    x0 = upy * z2 - upz * z1;
    x1 = upz * z0 - upx * z2;
    x2 = upx * z1 - upy * z0;
    len = Math.sqrt(x0 * x0 + x1 * x1 + x2 * x2);
    if (!len) {
        x0 = 0;
        x1 = 0;
        x2 = 0;
    } else {
        len = 1 / len;
        x0 *= len;
        x1 *= len;
        x2 *= len;
    }

    y0 = z1 * x2 - z2 * x1;
    y1 = z2 * x0 - z0 * x2;
    y2 = z0 * x1 - z1 * x0;

    len = Math.sqrt(y0 * y0 + y1 * y1 + y2 * y2);
    if (!len) {
        y0 = 0;
        y1 = 0;
        y2 = 0;
    } else {
        len = 1 / len;
        y0 *= len;
        y1 *= len;
        y2 *= len;
    }

    out[0] = x0;
    out[1] = y0;
    out[2] = z0;
    out[3] = 0;
    out[4] = x1;
    out[5] = y1;
    out[6] = z1;
    out[7] = 0;
    out[8] = x2;
    out[9] = y2;
    out[10] = z2;
    out[11] = 0;
    out[12] = -(x0 * eyex + x1 * eyey + x2 * eyez);
    out[13] = -(y0 * eyex + y1 * eyey + y2 * eyez);
    out[14] = -(z0 * eyex + z1 * eyey + z2 * eyez);
    out[15] = 1;

    return out;
};
},{"./identity":5}],7:[function(require,module,exports){
module.exports = perspective;

/**
 * Generates a perspective projection matrix with the given bounds
 *
 * @param {mat4} out mat4 frustum matrix will be written into
 * @param {number} fovy Vertical field of view in radians
 * @param {number} aspect Aspect ratio. typically viewport width/height
 * @param {number} near Near bound of the frustum
 * @param {number} far Far bound of the frustum
 * @returns {mat4} out
 */
function perspective(out, fovy, aspect, near, far) {
    var f = 1.0 / Math.tan(fovy / 2),
        nf = 1 / (near - far);
    out[0] = f / aspect;
    out[1] = 0;
    out[2] = 0;
    out[3] = 0;
    out[4] = 0;
    out[5] = f;
    out[6] = 0;
    out[7] = 0;
    out[8] = 0;
    out[9] = 0;
    out[10] = (far + near) * nf;
    out[11] = -1;
    out[12] = 0;
    out[13] = 0;
    out[14] = (2 * far * near) * nf;
    out[15] = 0;
    return out;
};
},{}],8:[function(require,module,exports){
module.exports = function(strings) {
  if (typeof strings === 'string') strings = [strings]
  var exprs = [].slice.call(arguments,1)
  var parts = []
  for (var i = 0; i < strings.length-1; i++) {
    parts.push(strings[i], exprs[i] || '')
  }
  parts.push(strings[i])
  return parts.join('')
}

},{}],9:[function(require,module,exports){
'use strict'

module.exports = mouseListen

var mouse = require('mouse-event')

function mouseListen (element, callback) {
  if (!callback) {
    callback = element
    element = window
  }

  var buttonState = 0
  var x = 0
  var y = 0
  var mods = {
    shift: false,
    alt: false,
    control: false,
    meta: false
  }
  var attached = false

  function updateMods (ev) {
    var changed = false
    if ('altKey' in ev) {
      changed = changed || ev.altKey !== mods.alt
      mods.alt = !!ev.altKey
    }
    if ('shiftKey' in ev) {
      changed = changed || ev.shiftKey !== mods.shift
      mods.shift = !!ev.shiftKey
    }
    if ('ctrlKey' in ev) {
      changed = changed || ev.ctrlKey !== mods.control
      mods.control = !!ev.ctrlKey
    }
    if ('metaKey' in ev) {
      changed = changed || ev.metaKey !== mods.meta
      mods.meta = !!ev.metaKey
    }
    return changed
  }

  function handleEvent (nextButtons, ev) {
    var nextX = mouse.x(ev)
    var nextY = mouse.y(ev)
    if ('buttons' in ev) {
      nextButtons = ev.buttons | 0
    }
    if (nextButtons !== buttonState ||
      nextX !== x ||
      nextY !== y ||
      updateMods(ev)) {
      buttonState = nextButtons | 0
      x = nextX || 0
      y = nextY || 0
      callback && callback(buttonState, x, y, mods)
    }
  }

  function clearState (ev) {
    handleEvent(0, ev)
  }

  function handleBlur () {
    if (buttonState ||
      x ||
      y ||
      mods.shift ||
      mods.alt ||
      mods.meta ||
      mods.control) {
      x = y = 0
      buttonState = 0
      mods.shift = mods.alt = mods.control = mods.meta = false
      callback && callback(0, 0, 0, mods)
    }
  }

  function handleMods (ev) {
    if (updateMods(ev)) {
      callback && callback(buttonState, x, y, mods)
    }
  }

  function handleMouseMove (ev) {
    if (mouse.buttons(ev) === 0) {
      handleEvent(0, ev)
    } else {
      handleEvent(buttonState, ev)
    }
  }

  function handleMouseDown (ev) {
    handleEvent(buttonState | mouse.buttons(ev), ev)
  }

  function handleMouseUp (ev) {
    handleEvent(buttonState & ~mouse.buttons(ev), ev)
  }

  function attachListeners () {
    if (attached) {
      return
    }
    attached = true

    element.addEventListener('mousemove', handleMouseMove)

    element.addEventListener('mousedown', handleMouseDown)

    element.addEventListener('mouseup', handleMouseUp)

    element.addEventListener('mouseleave', clearState)
    element.addEventListener('mouseenter', clearState)
    element.addEventListener('mouseout', clearState)
    element.addEventListener('mouseover', clearState)

    element.addEventListener('blur', handleBlur)

    element.addEventListener('keyup', handleMods)
    element.addEventListener('keydown', handleMods)
    element.addEventListener('keypress', handleMods)

    if (element !== window) {
      window.addEventListener('blur', handleBlur)

      window.addEventListener('keyup', handleMods)
      window.addEventListener('keydown', handleMods)
      window.addEventListener('keypress', handleMods)
    }
  }

  function detachListeners () {
    if (!attached) {
      return
    }
    attached = false

    element.removeEventListener('mousemove', handleMouseMove)

    element.removeEventListener('mousedown', handleMouseDown)

    element.removeEventListener('mouseup', handleMouseUp)

    element.removeEventListener('mouseleave', clearState)
    element.removeEventListener('mouseenter', clearState)
    element.removeEventListener('mouseout', clearState)
    element.removeEventListener('mouseover', clearState)

    element.removeEventListener('blur', handleBlur)

    element.removeEventListener('keyup', handleMods)
    element.removeEventListener('keydown', handleMods)
    element.removeEventListener('keypress', handleMods)

    if (element !== window) {
      window.removeEventListener('blur', handleBlur)

      window.removeEventListener('keyup', handleMods)
      window.removeEventListener('keydown', handleMods)
      window.removeEventListener('keypress', handleMods)
    }
  }

  // Attach listeners
  attachListeners()

  var result = {
    element: element
  }

  Object.defineProperties(result, {
    enabled: {
      get: function () { return attached },
      set: function (f) {
        if (f) {
          attachListeners()
        } else {
          detachListeners()
        }
      },
      enumerable: true
    },
    buttons: {
      get: function () { return buttonState },
      enumerable: true
    },
    x: {
      get: function () { return x },
      enumerable: true
    },
    y: {
      get: function () { return y },
      enumerable: true
    },
    mods: {
      get: function () { return mods },
      enumerable: true
    }
  })

  return result
}

},{"mouse-event":10}],10:[function(require,module,exports){
'use strict'

function mouseButtons(ev) {
  if(typeof ev === 'object') {
    if('buttons' in ev) {
      return ev.buttons
    } else if('which' in ev) {
      var b = ev.which
      if(b === 2) {
        return 4
      } else if(b === 3) {
        return 2
      } else if(b > 0) {
        return 1<<(b-1)
      }
    } else if('button' in ev) {
      var b = ev.button
      if(b === 1) {
        return 4
      } else if(b === 2) {
        return 2
      } else if(b >= 0) {
        return 1<<b
      }
    }
  }
  return 0
}
exports.buttons = mouseButtons

function mouseElement(ev) {
  return ev.target || ev.srcElement || window
}
exports.element = mouseElement

function mouseRelativeX(ev) {
  if(typeof ev === 'object') {
    if('offsetX' in ev) {
      return ev.offsetX
    }
    var target = mouseElement(ev)
    var bounds = target.getBoundingClientRect()
    return ev.clientX - bounds.left
  }
  return 0
}
exports.x = mouseRelativeX

function mouseRelativeY(ev) {
  if(typeof ev === 'object') {
    if('offsetY' in ev) {
      return ev.offsetY
    }
    var target = mouseElement(ev)
    var bounds = target.getBoundingClientRect()
    return ev.clientY - bounds.top
  }
  return 0
}
exports.y = mouseRelativeY

},{}],11:[function(require,module,exports){
'use strict'

var toPX = require('to-px')

module.exports = mouseWheelListen

function mouseWheelListen(element, callback, noScroll) {
  if(typeof element === 'function') {
    noScroll = !!callback
    callback = element
    element = window
  }
  var lineHeight = toPX('ex', element)
  var listener = function(ev) {
    if(noScroll) {
      ev.preventDefault()
    }
    var dx = ev.deltaX || 0
    var dy = ev.deltaY || 0
    var dz = ev.deltaZ || 0
    var mode = ev.deltaMode
    var scale = 1
    switch(mode) {
      case 1:
        scale = lineHeight
      break
      case 2:
        scale = window.innerHeight
      break
    }
    dx *= scale
    dy *= scale
    dz *= scale
    if(dx || dy || dz) {
      return callback(dx, dy, dz, ev)
    }
  }
  element.addEventListener('wheel', listener)
  return listener
}

},{"to-px":19}],12:[function(require,module,exports){
module.exports = function parseUnit(str, out) {
    if (!out)
        out = [ 0, '' ]

    str = String(str)
    var num = parseFloat(str, 10)
    out[0] = num
    out[1] = str.match(/[\d.\-\+]*\s*(.*)/)[1] || ''
    return out
}
},{}],13:[function(require,module,exports){
const fourierTransform = require('fourier-transform')

function compareInt (a, b) {
  return a - b
}

function applyPrefix (prefix, object) {
  const result = {}
  Object.keys(object).forEach((name) => {
    result[prefix + name] = object[name]
  })
  return result
}

module.exports = function (options) {
  // namespace
  const name = options.name || ''

  //
  // basic inputs
  //
  // regl instance
  const regl = options.regl
  // web audio analyser node
  const analyser = options.analyser
  // sample rate in Hz
  const sampleRate = options.sampleRate || 44100

  //
  // beat detection
  //
  // number of beat detection bins (set to 0 to disable)
  const binCount = 'beats' in options ? options.beats : 16
  // length of moving window in seconds
  const beatTime = options.beatTime || 1.0
  // strictness of beat detection (should be between 0.5 and 1)
  const beatThreshold = options.beatThreshold || 0.8

  // Tempo
  const initTempo = options.tempo || (100 / 60)
  const maxTempo = options.maxTempoBeat || Math.ceil(binCount / 4)
  const tempoBufferSize = options.tempoBufferSize || 4096

  //
  // pitch detection
  //
  // number of pitches to detect (set to 0 to disable)
  const pitchCount = 'pitches' in options ? options.pitches : 4
  // minimum detectable pitch in Hz
  const maxPitch = (options.maxPitch || 10000)
  // length of moving pitch window in seconds
  const pitchTime = (options.pitchTime || 0.25)

  // -------------------------------------------
  // implementation stuff
  // -------------------------------------------
  const prefix = name ? name + '_' : ''

  const N = analyser.frequencyBinCount

  const freq = new Uint8Array(N)
  const time = new Uint8Array(N)
  const cepstrum = new Float64Array(N / 2)

  const freqTexture = regl.texture({
    shape: [N, 1, 1],
    type: 'uint8'
  })

  const timeTexture = regl.texture({
    shape: [N, 1, 1],
    type: 'uint8'
  })

  const uniforms = {
    sampleCount: N,
    freq: freqTexture,
    time: timeTexture,
    tempo: regl.context('tempo'),
    volume: regl.prop('volume')
  }

  for (let i = 0; i < binCount; ++i) {
    uniforms['beats[' + i + ']'] = regl.prop('beatEnvelope[' + i + ']')
  }

  for (let i = 0; i < pitchCount; ++i) {
    uniforms['pitches[' + i + ']'] = regl.prop('pitches[' + i + ']')
  }

  let tempoCounter = 0

  const setupAnalysis = regl({
    context: applyPrefix(prefix, {
      sampleCount: N,
      freq,
      time,
      cepstrum: cepstrum,
      freqTexture,
      timeTexture,
      volume: regl.prop('volume'),
      beats: regl.prop('beats'),
      pitches: regl.prop('pitches'),
      tempo: () => (tempoCounter / tempoBufferSize) * sampleRate / N
    }),
    uniforms: applyPrefix(prefix, uniforms)
  })

  const binSize = Math.floor(N / binCount) | 0
  const beatBufferSize = Math.ceil(beatTime * sampleRate / N) | 0

  const volumeHistory = new Float64Array(binCount * beatBufferSize)
  const beatLevels = Array(binCount).fill(0)
  const beatEnvelope = Array(binCount).fill(0)
  const beats = Array(binCount).fill(0)

  const medianArray = Array(beatBufferSize).fill(0)
  const cutoffIndex = Math.floor((beatBufferSize - 1) * beatThreshold) | 0

  const tempoWindow = Array(tempoBufferSize).fill(0)
  let tempoPtr = 0

  for (let i = 0; i < tempoBufferSize; ++i) {
    if (Math.random() < initTempo * N / sampleRate) {
      tempoWindow[i] = 1
      tempoCounter += 1
    }
  }

  let beatPtr = 0
  let volume = 0
  function estimateBeats () {
    let vol = 0.0
    let beatFound = false
    for (var i = 0; i < binCount; ++i) {
      for (let j = i * beatBufferSize, k = 0; k < beatBufferSize; ++j, ++k) {
        medianArray[k] = volumeHistory[j]
      }
      medianArray.sort(compareInt)

      let sum = 0.0
      for (let j = binSize * i; j < binSize * (i + 1); ++j) {
        const x = Math.pow(freq[j] / 255.0, 2.0)
        sum += x
        vol += x
      }
      sum = Math.sqrt(sum) / binSize
      const prevBeat = beatLevels[i]
      const curBeat = beatLevels[i] = sum > medianArray[cutoffIndex] ? sum : 0

      if (curBeat && !prevBeat) {
        beats[i] = 1
        beatEnvelope[i] = 1.0
        if (i < maxTempo) {
          beatFound = true
        }
      } else {
        beats[i] = 0
        if (curBeat) {
          beatEnvelope[i] = 0.4 * beatEnvelope[i] + 0.05
        } else {
          beatEnvelope[i] *= 0.25
        }
      }
      volumeHistory[beatPtr + beatBufferSize * i] = sum
    }
    volume = Math.sqrt(volume) / N
    beatPtr = (beatPtr + 1) % beatBufferSize

    tempoCounter -= tempoWindow[tempoPtr]
    tempoCounter += (tempoWindow[tempoPtr] = (beatFound) | 0)
    tempoPtr = (tempoPtr + 1) % tempoBufferSize
  }

  const startQ = Math.min(Math.ceil(sampleRate / maxPitch), N / 2) | 0
  const endQ = (N / 2) | 0

  const pitches = Array(pitchCount).fill(0)

  const pitchWindow = Math.ceil(pitchTime * sampleRate / N) | 0
  const pitchHistogram = new Float64Array(N)
  const pitchHistory = new Float64Array(pitchWindow * pitchCount)
  let pitchWindowPtr = 0

  const pitchIndex = Array(N).fill(0).map(function (_, i) {
    return i
  })
  function comparePitch (a, b) {
    return pitchHistogram[b] - pitchHistogram[a]
  }

  const pitchQ = Array(pitchCount).fill(0)
  const pitchW = Array(pitchCount).fill(0)

  const logFreq = new Float64Array(N)

  function estimatePitch () {
    for (let i = 0; i < N; ++i) {
      logFreq[i] = Math.log(1 + freq[i])
    }
    fourierTransform(logFreq, cepstrum)

    for (let i = 0; i < pitchCount; ++i) {
      pitchQ[i] = 0
      pitchW[i] = -Infinity
    }

    for (let i = startQ; i < endQ; ++i) {
      const a = cepstrum[i - 1]
      const b = cepstrum[i]
      const c = cepstrum[i + 1]

      if (b > a && b > c) {
        for (let j = 0; j < pitchCount; ++j) {
          if (pitchW[j] < b) {
            for (let k = pitchCount - 1; k > j; --k) {
              pitchQ[k] = pitchQ[k - 1]
              pitchW[k] = pitchW[k - 1]
            }
            pitchQ[j] = i
            pitchW[j] = b
            break
          }
        }
      }
    }

    // Update histogram
    for (let j = 0; j < pitchCount; ++j) {
      const w = 1.0 / (1.0 + j)
      if (pitchQ[j]) {
        pitchHistogram[pitchQ[j]] += w
      }
      var prev = pitchHistory[pitchWindowPtr]
      if (prev) {
        pitchHistogram[prev] -= w
      }
      pitchHistory[pitchWindowPtr] = pitchQ[j]
      pitchWindowPtr = (pitchWindowPtr + 1) % pitchHistory.length
    }

    // Take top k pitch values for current pitch
    // FIXME: should use heap or insertion sort here
    pitchIndex.sort(comparePitch)

    for (let j = 0; j < pitchCount; ++j) {
      if (pitchIndex[j]) {
        pitches[j] = sampleRate / pitchIndex[j]
      } else {
        for (; j < pitchCount; ++j) {
          pitches[j] = 0
        }
      }
    }
  }

  return function (block) {
    // poll analyser
    analyser.getByteFrequencyData(freq)
    analyser.getByteTimeDomainData(time)

    // upload texture data
    freqTexture.subimage(freq)
    timeTexture.subimage(time)

    // update beat detection
    if (binCount) {
      estimateBeats()
    }

    // update pitch detection
    if (pitchCount) {
      estimatePitch()
    }

    setupAnalysis({
      volume,
      beats,
      pitches,
      beatEnvelope
    },
    block)
  }
}

},{"fourier-transform":4}],14:[function(require,module,exports){
const getUserMedia = require('getusermedia')
const reglAnalyser = require('./analyser')

module.exports = function (options) {
  getUserMedia({audio: true}, function (err, stream) {
    if (err) {
      options.error && options.error(err)
      return
    }
    const context = options.audioContext || (new window.AudioContext())
    const analyser = context.createAnalyser()
    context.createMediaStreamSource(stream).connect(analyser)
    options.done(reglAnalyser(Object.assign({
      analyser,
      sampleRate: context.sampleRate,
      name: 'mic'
    }, options)))
  })
}

},{"./analyser":13,"getusermedia":15}],15:[function(require,module,exports){
// getUserMedia helper by @HenrikJoreteg used for navigator.getUserMedia shim
var adapter = require('webrtc-adapter');

module.exports = function (constraints, cb) {
    var error;
    var haveOpts = arguments.length === 2;
    var defaultOpts = {video: true, audio: true};

    var denied = 'PermissionDeniedError';
    var altDenied = 'PERMISSION_DENIED';
    var notSatisfied = 'ConstraintNotSatisfiedError';

    // make constraints optional
    if (!haveOpts) {
        cb = constraints;
        constraints = defaultOpts;
    }

    // treat lack of browser support like an error
    if (typeof navigator === 'undefined' || !navigator.getUserMedia) {
        // throw proper error per spec
        error = new Error('MediaStreamError');
        error.name = 'NotSupportedError';

        // keep all callbacks async
        return setTimeout(function () {
            cb(error);
        }, 0);
    }

    // normalize error handling when no media types are requested
    if (!constraints.audio && !constraints.video) {
        error = new Error('MediaStreamError');
        error.name = 'NoMediaRequestedError';

        // keep all callbacks async
        return setTimeout(function () {
            cb(error);
        }, 0);
    }

    navigator.mediaDevices.getUserMedia(constraints)
    .then(function (stream) {
        cb(null, stream);
    }).catch(function (err) {
        var error;
        // coerce into an error object since FF gives us a string
        // there are only two valid names according to the spec
        // we coerce all non-denied to "constraint not satisfied".
        if (typeof err === 'string') {
            error = new Error('MediaStreamError');
            if (err === denied || err === altDenied) {
                error.name = denied;
            } else {
                error.name = notSatisfied;
            }
        } else {
            // if we get an error object make sure '.name' property is set
            // according to spec: http://dev.w3.org/2011/webrtc/editor/getusermedia.html#navigatorusermediaerror-and-navigatorusermediaerrorcallback
            error = err;
            if (!error.name) {
                // this is likely chrome which
                // sets a property called "ERROR_DENIED" on the error object
                // if so we make sure to set a name
                if (error[denied]) {
                    err.name = denied;
                } else {
                    err.name = notSatisfied;
                }
            }
        }

        cb(error);
    });
};

},{"webrtc-adapter":20}],16:[function(require,module,exports){
var mouseChange = require('mouse-change')
var mouseWheel = require('mouse-wheel')
var identity = require('gl-mat4/identity')
var perspective = require('gl-mat4/perspective')
var lookAt = require('gl-mat4/lookAt')

module.exports = createCamera

var isBrowser = typeof window !== 'undefined'

function createCamera (regl, props_) {
  var props = props_ || {}

  // Preserve backward-compatibilty while renaming preventDefault -> noScroll
  if (typeof props.noScroll === 'undefined') {
    props.noScroll = props.preventDefault;
  }

  var cameraState = {
    view: identity(new Float32Array(16)),
    projection: identity(new Float32Array(16)),
    center: new Float32Array(props.center || 3),
    theta: props.theta || 0,
    phi: props.phi || 0,
    distance: Math.log(props.distance || 10.0),
    eye: new Float32Array(3),
    up: new Float32Array(props.up || [0, 1, 0]),
    fovy: props.fovy || Math.PI / 4.0,
    near: typeof props.near !== 'undefined' ? props.near : 0.01,
    far: typeof props.far !== 'undefined' ? props.far : 1000.0,
    noScroll: typeof props.noScroll !== 'undefined' ? props.noScroll : false,
    flipY: !!props.flipY,
    dtheta: 0,
    dphi: 0,
    rotationSpeed: typeof props.rotationSpeed !== 'undefined' ? props.rotationSpeed : 1,
    zoomSpeed: typeof props.zoomSpeed !== 'undefined' ? props.zoomSpeed : 1,
    renderOnDirty: typeof props.renderOnDirty !== undefined ? !!props.renderOnDirty : false
  }

  var element = props.element
  var damping = typeof props.damping !== 'undefined' ? props.damping : 0.9

  var right = new Float32Array([1, 0, 0])
  var front = new Float32Array([0, 0, 1])

  var minDistance = Math.log('minDistance' in props ? props.minDistance : 0.1)
  var maxDistance = Math.log('maxDistance' in props ? props.maxDistance : 1000)

  var ddistance = 0

  var prevX = 0
  var prevY = 0

  if (isBrowser && props.mouse !== false) {
    var source = element || regl._gl.canvas

    function getWidth () {
      return element ? element.offsetWidth : window.innerWidth
    }

    function getHeight () {
      return element ? element.offsetHeight : window.innerHeight
    }

    mouseChange(source, function (buttons, x, y) {
      if (buttons & 1) {
        var dx = (x - prevX) / getWidth()
        var dy = (y - prevY) / getHeight()

        cameraState.dtheta += cameraState.rotationSpeed * 4.0 * dx
        cameraState.dphi += cameraState.rotationSpeed * 4.0 * dy
        cameraState.dirty = true;
      }
      prevX = x
      prevY = y
    })

    mouseWheel(source, function (dx, dy) {
      ddistance += dy / getHeight() * cameraState.zoomSpeed
      cameraState.dirty = true;
    }, props.noScroll)
  }

  function damp (x) {
    var xd = x * damping
    if (Math.abs(xd) < 0.1) {
      return 0
    }
    cameraState.dirty = true;
    return xd
  }

  function clamp (x, lo, hi) {
    return Math.min(Math.max(x, lo), hi)
  }

  function updateCamera (props) {
    Object.keys(props).forEach(function (prop) {
      cameraState[prop] = props[prop]
    })

    var center = cameraState.center
    var eye = cameraState.eye
    var up = cameraState.up
    var dtheta = cameraState.dtheta
    var dphi = cameraState.dphi

    cameraState.theta += dtheta
    cameraState.phi = clamp(
      cameraState.phi + dphi,
      -Math.PI / 2.0,
      Math.PI / 2.0)
    cameraState.distance = clamp(
      cameraState.distance + ddistance,
      minDistance,
      maxDistance)

    cameraState.dtheta = damp(dtheta)
    cameraState.dphi = damp(dphi)
    ddistance = damp(ddistance)

    var theta = cameraState.theta
    var phi = cameraState.phi
    var r = Math.exp(cameraState.distance)

    var vf = r * Math.sin(theta) * Math.cos(phi)
    var vr = r * Math.cos(theta) * Math.cos(phi)
    var vu = r * Math.sin(phi)

    for (var i = 0; i < 3; ++i) {
      eye[i] = center[i] + vf * front[i] + vr * right[i] + vu * up[i]
    }

    lookAt(cameraState.view, eye, center, up)
  }

  cameraState.dirty = true;

  var injectContext = regl({
    context: Object.assign({}, cameraState, {
      dirty: function () {
        return cameraState.dirty;
      },
      projection: function (context) {
        perspective(cameraState.projection,
          cameraState.fovy,
          context.viewportWidth / context.viewportHeight,
          cameraState.near,
          cameraState.far)
        if (cameraState.flipY) { cameraState.projection[5] *= -1 }
        return cameraState.projection
      }
    }),
    uniforms: Object.keys(cameraState).reduce(function (uniforms, name) {
      uniforms[name] = regl.context(name)
      return uniforms
    }, {})
  })

  function setupCamera (props, block) {
    if (typeof setupCamera.dirty !== 'undefined') {
      cameraState.dirty = setupCamera.dirty || cameraState.dirty
      setupCamera.dirty = undefined;
    }

    if (props && block) {
      cameraState.dirty = true;
    }

    if (cameraState.renderOnDirty && !cameraState.dirty) return;

    if (!block) {
      block = props
      props = {}
    }

    updateCamera(props)
    injectContext(block)
    cameraState.dirty = false;
  }

  Object.keys(cameraState).forEach(function (name) {
    setupCamera[name] = cameraState[name]
  })

  return setupCamera
}

},{"gl-mat4/identity":5,"gl-mat4/lookAt":6,"gl-mat4/perspective":7,"mouse-change":9,"mouse-wheel":11}],17:[function(require,module,exports){
(function(da,ea){"object"===typeof exports&&"undefined"!==typeof module?module.exports=ea():"function"===typeof define&&define.amd?define(ea):da.createREGL=ea()})(this,function(){function da(a,b){this.id=vb++;this.type=a;this.data=b}function ea(a){if(0===a.length)return[];var b=a.charAt(0),c=a.charAt(a.length-1);if(1<a.length&&b===c&&('"'===b||"'"===b))return['"'+a.substr(1,a.length-2).replace(/\\/g,"\\\\").replace(/"/g,'\\"')+'"'];if(b=/\[(false|true|null|\d+|'[^']*'|"[^"]*")\]/.exec(a))return ea(a.substr(0,
b.index)).concat(ea(b[1])).concat(ea(a.substr(b.index+b[0].length)));b=a.split(".");if(1===b.length)return['"'+a.replace(/\\/g,"\\\\").replace(/"/g,'\\"')+'"'];a=[];for(c=0;c<b.length;++c)a=a.concat(ea(b[c]));return a}function Wa(a){return"["+ea(a).join("][")+"]"}function wb(){var a={"":0},b=[""];return{id:function(c){var d=a[c];if(d)return d;d=a[c]=b.length;b.push(c);return d},str:function(a){return b[a]}}}function xb(a,b,c){function d(){var b=window.innerWidth,d=window.innerHeight;a!==document.body&&
(d=a.getBoundingClientRect(),b=d.right-d.left,d=d.bottom-d.top);f.width=c*b;f.height=c*d;A(f.style,{width:b+"px",height:d+"px"})}var f=document.createElement("canvas");A(f.style,{border:0,margin:0,padding:0,top:0,left:0});a.appendChild(f);a===document.body&&(f.style.position="absolute",A(a.style,{margin:0,padding:0}));window.addEventListener("resize",d,!1);d();return{canvas:f,onDestroy:function(){window.removeEventListener("resize",d);a.removeChild(f)}}}function yb(a,b){function c(c){try{return a.getContext(c,
b)}catch(f){return null}}return c("webgl")||c("experimental-webgl")||c("webgl-experimental")}function Xa(a){return"string"===typeof a?a.split():a}function Ya(a){return"string"===typeof a?document.querySelector(a):a}function zb(a){var b=a||{},c,d,f,k;a={};var p=[],h=[],g="undefined"===typeof window?1:window.devicePixelRatio,r=!1,v=function(a){},l=function(){};"string"===typeof b?c=document.querySelector(b):"object"===typeof b&&("string"===typeof b.nodeName&&"function"===typeof b.appendChild&&"function"===
typeof b.getBoundingClientRect?c=b:"function"===typeof b.drawArrays||"function"===typeof b.drawElements?(k=b,f=k.canvas):("gl"in b?k=b.gl:"canvas"in b?f=Ya(b.canvas):"container"in b&&(d=Ya(b.container)),"attributes"in b&&(a=b.attributes),"extensions"in b&&(p=Xa(b.extensions)),"optionalExtensions"in b&&(h=Xa(b.optionalExtensions)),"onDone"in b&&(v=b.onDone),"profile"in b&&(r=!!b.profile),"pixelRatio"in b&&(g=+b.pixelRatio)));c&&("canvas"===c.nodeName.toLowerCase()?f=c:d=c);if(!k){if(!f){c=xb(d||document.body,
v,g);if(!c)return null;f=c.canvas;l=c.onDestroy}k=yb(f,a)}return k?{gl:k,canvas:f,container:d,extensions:p,optionalExtensions:h,pixelRatio:g,profile:r,onDone:v,onDestroy:l}:(l(),v("webgl not supported, try upgrading your browser or graphics drivers http://get.webgl.org"),null)}function Ab(a,b){function c(b){b=b.toLowerCase();var c;try{c=d[b]=a.getExtension(b)}catch(g){}return!!c}for(var d={},f=0;f<b.extensions.length;++f){var k=b.extensions[f];if(!c(k))return b.onDestroy(),b.onDone('"'+k+'" extension is not supported by the current WebGL context, try upgrading your system or a different browser'),
null}b.optionalExtensions.forEach(c);return{extensions:d,restore:function(){Object.keys(d).forEach(function(a){if(!c(a))throw Error("(regl): error restoring extension "+a);})}}}function ja(a){return!!a&&"object"===typeof a&&Array.isArray(a.shape)&&Array.isArray(a.stride)&&"number"===typeof a.offset&&a.shape.length===a.stride.length&&(Array.isArray(a.data)||O(a.data))}function J(a,b){for(var c=Array(a),d=0;d<a;++d)c[d]=b(d);return c}function Za(a){var b,c;b=(65535<a)<<4;a>>>=b;c=(255<a)<<3;a>>>=c;
b|=c;c=(15<a)<<2;a>>>=c;b|=c;c=(3<a)<<1;return b|c|a>>>c>>1}function fa(a){a:{for(var b=16;268435456>=b;b*=16)if(a<=b){a=b;break a}a=0}b=$a[Za(a)>>2];return 0<b.length?b.pop():new ArrayBuffer(a)}function ab(a){$a[Za(a.byteLength)>>2].push(a)}function bb(a,b,c,d,f,k){for(var p=0;p<b;++p)for(var h=a[p],g=0;g<c;++g)for(var r=h[g],v=0;v<d;++v)f[k++]=r[v]}function cb(a,b,c,d,f){for(var k=1,p=c+1;p<b.length;++p)k*=b[p];var h=b[c];if(4===b.length-c){var g=b[c+1],r=b[c+2];b=b[c+3];for(p=0;p<h;++p)bb(a[p],
g,r,b,d,f),f+=k}else for(p=0;p<h;++p)cb(a[p],b,c+1,d,f),f+=k}function Ga(a){return Ha[Object.prototype.toString.call(a)]|0}function db(a,b){for(var c=0;c<b.length;++c)a[c]=b[c]}function eb(a,b,c,d,f,k,p){for(var h=0,g=0;g<c;++g)for(var r=0;r<d;++r)a[h++]=b[f*g+k*r+p]}function Bb(a,b,c){function d(b){this.id=h++;this.buffer=a.createBuffer();this.type=b;this.usage=35044;this.byteLength=0;this.dimension=1;this.dtype=5121;this.persistentData=null;c.profile&&(this.stats={size:0})}function f(b,c,d){b.byteLength=
c.byteLength;a.bufferData(b.type,c,d)}function k(a,b,c,d,e,n){a.usage=c;if(Array.isArray(b)){if(a.dtype=d||5126,0<b.length)if(Array.isArray(b[0])){e=fb(b);for(var D=d=1;D<e.length;++D)d*=e[D];a.dimension=d;b=Pa(b,e,a.dtype);f(a,b,c);n?a.persistentData=b:x.freeType(b)}else"number"===typeof b[0]?(a.dimension=e,e=x.allocType(a.dtype,b.length),db(e,b),f(a,e,c),n?a.persistentData=e:x.freeType(e)):O(b[0])&&(a.dimension=b[0].length,a.dtype=d||Ga(b[0])||5126,b=Pa(b,[b.length,b[0].length],a.dtype),f(a,b,c),
n?a.persistentData=b:x.freeType(b))}else if(O(b))a.dtype=d||Ga(b),a.dimension=e,f(a,b,c),n&&(a.persistentData=new Uint8Array(new Uint8Array(b.buffer)));else if(ja(b)){e=b.shape;var g=b.stride,D=b.offset,t=0,k=0,p=0,r=0;1===e.length?(t=e[0],k=1,p=g[0],r=0):2===e.length&&(t=e[0],k=e[1],p=g[0],r=g[1]);a.dtype=d||Ga(b.data)||5126;a.dimension=k;e=x.allocType(a.dtype,t*k);eb(e,b.data,t,k,p,r,D);f(a,e,c);n?a.persistentData=e:x.freeType(e)}}function p(c){b.bufferCount--;a.deleteBuffer(c.buffer);c.buffer=
null;delete g[c.id]}var h=0,g={};d.prototype.bind=function(){a.bindBuffer(this.type,this.buffer)};d.prototype.destroy=function(){p(this)};var r=[];c.profile&&(b.getTotalBufferSize=function(){var a=0;Object.keys(g).forEach(function(b){a+=g[b].stats.size});return a});return{create:function(f,l,u,m){function e(b){var d=35044,g=null,l=0,f=0,u=1;Array.isArray(b)||O(b)||ja(b)?g=b:"number"===typeof b?l=b|0:b&&("data"in b&&(g=b.data),"usage"in b&&(d=hb[b.usage]),"type"in b&&(f=Qa[b.type]),"dimension"in b&&
(u=b.dimension|0),"length"in b&&(l=b.length|0));n.bind();g?k(n,g,d,f,u,m):(a.bufferData(n.type,l,d),n.dtype=f||5121,n.usage=d,n.dimension=u,n.byteLength=l);c.profile&&(n.stats.size=n.byteLength*ga[n.dtype]);return e}b.bufferCount++;var n=new d(l);g[n.id]=n;u||e(f);e._reglType="buffer";e._buffer=n;e.subdata=function(b,c){var d=(c||0)|0,g;n.bind();if(O(b))a.bufferSubData(n.type,d,b);else if(Array.isArray(b)){if(0<b.length)if("number"===typeof b[0]){var l=x.allocType(n.dtype,b.length);db(l,b);a.bufferSubData(n.type,
d,l);x.freeType(l)}else if(Array.isArray(b[0])||O(b[0]))g=fb(b),l=Pa(b,g,n.dtype),a.bufferSubData(n.type,d,l),x.freeType(l)}else if(ja(b)){g=b.shape;var f=b.stride,m=l=0,u=0,k=0;1===g.length?(l=g[0],m=1,u=f[0],k=0):2===g.length&&(l=g[0],m=g[1],u=f[0],k=f[1]);g=Array.isArray(b.data)?n.dtype:Ga(b.data);g=x.allocType(g,l*m);eb(g,b.data,l,m,u,k,b.offset);a.bufferSubData(n.type,d,g);x.freeType(g)}return e};c.profile&&(e.stats=n.stats);e.destroy=function(){p(n)};return e},createStream:function(a,b){var c=
r.pop();c||(c=new d(a));c.bind();k(c,b,35040,0,1,!1);return c},destroyStream:function(a){r.push(a)},clear:function(){H(g).forEach(p);r.forEach(p)},getBuffer:function(a){return a&&a._buffer instanceof d?a._buffer:null},restore:function(){H(g).forEach(function(b){b.buffer=a.createBuffer();a.bindBuffer(b.type,b.buffer);a.bufferData(b.type,b.persistentData||b.byteLength,b.usage)})},_initBuffer:k}}function Cb(a,b,c,d){function f(a){this.id=g++;h[this.id]=this;this.buffer=a;this.primType=4;this.type=this.vertCount=
0}function k(d,g,f,e,n,k,w){d.buffer.bind();if(g){var t=w;w||O(g)&&(!ja(g)||O(g.data))||(t=b.oes_element_index_uint?5125:5123);c._initBuffer(d.buffer,g,f,t,3)}else a.bufferData(34963,k,f),d.buffer.dtype=t||5121,d.buffer.usage=f,d.buffer.dimension=3,d.buffer.byteLength=k;t=w;if(!w){switch(d.buffer.dtype){case 5121:case 5120:t=5121;break;case 5123:case 5122:t=5123;break;case 5125:case 5124:t=5125}d.buffer.dtype=t}d.type=t;g=n;0>g&&(g=d.buffer.byteLength,5123===t?g>>=1:5125===t&&(g>>=2));d.vertCount=
g;g=e;0>e&&(g=4,e=d.buffer.dimension,1===e&&(g=0),2===e&&(g=1),3===e&&(g=4));d.primType=g}function p(a){d.elementsCount--;delete h[a.id];a.buffer.destroy();a.buffer=null}var h={},g=0,r={uint8:5121,uint16:5123};b.oes_element_index_uint&&(r.uint32=5125);f.prototype.bind=function(){this.buffer.bind()};var v=[];return{create:function(a,b){function g(a){if(a)if("number"===typeof a)e(a),n.primType=4,n.vertCount=a|0,n.type=5121;else{var b=null,c=35044,d=-1,f=-1,l=0,p=0;if(Array.isArray(a)||O(a)||ja(a))b=
a;else if("data"in a&&(b=a.data),"usage"in a&&(c=hb[a.usage]),"primitive"in a&&(d=Ra[a.primitive]),"count"in a&&(f=a.count|0),"type"in a&&(p=r[a.type]),"length"in a)l=a.length|0;else if(l=f,5123===p||5122===p)l*=2;else if(5125===p||5124===p)l*=4;k(n,b,c,d,f,l,p)}else e(),n.primType=4,n.vertCount=0,n.type=5121;return g}var e=c.create(null,34963,!0),n=new f(e._buffer);d.elementsCount++;g(a);g._reglType="elements";g._elements=n;g.subdata=function(a,b){e.subdata(a,b);return g};g.destroy=function(){p(n)};
return g},createStream:function(a){var b=v.pop();b||(b=new f(c.create(null,34963,!0,!1)._buffer));k(b,a,35040,-1,-1,0,0);return b},destroyStream:function(a){v.push(a)},getElements:function(a){return"function"===typeof a&&a._elements instanceof f?a._elements:null},clear:function(){H(h).forEach(p)}}}function ib(a){for(var b=x.allocType(5123,a.length),c=0;c<a.length;++c)if(isNaN(a[c]))b[c]=65535;else if(Infinity===a[c])b[c]=31744;else if(-Infinity===a[c])b[c]=64512;else{jb[0]=a[c];var d=Db[0],f=d>>>
31<<15,k=(d<<1>>>24)-127,d=d>>13&1023;b[c]=-24>k?f:-14>k?f+(d+1024>>-14-k):15<k?f+31744:f+(k+15<<10)+d}return b}function na(a){return Array.isArray(a)||O(a)}function kb(a){return Array.isArray(a)&&(0===a.length||"number"===typeof a[0])}function lb(a){return Array.isArray(a)&&0!==a.length&&na(a[0])?!0:!1}function oa(a){return Object.prototype.toString.call(a)}function Sa(a){if(!a)return!1;var b=oa(a);return 0<=Eb.indexOf(b)?!0:kb(a)||lb(a)||ja(a)}function mb(a,b){36193===a.type?(a.data=ib(b),x.freeType(b)):
a.data=b}function Ia(a,b,c,d,f,k){a="undefined"!==typeof C[a]?C[a]:R[a]*za[b];k&&(a*=6);if(f){for(d=0;1<=c;)d+=a*c*c,c/=2;return d}return a*c*d}function Fb(a,b,c,d,f,k,p){function h(){this.format=this.internalformat=6408;this.type=5121;this.flipY=this.premultiplyAlpha=this.compressed=!1;this.unpackAlignment=1;this.colorSpace=37444;this.channels=this.height=this.width=0}function g(a,b){a.internalformat=b.internalformat;a.format=b.format;a.type=b.type;a.compressed=b.compressed;a.premultiplyAlpha=b.premultiplyAlpha;
a.flipY=b.flipY;a.unpackAlignment=b.unpackAlignment;a.colorSpace=b.colorSpace;a.width=b.width;a.height=b.height;a.channels=b.channels}function r(a,b){if("object"===typeof b&&b){"premultiplyAlpha"in b&&(a.premultiplyAlpha=b.premultiplyAlpha);"flipY"in b&&(a.flipY=b.flipY);"alignment"in b&&(a.unpackAlignment=b.alignment);"colorSpace"in b&&(a.colorSpace=ua[b.colorSpace]);"type"in b&&(a.type=E[b.type]);var c=a.width,g=a.height,e=a.channels,d=!1;"shape"in b?(c=b.shape[0],g=b.shape[1],3===b.shape.length&&
(e=b.shape[2],d=!0)):("radius"in b&&(c=g=b.radius),"width"in b&&(c=b.width),"height"in b&&(g=b.height),"channels"in b&&(e=b.channels,d=!0));a.width=c|0;a.height=g|0;a.channels=e|0;c=!1;"format"in b&&(c=b.format,g=a.internalformat=S[c],a.format=Gb[g],c in E&&!("type"in b)&&(a.type=E[c]),c in U&&(a.compressed=!0),c=!0);!d&&c?a.channels=R[a.format]:d&&!c&&a.channels!==Ka[a.format]&&(a.format=a.internalformat=Ka[a.channels])}}function v(b){a.pixelStorei(37440,b.flipY);a.pixelStorei(37441,b.premultiplyAlpha);
a.pixelStorei(37443,b.colorSpace);a.pixelStorei(3317,b.unpackAlignment)}function l(){h.call(this);this.yOffset=this.xOffset=0;this.data=null;this.needsFree=!1;this.element=null;this.needsCopy=!1}function u(a,b){var c=null;Sa(b)?c=b:b&&(r(a,b),"x"in b&&(a.xOffset=b.x|0),"y"in b&&(a.yOffset=b.y|0),Sa(b.data)&&(c=b.data));if(b.copy){var g=f.viewportWidth,e=f.viewportHeight;a.width=a.width||g-a.xOffset;a.height=a.height||e-a.yOffset;a.needsCopy=!0}else if(!c)a.width=a.width||1,a.height=a.height||1,a.channels=
a.channels||4;else if(O(c))a.channels=a.channels||4,a.data=c,"type"in b||5121!==a.type||(a.type=Ha[Object.prototype.toString.call(c)]|0);else if(kb(c)){a.channels=a.channels||4;g=c;e=g.length;switch(a.type){case 5121:case 5123:case 5125:case 5126:e=x.allocType(a.type,e);e.set(g);a.data=e;break;case 36193:a.data=ib(g)}a.alignment=1;a.needsFree=!0}else if(ja(c)){g=c.data;Array.isArray(g)||5121!==a.type||(a.type=Ha[Object.prototype.toString.call(g)]|0);var e=c.shape,d=c.stride,n,l,q,y;3===e.length?(q=
e[2],y=d[2]):y=q=1;n=e[0];l=e[1];e=d[0];d=d[1];a.alignment=1;a.width=n;a.height=l;a.channels=q;a.format=a.internalformat=Ka[q];a.needsFree=!0;n=y;c=c.offset;q=a.width;y=a.height;l=a.channels;for(var K=x.allocType(36193===a.type?5126:a.type,q*y*l),aa=0,va=0;va<y;++va)for(var wa=0;wa<q;++wa)for(var ca=0;ca<l;++ca)K[aa++]=g[e*wa+d*va+n*ca+c];mb(a,K)}else if("[object HTMLCanvasElement]"===oa(c)||"[object CanvasRenderingContext2D]"===oa(c))"[object HTMLCanvasElement]"===oa(c)?a.element=c:a.element=c.canvas,
a.width=a.element.width,a.height=a.element.height,a.channels=4;else if("[object HTMLImageElement]"===oa(c))a.element=c,a.width=c.naturalWidth,a.height=c.naturalHeight,a.channels=4;else if("[object HTMLVideoElement]"===oa(c))a.element=c,a.width=c.videoWidth,a.height=c.videoHeight,a.channels=4;else if(lb(c)){g=a.width||c[0].length;e=a.height||c.length;d=a.channels;d=na(c[0][0])?d||c[0][0].length:d||1;n=La.shape(c);q=1;for(y=0;y<n.length;++y)q*=n[y];q=x.allocType(36193===a.type?5126:a.type,q);La.flatten(c,
n,"",q);mb(a,q);a.alignment=1;a.width=g;a.height=e;a.channels=d;a.format=a.internalformat=Ka[d];a.needsFree=!0}}function m(b,c,g,e,f){var n=b.element,l=b.data,k=b.internalformat,q=b.format,y=b.type,K=b.width,aa=b.height;v(b);n?a.texSubImage2D(c,f,g,e,q,y,n):b.compressed?a.compressedTexSubImage2D(c,f,g,e,k,K,aa,l):b.needsCopy?(d(),a.copyTexSubImage2D(c,f,g,e,b.xOffset,b.yOffset,K,aa)):a.texSubImage2D(c,f,g,e,K,aa,q,y,l)}function e(){return N.pop()||new l}function n(a){a.needsFree&&x.freeType(a.data);
l.call(a);N.push(a)}function D(){h.call(this);this.genMipmaps=!1;this.mipmapHint=4352;this.mipmask=0;this.images=Array(16)}function w(a,b,c){var g=a.images[0]=e();a.mipmask=1;g.width=a.width=b;g.height=a.height=c;g.channels=a.channels=4}function t(a,b){var c=null;if(Sa(b))c=a.images[0]=e(),g(c,a),u(c,b),a.mipmask=1;else if(r(a,b),Array.isArray(b.mipmap))for(var d=b.mipmap,f=0;f<d.length;++f)c=a.images[f]=e(),g(c,a),c.width>>=f,c.height>>=f,u(c,d[f]),a.mipmask|=1<<f;else c=a.images[0]=e(),g(c,a),u(c,
b),a.mipmask=1;g(a,a.images[0])}function Z(b,c){for(var g=b.images,e=0;e<g.length&&g[e];++e){var f=g[e],n=c,l=e,k=f.element,q=f.data,y=f.internalformat,K=f.format,aa=f.type,va=f.width,wa=f.height;v(f);k?a.texImage2D(n,l,K,K,aa,k):f.compressed?a.compressedTexImage2D(n,l,y,va,wa,0,q):f.needsCopy?(d(),a.copyTexImage2D(n,l,K,f.xOffset,f.yOffset,va,wa,0)):a.texImage2D(n,l,K,va,wa,0,K,aa,q)}}function B(){var a=nb.pop()||new D;h.call(a);for(var b=a.mipmask=0;16>b;++b)a.images[b]=null;return a}function gb(a){for(var b=
a.images,c=0;c<b.length;++c)b[c]&&n(b[c]),b[c]=null;nb.push(a)}function C(){this.magFilter=this.minFilter=9728;this.wrapT=this.wrapS=33071;this.anisotropic=1;this.genMipmaps=!1;this.mipmapHint=4352}function M(a,b){"min"in b&&(a.minFilter=Ta[b.min],0<=Hb.indexOf(a.minFilter)&&(a.genMipmaps=!0));"mag"in b&&(a.magFilter=T[b.mag]);var c=a.wrapS,g=a.wrapT;if("wrap"in b){var e=b.wrap;"string"===typeof e?c=g=I[e]:Array.isArray(e)&&(c=I[e[0]],g=I[e[1]])}else"wrapS"in b&&(c=I[b.wrapS]),"wrapT"in b&&(g=I[b.wrapT]);
a.wrapS=c;a.wrapT=g;"anisotropic"in b&&(a.anisotropic=b.anisotropic);if("mipmap"in b){c=!1;switch(typeof b.mipmap){case "string":a.mipmapHint=sa[b.mipmap];c=a.genMipmaps=!0;break;case "boolean":c=a.genMipmaps=b.mipmap;break;case "object":a.genMipmaps=!1,c=!0}!c||"min"in b||(a.minFilter=9984)}}function P(c,g){a.texParameteri(g,10241,c.minFilter);a.texParameteri(g,10240,c.magFilter);a.texParameteri(g,10242,c.wrapS);a.texParameteri(g,10243,c.wrapT);b.ext_texture_filter_anisotropic&&a.texParameteri(g,
34046,c.anisotropic);c.genMipmaps&&(a.hint(33170,c.mipmapHint),a.generateMipmap(g))}function G(b){h.call(this);this.mipmask=0;this.internalformat=6408;this.id=xa++;this.refCount=1;this.target=b;this.texture=a.createTexture();this.unit=-1;this.bindCount=0;this.texInfo=new C;p.profile&&(this.stats={size:0})}function Q(b){a.activeTexture(33984);a.bindTexture(b.target,b.texture)}function Aa(){var b=ka[0];b?a.bindTexture(b.target,b.texture):a.bindTexture(3553,null)}function z(b){var c=b.texture,g=b.unit,
e=b.target;0<=g&&(a.activeTexture(33984+g),a.bindTexture(e,null),ka[g]=null);a.deleteTexture(c);b.texture=null;b.params=null;b.pixels=null;b.refCount=0;delete V[b.id];k.textureCount--}var sa={"don't care":4352,"dont care":4352,nice:4354,fast:4353},I={repeat:10497,clamp:33071,mirror:33648},T={nearest:9728,linear:9729},Ta=A({mipmap:9987,"nearest mipmap nearest":9984,"linear mipmap nearest":9985,"nearest mipmap linear":9986,"linear mipmap linear":9987},T),ua={none:0,browser:37444},E={uint8:5121,rgba4:32819,
rgb565:33635,"rgb5 a1":32820},S={alpha:6406,luminance:6409,"luminance alpha":6410,rgb:6407,rgba:6408,rgba4:32854,"rgb5 a1":32855,rgb565:36194},U={};b.ext_srgb&&(S.srgb=35904,S.srgba=35906);b.oes_texture_float&&(E.float32=E["float"]=5126);b.oes_texture_half_float&&(E.float16=E["half float"]=36193);b.webgl_depth_texture&&(A(S,{depth:6402,"depth stencil":34041}),A(E,{uint16:5123,uint32:5125,"depth stencil":34042}));b.webgl_compressed_texture_s3tc&&A(U,{"rgb s3tc dxt1":33776,"rgba s3tc dxt1":33777,"rgba s3tc dxt3":33778,
"rgba s3tc dxt5":33779});b.webgl_compressed_texture_atc&&A(U,{"rgb atc":35986,"rgba atc explicit alpha":35987,"rgba atc interpolated alpha":34798});b.webgl_compressed_texture_pvrtc&&A(U,{"rgb pvrtc 4bppv1":35840,"rgb pvrtc 2bppv1":35841,"rgba pvrtc 4bppv1":35842,"rgba pvrtc 2bppv1":35843});b.webgl_compressed_texture_etc1&&(U["rgb etc1"]=36196);var Ib=Array.prototype.slice.call(a.getParameter(34467));Object.keys(U).forEach(function(a){var b=U[a];0<=Ib.indexOf(b)&&(S[a]=b)});var ba=Object.keys(S);c.textureFormats=
ba;var J=[];Object.keys(S).forEach(function(a){J[S[a]]=a});var W=[];Object.keys(E).forEach(function(a){W[E[a]]=a});var la=[];Object.keys(T).forEach(function(a){la[T[a]]=a});var ya=[];Object.keys(Ta).forEach(function(a){ya[Ta[a]]=a});var ha=[];Object.keys(I).forEach(function(a){ha[I[a]]=a});var Gb=ba.reduce(function(a,b){var c=S[b];6409===c||6406===c||6409===c||6410===c||6402===c||34041===c?a[c]=c:32855===c||0<=b.indexOf("rgba")?a[c]=6408:a[c]=6407;return a},{}),N=[],nb=[],xa=0,V={},ma=c.maxTextureUnits,
ka=Array(ma).map(function(){return null});A(G.prototype,{bind:function(){this.bindCount+=1;var b=this.unit;if(0>b){for(var c=0;c<ma;++c){var g=ka[c];if(g){if(0<g.bindCount)continue;g.unit=-1}ka[c]=this;b=c;break}p.profile&&k.maxTextureUnits<b+1&&(k.maxTextureUnits=b+1);this.unit=b;a.activeTexture(33984+b);a.bindTexture(this.target,this.texture)}return b},unbind:function(){--this.bindCount},decRef:function(){0>=--this.refCount&&z(this)}});p.profile&&(k.getTotalTextureSize=function(){var a=0;Object.keys(V).forEach(function(b){a+=
V[b].stats.size});return a});return{create2D:function(b,c){function d(a,b){var c=f.texInfo;C.call(c);var e=B();"number"===typeof a?"number"===typeof b?w(e,a|0,b|0):w(e,a|0,a|0):a?(M(c,a),t(e,a)):w(e,1,1);c.genMipmaps&&(e.mipmask=(e.width<<1)-1);f.mipmask=e.mipmask;g(f,e);f.internalformat=e.internalformat;d.width=e.width;d.height=e.height;Q(f);Z(e,3553);P(c,3553);Aa();gb(e);p.profile&&(f.stats.size=Ia(f.internalformat,f.type,e.width,e.height,c.genMipmaps,!1));d.format=J[f.internalformat];d.type=W[f.type];
d.mag=la[c.magFilter];d.min=ya[c.minFilter];d.wrapS=ha[c.wrapS];d.wrapT=ha[c.wrapT];return d}var f=new G(3553);V[f.id]=f;k.textureCount++;d(b,c);d.subimage=function(a,b,c,l){b|=0;c|=0;l|=0;var q=e();g(q,f);q.width=0;q.height=0;u(q,a);q.width=q.width||(f.width>>l)-b;q.height=q.height||(f.height>>l)-c;Q(f);m(q,3553,b,c,l);Aa();n(q);return d};d.resize=function(b,c){var e=b|0,g=c|0||e;if(e===f.width&&g===f.height)return d;d.width=f.width=e;d.height=f.height=g;Q(f);for(var q=0;f.mipmask>>q;++q)a.texImage2D(3553,
q,f.format,e>>q,g>>q,0,f.format,f.type,null);Aa();p.profile&&(f.stats.size=Ia(f.internalformat,f.type,e,g,!1,!1));return d};d._reglType="texture2d";d._texture=f;p.profile&&(d.stats=f.stats);d.destroy=function(){f.decRef()};return d},createCube:function(b,c,d,f,l,sa){function z(a,b,c,e,d,f){var F,X=h.texInfo;C.call(X);for(F=0;6>F;++F)q[F]=B();if("number"===typeof a||!a)for(a=a|0||1,F=0;6>F;++F)w(q[F],a,a);else if("object"===typeof a)if(b)t(q[0],a),t(q[1],b),t(q[2],c),t(q[3],e),t(q[4],d),t(q[5],f);
else if(M(X,a),r(h,a),"faces"in a)for(a=a.faces,F=0;6>F;++F)g(q[F],h),t(q[F],a[F]);else for(F=0;6>F;++F)t(q[F],a);g(h,q[0]);h.mipmask=X.genMipmaps?(q[0].width<<1)-1:q[0].mipmask;h.internalformat=q[0].internalformat;z.width=q[0].width;z.height=q[0].height;Q(h);for(F=0;6>F;++F)Z(q[F],34069+F);P(X,34067);Aa();p.profile&&(h.stats.size=Ia(h.internalformat,h.type,z.width,z.height,X.genMipmaps,!0));z.format=J[h.internalformat];z.type=W[h.type];z.mag=la[X.magFilter];z.min=ya[X.minFilter];z.wrapS=ha[X.wrapS];
z.wrapT=ha[X.wrapT];for(F=0;6>F;++F)gb(q[F]);return z}var h=new G(34067);V[h.id]=h;k.cubeCount++;var q=Array(6);z(b,c,d,f,l,sa);z.subimage=function(a,b,c,q,d){c|=0;q|=0;d|=0;var f=e();g(f,h);f.width=0;f.height=0;u(f,b);f.width=f.width||(h.width>>d)-c;f.height=f.height||(h.height>>d)-q;Q(h);m(f,34069+a,c,q,d);Aa();n(f);return z};z.resize=function(b){b|=0;if(b!==h.width){z.width=h.width=b;z.height=h.height=b;Q(h);for(var c=0;6>c;++c)for(var q=0;h.mipmask>>q;++q)a.texImage2D(34069+c,q,h.format,b>>q,
b>>q,0,h.format,h.type,null);Aa();p.profile&&(h.stats.size=Ia(h.internalformat,h.type,z.width,z.height,!1,!0));return z}};z._reglType="textureCube";z._texture=h;p.profile&&(z.stats=h.stats);z.destroy=function(){h.decRef()};return z},clear:function(){for(var b=0;b<ma;++b)a.activeTexture(33984+b),a.bindTexture(3553,null),ka[b]=null;H(V).forEach(z);k.cubeCount=0;k.textureCount=0},getTexture:function(a){return null},restore:function(){H(V).forEach(function(b){b.texture=a.createTexture();a.bindTexture(b.target,
b.texture);for(var c=0;32>c;++c)if(0!==(b.mipmask&1<<c))if(3553===b.target)a.texImage2D(3553,c,b.internalformat,b.width>>c,b.height>>c,0,b.internalformat,b.type,null);else for(var e=0;6>e;++e)a.texImage2D(34069+e,c,b.internalformat,b.width>>c,b.height>>c,0,b.internalformat,b.type,null);P(b.texInfo,b.target)})}}}function Jb(a,b,c,d,f,k){function p(a,b,c){this.target=a;this.texture=b;this.renderbuffer=c;var e=a=0;b?(a=b.width,e=b.height):c&&(a=c.width,e=c.height);this.width=a;this.height=e}function h(a){a&&
(a.texture&&a.texture._texture.decRef(),a.renderbuffer&&a.renderbuffer._renderbuffer.decRef())}function g(a,b,c){a&&(a.texture?a.texture._texture.refCount+=1:a.renderbuffer._renderbuffer.refCount+=1)}function r(b,c){c&&(c.texture?a.framebufferTexture2D(36160,b,c.target,c.texture._texture.texture,0):a.framebufferRenderbuffer(36160,b,36161,c.renderbuffer._renderbuffer.renderbuffer))}function v(a){var b=3553,c=null,e=null,g=a;"object"===typeof a&&(g=a.data,"target"in a&&(b=a.target|0));a=g._reglType;
"texture2d"===a?c=g:"textureCube"===a?c=g:"renderbuffer"===a&&(e=g,b=36161);return new p(b,c,e)}function l(a,b,c,e,g){if(c)return a=d.create2D({width:a,height:b,format:e,type:g}),a._texture.refCount=0,new p(3553,a,null);a=f.create({width:a,height:b,format:e});a._renderbuffer.refCount=0;return new p(36161,null,a)}function u(a){return a&&(a.texture||a.renderbuffer)}function m(a,b,c){a&&(a.texture?a.texture.resize(b,c):a.renderbuffer&&a.renderbuffer.resize(b,c))}function e(){this.id=M++;P[this.id]=this;
this.framebuffer=a.createFramebuffer();this.height=this.width=0;this.colorAttachments=[];this.depthStencilAttachment=this.stencilAttachment=this.depthAttachment=null}function n(a){a.colorAttachments.forEach(h);h(a.depthAttachment);h(a.stencilAttachment);h(a.depthStencilAttachment)}function D(b){a.deleteFramebuffer(b.framebuffer);b.framebuffer=null;k.framebufferCount--;delete P[b.id]}function w(b){var e;a.bindFramebuffer(36160,b.framebuffer);var g=b.colorAttachments;for(e=0;e<g.length;++e)r(36064+
e,g[e]);for(e=g.length;e<c.maxColorAttachments;++e)a.framebufferTexture2D(36160,36064+e,3553,null,0);a.framebufferTexture2D(36160,33306,3553,null,0);a.framebufferTexture2D(36160,36096,3553,null,0);a.framebufferTexture2D(36160,36128,3553,null,0);r(36096,b.depthAttachment);r(36128,b.stencilAttachment);r(33306,b.depthStencilAttachment);a.checkFramebufferStatus(36160);a.bindFramebuffer(36160,Z.next);Z.cur=Z.next;a.getError()}function t(a,b){function c(a,b){var e,f=0,h=0,k=!0,m=!0;e=null;var p=!0,t="rgba",
r="uint8",D=1,W=null,la=null,Z=null,ha=!1;if("number"===typeof a)f=a|0,h=b|0||f;else if(a){"shape"in a?(h=a.shape,f=h[0],h=h[1]):("radius"in a&&(f=h=a.radius),"width"in a&&(f=a.width),"height"in a&&(h=a.height));if("color"in a||"colors"in a)e=a.color||a.colors,Array.isArray(e);if(!e){"colorCount"in a&&(D=a.colorCount|0);"colorTexture"in a&&(p=!!a.colorTexture,t="rgba4");if("colorType"in a&&(r=a.colorType,!p))if("half float"===r||"float16"===r)t="rgba16f";else if("float"===r||"float32"===r)t="rgba32f";
"colorFormat"in a&&(t=a.colorFormat,0<=B.indexOf(t)?p=!0:0<=C.indexOf(t)&&(p=!1))}if("depthTexture"in a||"depthStencilTexture"in a)ha=!(!a.depthTexture&&!a.depthStencilTexture);"depth"in a&&("boolean"===typeof a.depth?k=a.depth:(W=a.depth,m=!1));"stencil"in a&&("boolean"===typeof a.stencil?m=a.stencil:(la=a.stencil,k=!1));"depthStencil"in a&&("boolean"===typeof a.depthStencil?k=m=a.depthStencil:(Z=a.depthStencil,m=k=!1))}else f=h=1;var G=null,A=null,Q=null,x=null;if(Array.isArray(e))G=e.map(v);else if(e)G=
[v(e)];else for(G=Array(D),e=0;e<D;++e)G[e]=l(f,h,p,t,r);f=f||G[0].width;h=h||G[0].height;W?A=v(W):k&&!m&&(A=l(f,h,ha,"depth","uint32"));la?Q=v(la):m&&!k&&(Q=l(f,h,!1,"stencil","uint8"));Z?x=v(Z):!W&&!la&&m&&k&&(x=l(f,h,ha,"depth stencil","depth stencil"));k=null;for(e=0;e<G.length;++e)g(G[e],f,h),G[e]&&G[e].texture&&(m=ob[G[e].texture._texture.format]*Ma[G[e].texture._texture.type],null===k&&(k=m));g(A,f,h);g(Q,f,h);g(x,f,h);n(d);d.width=f;d.height=h;d.colorAttachments=G;d.depthAttachment=A;d.stencilAttachment=
Q;d.depthStencilAttachment=x;c.color=G.map(u);c.depth=u(A);c.stencil=u(Q);c.depthStencil=u(x);c.width=d.width;c.height=d.height;w(d);return c}var d=new e;k.framebufferCount++;c(a,b);return A(c,{resize:function(a,b){var e=a|0,g=b|0||e;if(e===d.width&&g===d.height)return c;for(var f=d.colorAttachments,h=0;h<f.length;++h)m(f[h],e,g);m(d.depthAttachment,e,g);m(d.stencilAttachment,e,g);m(d.depthStencilAttachment,e,g);d.width=c.width=e;d.height=c.height=g;w(d);return c},_reglType:"framebuffer",_framebuffer:d,
destroy:function(){D(d);n(d)},use:function(a){Z.setFBO({framebuffer:c},a)}})}var Z={cur:null,next:null,dirty:!1,setFBO:null},B=["rgba"],C=["rgba4","rgb565","rgb5 a1"];b.ext_srgb&&C.push("srgba");b.ext_color_buffer_half_float&&C.push("rgba16f","rgb16f");b.webgl_color_buffer_float&&C.push("rgba32f");var x=["uint8"];b.oes_texture_half_float&&x.push("half float","float16");b.oes_texture_float&&x.push("float","float32");var M=0,P={};return A(Z,{getFramebuffer:function(a){return"function"===typeof a&&"framebuffer"===
a._reglType&&(a=a._framebuffer,a instanceof e)?a:null},create:t,createCube:function(a){function b(a){var e,g={color:null},f=0,h=null;e="rgba";var l="uint8",n=1;if("number"===typeof a)f=a|0;else if(a){"shape"in a?f=a.shape[0]:("radius"in a&&(f=a.radius|0),"width"in a?f=a.width|0:"height"in a&&(f=a.height|0));if("color"in a||"colors"in a)h=a.color||a.colors,Array.isArray(h);h||("colorCount"in a&&(n=a.colorCount|0),"colorType"in a&&(l=a.colorType),"colorFormat"in a&&(e=a.colorFormat));"depth"in a&&(g.depth=
a.depth);"stencil"in a&&(g.stencil=a.stencil);"depthStencil"in a&&(g.depthStencil=a.depthStencil)}else f=1;if(h)if(Array.isArray(h))for(a=[],e=0;e<h.length;++e)a[e]=h[e];else a=[h];else for(a=Array(n),h={radius:f,format:e,type:l},e=0;e<n;++e)a[e]=d.createCube(h);g.color=Array(a.length);for(e=0;e<a.length;++e)n=a[e],f=f||n.width,g.color[e]={target:34069,data:a[e]};for(e=0;6>e;++e){for(n=0;n<a.length;++n)g.color[n].target=34069+e;0<e&&(g.depth=c[0].depth,g.stencil=c[0].stencil,g.depthStencil=c[0].depthStencil);
if(c[e])c[e](g);else c[e]=t(g)}return A(b,{width:f,height:f,color:a})}var c=Array(6);b(a);return A(b,{faces:c,resize:function(a){var e=a|0;if(e===b.width)return b;var g=b.color;for(a=0;a<g.length;++a)g[a].resize(e);for(a=0;6>a;++a)c[a].resize(e);b.width=b.height=e;return b},_reglType:"framebufferCube",destroy:function(){c.forEach(function(a){a.destroy()})}})},clear:function(){H(P).forEach(D)},restore:function(){H(P).forEach(function(b){b.framebuffer=a.createFramebuffer();w(b)})}})}function pb(){this.w=
this.z=this.y=this.x=this.state=0;this.buffer=null;this.size=0;this.normalized=!1;this.type=5126;this.divisor=this.stride=this.offset=0}function Kb(a,b,c,d,f){a=c.maxAttributes;b=Array(a);for(c=0;c<a;++c)b[c]=new pb;return{Record:pb,scope:{},state:b}}function Lb(a,b,c,d){function f(a,b,c,g){this.name=a;this.id=b;this.location=c;this.info=g}function k(a,b){for(var c=0;c<a.length;++c)if(a[c].id===b.id){a[c].location=b.location;return}a.push(b)}function p(c,g,d){d=35632===c?r:v;var f=d[g];if(!f){var h=
b.str(g),f=a.createShader(c);a.shaderSource(f,h);a.compileShader(f);d[g]=f}return f}function h(a,b){this.id=m++;this.fragId=a;this.vertId=b;this.program=null;this.uniforms=[];this.attributes=[];d.profile&&(this.stats={uniformsCount:0,attributesCount:0})}function g(c,g){var h,l;h=p(35632,c.fragId);l=p(35633,c.vertId);var m=c.program=a.createProgram();a.attachShader(m,h);a.attachShader(m,l);a.linkProgram(m);var r=a.getProgramParameter(m,35718);d.profile&&(c.stats.uniformsCount=r);var u=c.uniforms;for(h=
0;h<r;++h)if(l=a.getActiveUniform(m,h))if(1<l.size)for(var v=0;v<l.size;++v){var A=l.name.replace("[0]","["+v+"]");k(u,new f(A,b.id(A),a.getUniformLocation(m,A),l))}else k(u,new f(l.name,b.id(l.name),a.getUniformLocation(m,l.name),l));r=a.getProgramParameter(m,35721);d.profile&&(c.stats.attributesCount=r);u=c.attributes;for(h=0;h<r;++h)(l=a.getActiveAttrib(m,h))&&k(u,new f(l.name,b.id(l.name),a.getAttribLocation(m,l.name),l))}var r={},v={},l={},u=[],m=0;d.profile&&(c.getMaxUniformsCount=function(){var a=
0;u.forEach(function(b){b.stats.uniformsCount>a&&(a=b.stats.uniformsCount)});return a},c.getMaxAttributesCount=function(){var a=0;u.forEach(function(b){b.stats.attributesCount>a&&(a=b.stats.attributesCount)});return a});return{clear:function(){var b=a.deleteShader.bind(a);H(r).forEach(b);r={};H(v).forEach(b);v={};u.forEach(function(b){a.deleteProgram(b.program)});u.length=0;l={};c.shaderCount=0},program:function(a,b,f){var d=l[b];d||(d=l[b]={});var m=d[a];m||(m=new h(b,a),c.shaderCount++,g(m,f),d[a]=
m,u.push(m));return m},restore:function(){r={};v={};for(var a=0;a<u.length;++a)g(u[a])},shader:p,frag:-1,vert:-1}}function Mb(a,b,c,d,f,k){function p(g){var f;f=null===b.next?5121:b.next.colorAttachments[0].texture._texture.type;var h=0,l=0,k=d.framebufferWidth,m=d.framebufferHeight,e=null;O(g)?e=g:g&&(h=g.x|0,l=g.y|0,k=(g.width||d.framebufferWidth-h)|0,m=(g.height||d.framebufferHeight-l)|0,e=g.data||null);c();g=k*m*4;e||(5121===f?e=new Uint8Array(g):5126===f&&(e=e||new Float32Array(g)));a.pixelStorei(3333,
4);a.readPixels(h,l,k,m,6408,f,e);return e}function h(a){var c;b.setFBO({framebuffer:a.framebuffer},function(){c=p(a)});return c}return function(a){return a&&"framebuffer"in a?h(a):p(a)}}function Ba(a){return Array.prototype.slice.call(a)}function Ca(a){return Ba(a).join("")}function Nb(){function a(){var a=[],b=[];return A(function(){a.push.apply(a,Ba(arguments))},{def:function(){var f="v"+c++;b.push(f);0<arguments.length&&(a.push(f,"="),a.push.apply(a,Ba(arguments)),a.push(";"));return f},toString:function(){return Ca([0<
b.length?"var "+b+";":"",Ca(a)])}})}function b(){function b(a,d){f(a,d,"=",c.def(a,d),";")}var c=a(),f=a(),d=c.toString,l=f.toString;return A(function(){c.apply(c,Ba(arguments))},{def:c.def,entry:c,exit:f,save:b,set:function(a,f,e){b(a,f);c(a,f,"=",e,";")},toString:function(){return d()+l()}})}var c=0,d=[],f=[],k=a(),p={};return{global:k,link:function(a){for(var b=0;b<f.length;++b)if(f[b]===a)return d[b];b="g"+c++;d.push(b);f.push(a);return b},block:a,proc:function(a,c){function f(){var a="a"+d.length;
d.push(a);return a}var d=[];c=c||0;for(var l=0;l<c;++l)f();var l=b(),k=l.toString;return p[a]=A(l,{arg:f,toString:function(){return Ca(["function(",d.join(),"){",k(),"}"])}})},scope:b,cond:function(){var a=Ca(arguments),c=b(),f=b(),d=c.toString,l=f.toString;return A(c,{then:function(){c.apply(c,Ba(arguments));return this},"else":function(){f.apply(f,Ba(arguments));return this},toString:function(){var b=l();b&&(b="else{"+b+"}");return Ca(["if(",a,"){",d(),"}",b])}})},compile:function(){var a=['"use strict";',
k,"return {"];Object.keys(p).forEach(function(b){a.push('"',b,'":',p[b].toString(),",")});a.push("}");var b=Ca(a).replace(/;/g,";\n").replace(/}/g,"}\n").replace(/{/g,"{\n");return Function.apply(null,d.concat(b)).apply(null,f)}}}function Na(a){return Array.isArray(a)||O(a)||ja(a)}function qb(a){return a.sort(function(a,c){return"viewport"===a?-1:"viewport"===c?1:a<c?-1:1})}function Y(a,b,c,d){this.thisDep=a;this.contextDep=b;this.propDep=c;this.append=d}function ta(a){return a&&!(a.thisDep||a.contextDep||
a.propDep)}function B(a){return new Y(!1,!1,!1,a)}function N(a,b){var c=a.type;return 0===c?(c=a.data.length,new Y(!0,1<=c,2<=c,b)):4===c?(c=a.data,new Y(c.thisDep,c.contextDep,c.propDep,b)):new Y(3===c,2===c,1===c,b)}function Ob(a,b,c,d,f,k,p,h,g,r,v,l,u,m,e){function n(a){return a.replace(".","_")}function D(a,b,c){var e=n(a);Ja.push(a);Ea[e]=pa[e]=!!c;qa[e]=b}function w(a,b,c){var e=n(a);Ja.push(a);Array.isArray(c)?(pa[e]=c.slice(),Ea[e]=c.slice()):pa[e]=Ea[e]=c;ra[e]=b}function t(){var a=Nb(),
c=a.link,e=a.global;a.id=oa++;a.batchId="0";var f=c(ga),d=a.shared={props:"a0"};Object.keys(ga).forEach(function(a){d[a]=e.def(f,".",a)});var g=a.next={},ca=a.current={};Object.keys(ra).forEach(function(a){Array.isArray(pa[a])&&(g[a]=e.def(d.next,".",a),ca[a]=e.def(d.current,".",a))});var F=a.constants={};Object.keys(da).forEach(function(a){F[a]=e.def(JSON.stringify(da[a]))});a.invoke=function(b,e){switch(e.type){case 0:var X=["this",d.context,d.props,a.batchId];return b.def(c(e.data),".call(",X.slice(0,
Math.max(e.data.length+1,4)),")");case 1:return b.def(d.props,e.data);case 2:return b.def(d.context,e.data);case 3:return b.def("this",e.data);case 4:return e.data.append(a,b),e.data.ref}};a.attribCache={};var X={};a.scopeAttrib=function(a){a=b.id(a);if(a in X)return X[a];var e=r.scope[a];e||(e=r.scope[a]=new xa);return X[a]=c(e)};return a}function Z(a){var b=a["static"];a=a.dynamic;var c;if("profile"in b){var e=!!b.profile;c=B(function(a,b){return e});c.enable=e}else if("profile"in a){var f=a.profile;
c=N(f,function(a,b){return a.invoke(b,f)})}return c}function A(a,b){var c=a["static"],e=a.dynamic;if("framebuffer"in c){var f=c.framebuffer;return f?(f=h.getFramebuffer(f),B(function(a,b){var c=a.link(f),e=a.shared;b.set(e.framebuffer,".next",c);e=e.context;b.set(e,".framebufferWidth",c+".width");b.set(e,".framebufferHeight",c+".height");return c})):B(function(a,b){var c=a.shared;b.set(c.framebuffer,".next","null");c=c.context;b.set(c,".framebufferWidth",c+".drawingBufferWidth");b.set(c,".framebufferHeight",
c+".drawingBufferHeight");return"null"})}if("framebuffer"in e){var d=e.framebuffer;return N(d,function(a,b){var c=a.invoke(b,d),e=a.shared,q=e.framebuffer,c=b.def(q,".getFramebuffer(",c,")");b.set(q,".next",c);e=e.context;b.set(e,".framebufferWidth",c+"?"+c+".width:"+e+".drawingBufferWidth");b.set(e,".framebufferHeight",c+"?"+c+".height:"+e+".drawingBufferHeight");return c})}return null}function x(a,b,c){function e(a){if(a in f){var c=f[a];a=!0;var q=c.x|0,g=c.y|0,K,ca;"width"in c?K=c.width|0:a=!1;
"height"in c?ca=c.height|0:a=!1;return new Y(!a&&b&&b.thisDep,!a&&b&&b.contextDep,!a&&b&&b.propDep,function(a,b){var e=a.shared.context,f=K;"width"in c||(f=b.def(e,".","framebufferWidth","-",q));var d=ca;"height"in c||(d=b.def(e,".","framebufferHeight","-",g));return[q,g,f,d]})}if(a in d){var l=d[a];a=N(l,function(a,b){var c=a.invoke(b,l),e=a.shared.context,q=b.def(c,".x|0"),f=b.def(c,".y|0"),d=b.def('"width" in ',c,"?",c,".width|0:","(",e,".","framebufferWidth","-",q,")"),c=b.def('"height" in ',
c,"?",c,".height|0:","(",e,".","framebufferHeight","-",f,")");return[q,f,d,c]});b&&(a.thisDep=a.thisDep||b.thisDep,a.contextDep=a.contextDep||b.contextDep,a.propDep=a.propDep||b.propDep);return a}return b?new Y(b.thisDep,b.contextDep,b.propDep,function(a,b){var c=a.shared.context;return[0,0,b.def(c,".","framebufferWidth"),b.def(c,".","framebufferHeight")]}):null}var f=a["static"],d=a.dynamic;if(a=e("viewport")){var g=a;a=new Y(a.thisDep,a.contextDep,a.propDep,function(a,b){var c=g.append(a,b),e=a.shared.context;
b.set(e,".viewportWidth",c[2]);b.set(e,".viewportHeight",c[3]);return c})}return{viewport:a,scissor_box:e("scissor.box")}}function C(a){function c(a){if(a in e){var d=b.id(e[a]);a=B(function(){return d});a.id=d;return a}if(a in f){var q=f[a];return N(q,function(a,b){var c=a.invoke(b,q);return b.def(a.shared.strings,".id(",c,")")})}return null}var e=a["static"],f=a.dynamic,d=c("frag"),g=c("vert"),ca=null;ta(d)&&ta(g)?(ca=v.program(g.id,d.id),a=B(function(a,b){return a.link(ca)})):a=new Y(d&&d.thisDep||
g&&g.thisDep,d&&d.contextDep||g&&g.contextDep,d&&d.propDep||g&&g.propDep,function(a,b){var c=a.shared.shader,e;e=d?d.append(a,b):b.def(c,".","frag");var f;f=g?g.append(a,b):b.def(c,".","vert");return b.def(c+".program("+f+","+e+")")});return{frag:d,vert:g,progVar:a,program:ca}}function M(a,b){function c(a,b){if(a in e){var q=e[a]|0;return B(function(a,c){b&&(a.OFFSET=q);return q})}if(a in f){var g=f[a];return N(g,function(a,c){var e=a.invoke(c,g);b&&(a.OFFSET=e);return e})}return b&&d?B(function(a,
b){a.OFFSET="0";return 0}):null}var e=a["static"],f=a.dynamic,d=function(){if("elements"in e){var a=e.elements;Na(a)?a=k.getElements(k.create(a,!0)):a&&(a=k.getElements(a));var b=B(function(b,c){if(a){var e=b.link(a);return b.ELEMENTS=e}return b.ELEMENTS=null});b.value=a;return b}if("elements"in f){var c=f.elements;return N(c,function(a,b){var e=a.shared,d=e.isBufferArgs,e=e.elements,f=a.invoke(b,c),q=b.def("null"),d=b.def(d,"(",f,")"),f=a.cond(d).then(q,"=",e,".createStream(",f,");")["else"](q,"=",
e,".getElements(",f,");");b.entry(f);b.exit(a.cond(d).then(e,".destroyStream(",q,");"));return a.ELEMENTS=q})}return null}(),g=c("offset",!0);return{elements:d,primitive:function(){if("primitive"in e){var a=e.primitive;return B(function(b,c){return Ra[a]})}if("primitive"in f){var b=f.primitive;return N(b,function(a,c){var e=a.constants.primTypes,d=a.invoke(c,b);return c.def(e,"[",d,"]")})}return d?ta(d)?d.value?B(function(a,b){return b.def(a.ELEMENTS,".primType")}):B(function(){return 4}):new Y(d.thisDep,
d.contextDep,d.propDep,function(a,b){var c=a.ELEMENTS;return b.def(c,"?",c,".primType:",4)}):null}(),count:function(){if("count"in e){var a=e.count|0;return B(function(){return a})}if("count"in f){var b=f.count;return N(b,function(a,c){return a.invoke(c,b)})}return d?ta(d)?d?g?new Y(g.thisDep,g.contextDep,g.propDep,function(a,b){return b.def(a.ELEMENTS,".vertCount-",a.OFFSET)}):B(function(a,b){return b.def(a.ELEMENTS,".vertCount")}):B(function(){return-1}):new Y(d.thisDep||g.thisDep,d.contextDep||
g.contextDep,d.propDep||g.propDep,function(a,b){var c=a.ELEMENTS;return a.OFFSET?b.def(c,"?",c,".vertCount-",a.OFFSET,":-1"):b.def(c,"?",c,".vertCount:-1")}):null}(),instances:c("instances",!1),offset:g}}function P(a,b){var c=a["static"],e=a.dynamic,d={};Ja.forEach(function(a){function b(q,g){if(a in c){var y=q(c[a]);d[f]=B(function(){return y})}else if(a in e){var l=e[a];d[f]=N(l,function(a,b){return g(a,b,a.invoke(b,l))})}}var f=n(a);switch(a){case "cull.enable":case "blend.enable":case "dither":case "stencil.enable":case "depth.enable":case "scissor.enable":case "polygonOffset.enable":case "sample.alpha":case "sample.enable":case "depth.mask":return b(function(a){return a},
function(a,b,c){return c});case "depth.func":return b(function(a){return Ua[a]},function(a,b,c){return b.def(a.constants.compareFuncs,"[",c,"]")});case "depth.range":return b(function(a){return a},function(a,b,c){a=b.def("+",c,"[0]");b=b.def("+",c,"[1]");return[a,b]});case "blend.func":return b(function(a){return[Fa["srcRGB"in a?a.srcRGB:a.src],Fa["dstRGB"in a?a.dstRGB:a.dst],Fa["srcAlpha"in a?a.srcAlpha:a.src],Fa["dstAlpha"in a?a.dstAlpha:a.dst]]},function(a,b,c){function e(a,d){return b.def('"',
a,d,'" in ',c,"?",c,".",a,d,":",c,".",a)}a=a.constants.blendFuncs;var d=e("src","RGB"),f=e("dst","RGB"),d=b.def(a,"[",d,"]"),q=b.def(a,"[",e("src","Alpha"),"]"),f=b.def(a,"[",f,"]");a=b.def(a,"[",e("dst","Alpha"),"]");return[d,f,q,a]});case "blend.equation":return b(function(a){if("string"===typeof a)return[V[a],V[a]];if("object"===typeof a)return[V[a.rgb],V[a.alpha]]},function(a,b,c){var e=a.constants.blendEquations,d=b.def(),f=b.def();a=a.cond("typeof ",c,'==="string"');a.then(d,"=",f,"=",e,"[",
c,"];");a["else"](d,"=",e,"[",c,".rgb];",f,"=",e,"[",c,".alpha];");b(a);return[d,f]});case "blend.color":return b(function(a){return J(4,function(b){return+a[b]})},function(a,b,c){return J(4,function(a){return b.def("+",c,"[",a,"]")})});case "stencil.mask":return b(function(a){return a|0},function(a,b,c){return b.def(c,"|0")});case "stencil.func":return b(function(a){return[Ua[a.cmp||"keep"],a.ref||0,"mask"in a?a.mask:-1]},function(a,b,c){a=b.def('"cmp" in ',c,"?",a.constants.compareFuncs,"[",c,".cmp]",
":",7680);var e=b.def(c,".ref|0");b=b.def('"mask" in ',c,"?",c,".mask|0:-1");return[a,e,b]});case "stencil.opFront":case "stencil.opBack":return b(function(b){return["stencil.opBack"===a?1029:1028,Oa[b.fail||"keep"],Oa[b.zfail||"keep"],Oa[b.zpass||"keep"]]},function(b,c,e){function d(a){return c.def('"',a,'" in ',e,"?",f,"[",e,".",a,"]:",7680)}var f=b.constants.stencilOps;return["stencil.opBack"===a?1029:1028,d("fail"),d("zfail"),d("zpass")]});case "polygonOffset.offset":return b(function(a){return[a.factor|
0,a.units|0]},function(a,b,c){a=b.def(c,".factor|0");b=b.def(c,".units|0");return[a,b]});case "cull.face":return b(function(a){var b=0;"front"===a?b=1028:"back"===a&&(b=1029);return b},function(a,b,c){return b.def(c,'==="front"?',1028,":",1029)});case "lineWidth":return b(function(a){return a},function(a,b,c){return c});case "frontFace":return b(function(a){return rb[a]},function(a,b,c){return b.def(c+'==="cw"?2304:2305')});case "colorMask":return b(function(a){return a.map(function(a){return!!a})},
function(a,b,c){return J(4,function(a){return"!!"+c+"["+a+"]"})});case "sample.coverage":return b(function(a){return["value"in a?a.value:1,!!a.invert]},function(a,b,c){a=b.def('"value" in ',c,"?+",c,".value:1");b=b.def("!!",c,".invert");return[a,b]})}});return d}function G(a,b){var c=a["static"],e=a.dynamic,d={};Object.keys(c).forEach(function(a){var b=c[a],e;if("number"===typeof b||"boolean"===typeof b)e=B(function(){return b});else if("function"===typeof b){var f=b._reglType;if("texture2d"===f||
"textureCube"===f)e=B(function(a){return a.link(b)});else if("framebuffer"===f||"framebufferCube"===f)e=B(function(a){return a.link(b.color[0])})}else na(b)&&(e=B(function(a){return a.global.def("[",J(b.length,function(a){return b[a]}),"]")}));e.value=b;d[a]=e});Object.keys(e).forEach(function(a){var b=e[a];d[a]=N(b,function(a,c){return a.invoke(c,b)})});return d}function Q(a,c){var e=a["static"],d=a.dynamic,g={};Object.keys(e).forEach(function(a){var c=e[a],d=b.id(a),q=new xa;if(Na(c))q.state=1,
q.buffer=f.getBuffer(f.create(c,34962,!1,!0)),q.type=0;else{var y=f.getBuffer(c);if(y)q.state=1,q.buffer=y,q.type=0;else if(c.constant){var l=c.constant;q.buffer="null";q.state=2;"number"===typeof l?q.x=l:Da.forEach(function(a,b){b<l.length&&(q[a]=l[b])})}else{var y=Na(c.buffer)?f.getBuffer(f.create(c.buffer,34962,!1,!0)):f.getBuffer(c.buffer),h=c.offset|0,k=c.stride|0,m=c.size|0,n=!!c.normalized,aa=0;"type"in c&&(aa=Qa[c.type]);c=c.divisor|0;q.buffer=y;q.state=1;q.size=m;q.normalized=n;q.type=aa||
y.dtype;q.offset=h;q.stride=k;q.divisor=c}}g[a]=B(function(a,b){var c=a.attribCache;if(d in c)return c[d];var e={isStream:!1};Object.keys(q).forEach(function(a){e[a]=q[a]});q.buffer&&(e.buffer=a.link(q.buffer),e.type=e.type||e.buffer+".dtype");return c[d]=e})});Object.keys(d).forEach(function(a){var b=d[a];g[a]=N(b,function(a,c){function e(a){c(y[a],"=",d,".",a,"|0;")}var d=a.invoke(c,b),f=a.shared,q=f.isBufferArgs,g=f.buffer,y={isStream:c.def(!1)},l=new xa;l.state=1;Object.keys(l).forEach(function(a){y[a]=
c.def(""+l[a])});var h=y.buffer,K=y.type;c("if(",q,"(",d,")){",y.isStream,"=true;",h,"=",g,".createStream(",34962,",",d,");",K,"=",h,".dtype;","}else{",h,"=",g,".getBuffer(",d,");","if(",h,"){",K,"=",h,".dtype;",'}else if("constant" in ',d,"){",y.state,"=",2,";","if(typeof "+d+'.constant === "number"){',y[Da[0]],"=",d,".constant;",Da.slice(1).map(function(a){return y[a]}).join("="),"=0;","}else{",Da.map(function(a,b){return y[a]+"="+d+".constant.length>="+b+"?"+d+".constant["+b+"]:0;"}).join(""),
"}}else{","if(",q,"(",d,".buffer)){",h,"=",g,".createStream(",34962,",",d,".buffer);","}else{",h,"=",g,".getBuffer(",d,".buffer);","}",K,'="type" in ',d,"?",f.glTypes,"[",d,".type]:",h,".dtype;",y.normalized,"=!!",d,".normalized;");e("size");e("offset");e("stride");e("divisor");c("}}");c.exit("if(",y.isStream,"){",g,".destroyStream(",h,");","}");return y})});return g}function O(a){var b=a["static"],c=a.dynamic,e={};Object.keys(b).forEach(function(a){var c=b[a];e[a]=B(function(a,b){return"number"===
typeof c||"boolean"===typeof c?""+c:a.link(c)})});Object.keys(c).forEach(function(a){var b=c[a];e[a]=N(b,function(a,c){return a.invoke(c,b)})});return e}function z(a,b,c,e,d){var f=A(a,d),g=x(a,f,d),l=M(a,d),h=P(a,d),k=C(a,d),m=g.viewport;m&&(h.viewport=m);m=n("scissor.box");(g=g[m])&&(h[m]=g);g=0<Object.keys(h).length;f={framebuffer:f,draw:l,shader:k,state:h,dirty:g};f.profile=Z(a,d);f.uniforms=G(c,d);f.attributes=Q(b,d);f.context=O(e,d);return f}function sa(a,b,c){var e=a.shared.context,d=a.scope();
Object.keys(c).forEach(function(f){b.save(e,"."+f);d(e,".",f,"=",c[f].append(a,b),";")});b(d)}function I(a,b,c,e){var d=a.shared,f=d.gl,g=d.framebuffer,l;ka&&(l=b.def(d.extensions,".webgl_draw_buffers"));var h=a.constants,d=h.drawBuffer,h=h.backBuffer;a=c?c.append(a,b):b.def(g,".next");e||b("if(",a,"!==",g,".cur){");b("if(",a,"){",f,".bindFramebuffer(",36160,",",a,".framebuffer);");ka&&b(l,".drawBuffersWEBGL(",d,"[",a,".colorAttachments.length]);");b("}else{",f,".bindFramebuffer(",36160,",null);");
ka&&b(l,".drawBuffersWEBGL(",h,");");b("}",g,".cur=",a,";");e||b("}")}function T(a,b,c){var e=a.shared,d=e.gl,f=a.current,g=a.next,l=e.current,h=e.next,k=a.cond(l,".dirty");Ja.forEach(function(b){b=n(b);if(!(b in c.state)){var e,y;if(b in g){e=g[b];y=f[b];var m=J(pa[b].length,function(a){return k.def(e,"[",a,"]")});k(a.cond(m.map(function(a,b){return a+"!=="+y+"["+b+"]"}).join("||")).then(d,".",ra[b],"(",m,");",m.map(function(a,b){return y+"["+b+"]="+a}).join(";"),";"))}else e=k.def(h,".",b),m=a.cond(e,
"!==",l,".",b),k(m),b in qa?m(a.cond(e).then(d,".enable(",qa[b],");")["else"](d,".disable(",qa[b],");"),l,".",b,"=",e,";"):m(d,".",ra[b],"(",e,");",l,".",b,"=",e,";")}});0===Object.keys(c.state).length&&k(l,".dirty=false;");b(k)}function L(a,b,c,e){var d=a.shared,f=a.current,g=d.current,l=d.gl;qb(Object.keys(c)).forEach(function(d){var h=c[d];if(!e||e(h)){var k=h.append(a,b);if(qa[d]){var m=qa[d];ta(h)?k?b(l,".enable(",m,");"):b(l,".disable(",m,");"):b(a.cond(k).then(l,".enable(",m,");")["else"](l,
".disable(",m,");"));b(g,".",d,"=",k,";")}else if(na(k)){var n=f[d];b(l,".",ra[d],"(",k,");",k.map(function(a,b){return n+"["+b+"]="+a}).join(";"),";")}else b(l,".",ra[d],"(",k,");",g,".",d,"=",k,";")}})}function ua(a,b){ma&&(a.instancing=b.def(a.shared.extensions,".angle_instanced_arrays"))}function E(a,b,c,e,d){function f(){return"undefined"===typeof performance?"Date.now()":"performance.now()"}function g(a){r=b.def();a(r,"=",f(),";");"string"===typeof d?a(n,".count+=",d,";"):a(n,".count++;");m&&
(e?(u=b.def(),a(u,"=",t,".getNumPendingQueries();")):a(t,".beginQuery(",n,");"))}function l(a){a(n,".cpuTime+=",f(),"-",r,";");m&&(e?a(t,".pushScopeStats(",u,",",t,".getNumPendingQueries(),",n,");"):a(t,".endQuery();"))}function h(a){var c=b.def(p,".profile");b(p,".profile=",a,";");b.exit(p,".profile=",c,";")}var k=a.shared,n=a.stats,p=k.current,t=k.timer;c=c.profile;var r,u;if(c){if(ta(c)){c.enable?(g(b),l(b.exit),h("true")):h("false");return}c=c.append(a,b);h(c)}else c=b.def(p,".profile");k=a.block();
g(k);b("if(",c,"){",k,"}");a=a.block();l(a);b.exit("if(",c,"){",a,"}")}function S(a,b,c,e,d){function f(a){switch(a){case 35664:case 35667:case 35671:return 2;case 35665:case 35668:case 35672:return 3;case 35666:case 35669:case 35673:return 4;default:return 1}}function g(c,e,d){function f(){b("if(!",n,".buffer){",k,".enableVertexAttribArray(",m,");}");var c=d.type,g;g=d.size?b.def(d.size,"||",e):e;b("if(",n,".type!==",c,"||",n,".size!==",g,"||",aa.map(function(a){return n+"."+a+"!=="+d[a]}).join("||"),
"){",k,".bindBuffer(",34962,",",K,".buffer);",k,".vertexAttribPointer(",[m,g,c,d.normalized,d.stride,d.offset],");",n,".type=",c,";",n,".size=",g,";",aa.map(function(a){return n+"."+a+"="+d[a]+";"}).join(""),"}");ma&&(c=d.divisor,b("if(",n,".divisor!==",c,"){",a.instancing,".vertexAttribDivisorANGLE(",[m,c],");",n,".divisor=",c,";}"))}function h(){b("if(",n,".buffer){",k,".disableVertexAttribArray(",m,");","}if(",Da.map(function(a,b){return n+"."+a+"!=="+p[b]}).join("||"),"){",k,".vertexAttrib4f(",
m,",",p,");",Da.map(function(a,b){return n+"."+a+"="+p[b]+";"}).join(""),"}")}var k=l.gl,m=b.def(c,".location"),n=b.def(l.attributes,"[",m,"]");c=d.state;var K=d.buffer,p=[d.x,d.y,d.z,d.w],aa=["buffer","normalized","offset","stride"];1===c?f():2===c?h():(b("if(",c,"===",1,"){"),f(),b("}else{"),h(),b("}"))}var l=a.shared;e.forEach(function(e){var l=e.name,h=c.attributes[l],k;if(h){if(!d(h))return;k=h.append(a,b)}else{if(!d(sb))return;var m=a.scopeAttrib(l);k={};Object.keys(new xa).forEach(function(a){k[a]=
b.def(m,".",a)})}g(a.link(e),f(e.info.type),k)})}function U(a,c,e,d,f){for(var g=a.shared,l=g.gl,h,k=0;k<d.length;++k){var m=d[k],n=m.name,p=m.info.type,t=e.uniforms[n],m=a.link(m)+".location",r;if(t){if(!f(t))continue;if(ta(t)){n=t.value;if(35678===p||35680===p)p=a.link(n._texture||n.color[0]._texture),c(l,".uniform1i(",m,",",p+".bind());"),c.exit(p,".unbind();");else if(35674===p||35675===p||35676===p)n=a.global.def("new Float32Array(["+Array.prototype.slice.call(n)+"])"),t=2,35675===p?t=3:35676===
p&&(t=4),c(l,".uniformMatrix",t,"fv(",m,",false,",n,");");else{switch(p){case 5126:h="1f";break;case 35664:h="2f";break;case 35665:h="3f";break;case 35666:h="4f";break;case 35670:h="1i";break;case 5124:h="1i";break;case 35671:h="2i";break;case 35667:h="2i";break;case 35672:h="3i";break;case 35668:h="3i";break;case 35673:h="4i";break;case 35669:h="4i"}c(l,".uniform",h,"(",m,",",na(n)?Array.prototype.slice.call(n):n,");")}continue}else r=t.append(a,c)}else{if(!f(sb))continue;r=c.def(g.uniforms,"[",
b.id(n),"]")}35678===p?c("if(",r,"&&",r,'._reglType==="framebuffer"){',r,"=",r,".color[0];","}"):35680===p&&c("if(",r,"&&",r,'._reglType==="framebufferCube"){',r,"=",r,".color[0];","}");n=1;switch(p){case 35678:case 35680:p=c.def(r,"._texture");c(l,".uniform1i(",m,",",p,".bind());");c.exit(p,".unbind();");continue;case 5124:case 35670:h="1i";break;case 35667:case 35671:h="2i";n=2;break;case 35668:case 35672:h="3i";n=3;break;case 35669:case 35673:h="4i";n=4;break;case 5126:h="1f";break;case 35664:h=
"2f";n=2;break;case 35665:h="3f";n=3;break;case 35666:h="4f";n=4;break;case 35674:h="Matrix2fv";break;case 35675:h="Matrix3fv";break;case 35676:h="Matrix4fv"}c(l,".uniform",h,"(",m,",");if("M"===h.charAt(0)){var m=Math.pow(p-35674+2,2),u=a.global.def("new Float32Array(",m,")");c("false,(Array.isArray(",r,")||",r," instanceof Float32Array)?",r,":(",J(m,function(a){return u+"["+a+"]="+r+"["+a+"]"}),",",u,")")}else 1<n?c(J(n,function(a){return r+"["+a+"]"})):c(r);c(");")}}function H(a,b,c,e){function d(f){var g=
m[f];return g?g.contextDep&&e.contextDynamic||g.propDep?g.append(a,c):g.append(a,b):b.def(k,".",f)}function f(){function a(){c(w,".drawElementsInstancedANGLE(",[p,t,W,r+"<<(("+W+"-5121)>>1)",u],");")}function b(){c(w,".drawArraysInstancedANGLE(",[p,r,t,u],");")}n?v?a():(c("if(",n,"){"),a(),c("}else{"),b(),c("}")):b()}function g(){function a(){c(l+".drawElements("+[p,t,W,r+"<<(("+W+"-5121)>>1)"]+");")}function b(){c(l+".drawArrays("+[p,r,t]+");")}n?v?a():(c("if(",n,"){"),a(),c("}else{"),b(),c("}")):
b()}var h=a.shared,l=h.gl,k=h.draw,m=e.draw,n=function(){var d=m.elements,f=b;if(d){if(d.contextDep&&e.contextDynamic||d.propDep)f=c;d=d.append(a,f)}else d=f.def(k,".","elements");d&&f("if("+d+")"+l+".bindBuffer(34963,"+d+".buffer.buffer);");return d}(),p=d("primitive"),r=d("offset"),t=function(){var d=m.count,f=b;if(d){if(d.contextDep&&e.contextDynamic||d.propDep)f=c;d=d.append(a,f)}else d=f.def(k,".","count");return d}();if("number"===typeof t){if(0===t)return}else c("if(",t,"){"),c.exit("}");var u,
w;ma&&(u=d("instances"),w=a.instancing);var W=n+".type",v=m.elements&&ta(m.elements);ma&&("number"!==typeof u||0<=u)?"string"===typeof u?(c("if(",u,">0){"),f(),c("}else if(",u,"<0){"),g(),c("}")):f():g()}function ba(a,b,c,e,d){b=t();d=b.proc("body",d);ma&&(b.instancing=d.def(b.shared.extensions,".angle_instanced_arrays"));a(b,d,c,e);return b.compile().body}function R(a,b,c,e){ua(a,b);S(a,b,c,e.attributes,function(){return!0});U(a,b,c,e.uniforms,function(){return!0});H(a,b,b,c)}function W(a,b){var c=
a.proc("draw",1);ua(a,c);sa(a,c,b.context);I(a,c,b.framebuffer);T(a,c,b);L(a,c,b.state);E(a,c,b,!1,!0);var e=b.shader.progVar.append(a,c);c(a.shared.gl,".useProgram(",e,".program);");if(b.shader.program)R(a,c,b,b.shader.program);else{var d=a.global.def("{}"),f=c.def(e,".id"),g=c.def(d,"[",f,"]");c(a.cond(g).then(g,".call(this,a0);")["else"](g,"=",d,"[",f,"]=",a.link(function(c){return ba(R,a,b,c,1)}),"(",e,");",g,".call(this,a0);"))}0<Object.keys(b.state).length&&c(a.shared.current,".dirty=true;")}
function la(a,b,c,e){function d(){return!0}a.batchId="a1";ua(a,b);S(a,b,c,e.attributes,d);U(a,b,c,e.uniforms,d);H(a,b,b,c)}function ya(a,b,c,e){function d(a){return a.contextDep&&g||a.propDep}function f(a){return!d(a)}ua(a,b);var g=c.contextDep,h=b.def(),l=b.def();a.shared.props=l;a.batchId=h;var k=a.scope(),m=a.scope();b(k.entry,"for(",h,"=0;",h,"<","a1",";++",h,"){",l,"=","a0","[",h,"];",m,"}",k.exit);c.needsContext&&sa(a,m,c.context);c.needsFramebuffer&&I(a,m,c.framebuffer);L(a,m,c.state,d);c.profile&&
d(c.profile)&&E(a,m,c,!1,!0);e?(S(a,k,c,e.attributes,f),S(a,m,c,e.attributes,d),U(a,k,c,e.uniforms,f),U(a,m,c,e.uniforms,d),H(a,k,m,c)):(b=a.global.def("{}"),e=c.shader.progVar.append(a,m),l=m.def(e,".id"),k=m.def(b,"[",l,"]"),m(a.shared.gl,".useProgram(",e,".program);","if(!",k,"){",k,"=",b,"[",l,"]=",a.link(function(b){return ba(la,a,c,b,2)}),"(",e,");}",k,".call(this,a0[",h,"],",h,");"))}function ha(a,b){function c(a){return a.contextDep&&d||a.propDep}var e=a.proc("batch",2);a.batchId="0";ua(a,
e);var d=!1,f=!0;Object.keys(b.context).forEach(function(a){d=d||b.context[a].propDep});d||(sa(a,e,b.context),f=!1);var g=b.framebuffer,h=!1;g?(g.propDep?d=h=!0:g.contextDep&&d&&(h=!0),h||I(a,e,g)):I(a,e,null);b.state.viewport&&b.state.viewport.propDep&&(d=!0);T(a,e,b);L(a,e,b.state,function(a){return!c(a)});b.profile&&c(b.profile)||E(a,e,b,!1,"a1");b.contextDep=d;b.needsContext=f;b.needsFramebuffer=h;f=b.shader.progVar;if(f.contextDep&&d||f.propDep)ya(a,e,b,null);else if(f=f.append(a,e),e(a.shared.gl,
".useProgram(",f,".program);"),b.shader.program)ya(a,e,b,b.shader.program);else{var g=a.global.def("{}"),h=e.def(f,".id"),l=e.def(g,"[",h,"]");e(a.cond(l).then(l,".call(this,a0,a1);")["else"](l,"=",g,"[",h,"]=",a.link(function(c){return ba(ya,a,b,c,2)}),"(",f,");",l,".call(this,a0,a1);"))}0<Object.keys(b.state).length&&e(a.shared.current,".dirty=true;")}function ea(a,c){function e(b){var g=c.shader[b];g&&d.set(f.shader,"."+b,g.append(a,d))}var d=a.proc("scope",3);a.batchId="a2";var f=a.shared,g=f.current;
sa(a,d,c.context);c.framebuffer&&c.framebuffer.append(a,d);qb(Object.keys(c.state)).forEach(function(b){var e=c.state[b].append(a,d);na(e)?e.forEach(function(c,e){d.set(a.next[b],"["+e+"]",c)}):d.set(f.next,"."+b,e)});E(a,d,c,!0,!0);["elements","offset","count","instances","primitive"].forEach(function(b){var e=c.draw[b];e&&d.set(f.draw,"."+b,""+e.append(a,d))});Object.keys(c.uniforms).forEach(function(e){d.set(f.uniforms,"["+b.id(e)+"]",c.uniforms[e].append(a,d))});Object.keys(c.attributes).forEach(function(b){var e=
c.attributes[b].append(a,d),f=a.scopeAttrib(b);Object.keys(new xa).forEach(function(a){d.set(f,"."+a,e[a])})});e("vert");e("frag");0<Object.keys(c.state).length&&(d(g,".dirty=true;"),d.exit(g,".dirty=true;"));d("a1(",a.shared.context,",a0,",a.batchId,");")}function ja(a){if("object"===typeof a&&!na(a)){for(var b=Object.keys(a),c=0;c<b.length;++c)if(ia.isDynamic(a[b[c]]))return!0;return!1}}function fa(a,b,c){function e(a,b){g.forEach(function(c){var e=d[c];ia.isDynamic(e)&&(e=a.invoke(b,e),b(k,".",
c,"=",e,";"))})}var d=b["static"][c];if(d&&ja(d)){var f=a.global,g=Object.keys(d),h=!1,l=!1,m=!1,k=a.global.def("{}");g.forEach(function(b){var c=d[b];if(ia.isDynamic(c))"function"===typeof c&&(c=d[b]=ia.unbox(c)),b=N(c,null),h=h||b.thisDep,m=m||b.propDep,l=l||b.contextDep;else{f(k,".",b,"=");switch(typeof c){case "number":f(c);break;case "string":f('"',c,'"');break;case "object":Array.isArray(c)&&f("[",c.join(),"]");break;default:f(a.link(c))}f(";")}});b.dynamic[c]=new ia.DynamicVariable(4,{thisDep:h,
contextDep:l,propDep:m,ref:k,append:e});delete b["static"][c]}}var xa=r.Record,V={add:32774,subtract:32778,"reverse subtract":32779};c.ext_blend_minmax&&(V.min=32775,V.max=32776);var ma=c.angle_instanced_arrays,ka=c.webgl_draw_buffers,pa={dirty:!0,profile:e.profile},Ea={},Ja=[],qa={},ra={};D("dither",3024);D("blend.enable",3042);w("blend.color","blendColor",[0,0,0,0]);w("blend.equation","blendEquationSeparate",[32774,32774]);w("blend.func","blendFuncSeparate",[1,0,1,0]);D("depth.enable",2929,!0);
w("depth.func","depthFunc",513);w("depth.range","depthRange",[0,1]);w("depth.mask","depthMask",!0);w("colorMask","colorMask",[!0,!0,!0,!0]);D("cull.enable",2884);w("cull.face","cullFace",1029);w("frontFace","frontFace",2305);w("lineWidth","lineWidth",1);D("polygonOffset.enable",32823);w("polygonOffset.offset","polygonOffset",[0,0]);D("sample.alpha",32926);D("sample.enable",32928);w("sample.coverage","sampleCoverage",[1,!1]);D("stencil.enable",2960);w("stencil.mask","stencilMask",-1);w("stencil.func",
"stencilFunc",[519,0,-1]);w("stencil.opFront","stencilOpSeparate",[1028,7680,7680,7680]);w("stencil.opBack","stencilOpSeparate",[1029,7680,7680,7680]);D("scissor.enable",3089);w("scissor.box","scissor",[0,0,a.drawingBufferWidth,a.drawingBufferHeight]);w("viewport","viewport",[0,0,a.drawingBufferWidth,a.drawingBufferHeight]);var ga={gl:a,context:u,strings:b,next:Ea,current:pa,draw:l,elements:k,buffer:f,shader:v,attributes:r.state,uniforms:g,framebuffer:h,extensions:c,timer:m,isBufferArgs:Na},da={primTypes:Ra,
compareFuncs:Ua,blendFuncs:Fa,blendEquations:V,stencilOps:Oa,glTypes:Qa,orientationType:rb};ka&&(da.backBuffer=[1029],da.drawBuffer=J(d.maxDrawbuffers,function(a){return 0===a?[0]:J(a,function(a){return 36064+a})}));var oa=0;return{next:Ea,current:pa,procs:function(){var b=t(),c=b.proc("poll"),e=b.proc("refresh"),f=b.block();c(f);e(f);var g=b.shared,h=g.gl,l=g.next,m=g.current;f(m,".dirty=false;");I(b,c);I(b,e,null,!0);var k=a.getExtension("angle_instanced_arrays"),n;k&&(n=b.link(k));for(var p=0;p<
d.maxAttributes;++p){var r=e.def(g.attributes,"[",p,"]"),u=b.cond(r,".buffer");u.then(h,".enableVertexAttribArray(",p,");",h,".bindBuffer(",34962,",",r,".buffer.buffer);",h,".vertexAttribPointer(",p,",",r,".size,",r,".type,",r,".normalized,",r,".stride,",r,".offset);")["else"](h,".disableVertexAttribArray(",p,");",h,".vertexAttrib4f(",p,",",r,".x,",r,".y,",r,".z,",r,".w);",r,".buffer=null;");e(u);k&&e(n,".vertexAttribDivisorANGLE(",p,",",r,".divisor);")}Object.keys(qa).forEach(function(a){var d=qa[a],
g=f.def(l,".",a),k=b.block();k("if(",g,"){",h,".enable(",d,")}else{",h,".disable(",d,")}",m,".",a,"=",g,";");e(k);c("if(",g,"!==",m,".",a,"){",k,"}")});Object.keys(ra).forEach(function(a){var d=ra[a],g=pa[a],k,n,p=b.block();p(h,".",d,"(");na(g)?(d=g.length,k=b.global.def(l,".",a),n=b.global.def(m,".",a),p(J(d,function(a){return k+"["+a+"]"}),");",J(d,function(a){return n+"["+a+"]="+k+"["+a+"];"}).join("")),c("if(",J(d,function(a){return k+"["+a+"]!=="+n+"["+a+"]"}).join("||"),"){",p,"}")):(k=f.def(l,
".",a),n=f.def(m,".",a),p(k,");",m,".",a,"=",k,";"),c("if(",k,"!==",n,"){",p,"}"));e(p)});return b.compile()}(),compile:function(a,b,c,e,d){var f=t();f.stats=f.link(d);Object.keys(b["static"]).forEach(function(a){fa(f,b,a)});Pb.forEach(function(b){fa(f,a,b)});c=z(a,b,c,e,f);W(f,c);ea(f,c);ha(f,c);return f.compile()}}}function tb(a,b){for(var c=0;c<a.length;++c)if(a[c]===b)return c;return-1}var A=function(a,b){for(var c=Object.keys(b),d=0;d<c.length;++d)a[c[d]]=b[c[d]];return a},vb=0,ia={DynamicVariable:da,
define:function(a,b){return new da(a,Wa(b+""))},isDynamic:function(a){return"function"===typeof a&&!a._reglType||a instanceof da},unbox:function(a,b){return"function"===typeof a?new da(0,a):a},accessor:Wa},Va={next:"function"===typeof requestAnimationFrame?function(a){return requestAnimationFrame(a)}:function(a){return setTimeout(a,16)},cancel:"function"===typeof cancelAnimationFrame?function(a){return cancelAnimationFrame(a)}:clearTimeout},ub="undefined"!==typeof performance&&performance.now?function(){return performance.now()}:
function(){return+new Date},Qb=function(a,b){var c=1;b.ext_texture_filter_anisotropic&&(c=a.getParameter(34047));var d=1,f=1;b.webgl_draw_buffers&&(d=a.getParameter(34852),f=a.getParameter(36063));return{colorBits:[a.getParameter(3410),a.getParameter(3411),a.getParameter(3412),a.getParameter(3413)],depthBits:a.getParameter(3414),stencilBits:a.getParameter(3415),subpixelBits:a.getParameter(3408),extensions:Object.keys(b).filter(function(a){return!!b[a]}),maxAnisotropic:c,maxDrawbuffers:d,maxColorAttachments:f,
pointSizeDims:a.getParameter(33901),lineWidthDims:a.getParameter(33902),maxViewportDims:a.getParameter(3386),maxCombinedTextureUnits:a.getParameter(35661),maxCubeMapSize:a.getParameter(34076),maxRenderbufferSize:a.getParameter(34024),maxTextureUnits:a.getParameter(34930),maxTextureSize:a.getParameter(3379),maxAttributes:a.getParameter(34921),maxVertexUniforms:a.getParameter(36347),maxVertexTextureUnits:a.getParameter(35660),maxVaryingVectors:a.getParameter(36348),maxFragmentUniforms:a.getParameter(36349),
glsl:a.getParameter(35724),renderer:a.getParameter(7937),vendor:a.getParameter(7936),version:a.getParameter(7938)}},O=function(a){return a instanceof Uint8Array||a instanceof Uint16Array||a instanceof Uint32Array||a instanceof Int8Array||a instanceof Int16Array||a instanceof Int32Array||a instanceof Float32Array||a instanceof Float64Array||a instanceof Uint8ClampedArray},H=function(a){return Object.keys(a).map(function(b){return a[b]})},$a=J(8,function(){return[]}),x={alloc:fa,free:ab,allocType:function(a,
b){var c=null;switch(a){case 5120:c=new Int8Array(fa(b),0,b);break;case 5121:c=new Uint8Array(fa(b),0,b);break;case 5122:c=new Int16Array(fa(2*b),0,b);break;case 5123:c=new Uint16Array(fa(2*b),0,b);break;case 5124:c=new Int32Array(fa(4*b),0,b);break;case 5125:c=new Uint32Array(fa(4*b),0,b);break;case 5126:c=new Float32Array(fa(4*b),0,b);break;default:return null}return c.length!==b?c.subarray(0,b):c},freeType:function(a){ab(a.buffer)}},La={shape:function(a){for(var b=[];a.length;a=a[0])b.push(a.length);
return b},flatten:function(a,b,c,d){var f=1;if(b.length)for(var k=0;k<b.length;++k)f*=b[k];else f=0;c=d||x.allocType(c,f);switch(b.length){case 0:break;case 1:d=b[0];for(b=0;b<d;++b)c[b]=a[b];break;case 2:d=b[0];b=b[1];for(k=f=0;k<d;++k)for(var p=a[k],h=0;h<b;++h)c[f++]=p[h];break;case 3:bb(a,b[0],b[1],b[2],c,0);break;default:cb(a,b,0,c,0)}return c}},Ha={"[object Int8Array]":5120,"[object Int16Array]":5122,"[object Int32Array]":5124,"[object Uint8Array]":5121,"[object Uint8ClampedArray]":5121,"[object Uint16Array]":5123,
"[object Uint32Array]":5125,"[object Float32Array]":5126,"[object Float64Array]":5121,"[object ArrayBuffer]":5121},Qa={int8:5120,int16:5122,int32:5124,uint8:5121,uint16:5123,uint32:5125,"float":5126,float32:5126},hb={dynamic:35048,stream:35040,"static":35044},Pa=La.flatten,fb=La.shape,ga=[];ga[5120]=1;ga[5122]=2;ga[5124]=4;ga[5121]=1;ga[5123]=2;ga[5125]=4;ga[5126]=4;var Ra={points:0,point:0,lines:1,line:1,triangles:4,triangle:4,"line loop":2,"line strip":3,"triangle strip":5,"triangle fan":6},jb=
new Float32Array(1),Db=new Uint32Array(jb.buffer),Hb=[9984,9986,9985,9987],Ka=[0,6409,6410,6407,6408],R={};R[6409]=R[6406]=R[6402]=1;R[34041]=R[6410]=2;R[6407]=R[35904]=3;R[6408]=R[35906]=4;var Eb=Object.keys(Ha).concat(["[object HTMLCanvasElement]","[object CanvasRenderingContext2D]","[object HTMLImageElement]","[object HTMLVideoElement]"]),za=[];za[5121]=1;za[5126]=4;za[36193]=2;za[5123]=2;za[5125]=4;var C=[];C[32854]=2;C[32855]=2;C[36194]=2;C[34041]=4;C[33776]=.5;C[33777]=.5;C[33778]=1;C[33779]=
1;C[35986]=.5;C[35987]=1;C[34798]=1;C[35840]=.5;C[35841]=.25;C[35842]=.5;C[35843]=.25;C[36196]=.5;var L=[];L[32854]=2;L[32855]=2;L[36194]=2;L[33189]=2;L[36168]=1;L[34041]=4;L[35907]=4;L[34836]=16;L[34842]=8;L[34843]=6;var Rb=function(a,b,c,d,f){function k(a){this.id=r++;this.refCount=1;this.renderbuffer=a;this.format=32854;this.height=this.width=0;f.profile&&(this.stats={size:0})}function p(b){var c=b.renderbuffer;a.bindRenderbuffer(36161,null);a.deleteRenderbuffer(c);b.renderbuffer=null;b.refCount=
0;delete v[b.id];d.renderbufferCount--}var h={rgba4:32854,rgb565:36194,"rgb5 a1":32855,depth:33189,stencil:36168,"depth stencil":34041};b.ext_srgb&&(h.srgba=35907);b.ext_color_buffer_half_float&&(h.rgba16f=34842,h.rgb16f=34843);b.webgl_color_buffer_float&&(h.rgba32f=34836);var g=[];Object.keys(h).forEach(function(a){g[h[a]]=a});var r=0,v={};k.prototype.decRef=function(){0>=--this.refCount&&p(this)};f.profile&&(d.getTotalRenderbufferSize=function(){var a=0;Object.keys(v).forEach(function(b){a+=v[b].stats.size});
return a});return{create:function(b,c){function m(b,c){var d=0,l=0,k=32854;"object"===typeof b&&b?("shape"in b?(l=b.shape,d=l[0]|0,l=l[1]|0):("radius"in b&&(d=l=b.radius|0),"width"in b&&(d=b.width|0),"height"in b&&(l=b.height|0)),"format"in b&&(k=h[b.format])):"number"===typeof b?(d=b|0,l="number"===typeof c?c|0:d):b||(d=l=1);if(d!==e.width||l!==e.height||k!==e.format)return m.width=e.width=d,m.height=e.height=l,e.format=k,a.bindRenderbuffer(36161,e.renderbuffer),a.renderbufferStorage(36161,k,d,l),
f.profile&&(e.stats.size=L[e.format]*e.width*e.height),m.format=g[e.format],m}var e=new k(a.createRenderbuffer());v[e.id]=e;d.renderbufferCount++;m(b,c);m.resize=function(b,c){var d=b|0,g=c|0||d;if(d===e.width&&g===e.height)return m;m.width=e.width=d;m.height=e.height=g;a.bindRenderbuffer(36161,e.renderbuffer);a.renderbufferStorage(36161,e.format,d,g);f.profile&&(e.stats.size=L[e.format]*e.width*e.height);return m};m._reglType="renderbuffer";m._renderbuffer=e;f.profile&&(m.stats=e.stats);m.destroy=
function(){e.decRef()};return m},clear:function(){H(v).forEach(p)},restore:function(){H(v).forEach(function(b){b.renderbuffer=a.createRenderbuffer();a.bindRenderbuffer(36161,b.renderbuffer);a.renderbufferStorage(36161,b.format,b.width,b.height)});a.bindRenderbuffer(36161,null)}}},ob=[];ob[6408]=4;var Ma=[];Ma[5121]=1;Ma[5126]=4;Ma[36193]=2;var Da=["x","y","z","w"],Pb="blend.func blend.equation stencil.func stencil.opFront stencil.opBack sample.coverage viewport scissor.box polygonOffset.offset".split(" "),
Fa={0:0,1:1,zero:0,one:1,"src color":768,"one minus src color":769,"src alpha":770,"one minus src alpha":771,"dst color":774,"one minus dst color":775,"dst alpha":772,"one minus dst alpha":773,"constant color":32769,"one minus constant color":32770,"constant alpha":32771,"one minus constant alpha":32772,"src alpha saturate":776},Ua={never:512,less:513,"<":513,equal:514,"=":514,"==":514,"===":514,lequal:515,"<=":515,greater:516,">":516,notequal:517,"!=":517,"!==":517,gequal:518,">=":518,always:519},
Oa={0:0,zero:0,keep:7680,replace:7681,increment:7682,decrement:7683,"increment wrap":34055,"decrement wrap":34056,invert:5386},rb={cw:2304,ccw:2305},sb=new Y(!1,!1,!1,function(){}),Sb=function(a,b){function c(){this.endQueryIndex=this.startQueryIndex=-1;this.sum=0;this.stats=null}function d(a,b,d){var e=h.pop()||new c;e.startQueryIndex=a;e.endQueryIndex=b;e.sum=0;e.stats=d;g.push(e)}var f=b.ext_disjoint_timer_query;if(!f)return null;var k=[],p=[],h=[],g=[],r=[],v=[];return{beginQuery:function(a){var b=
k.pop()||f.createQueryEXT();f.beginQueryEXT(35007,b);p.push(b);d(p.length-1,p.length,a)},endQuery:function(){f.endQueryEXT(35007)},pushScopeStats:d,update:function(){var a,b;a=p.length;if(0!==a){v.length=Math.max(v.length,a+1);r.length=Math.max(r.length,a+1);r[0]=0;var c=v[0]=0;for(b=a=0;b<p.length;++b){var e=p[b];f.getQueryObjectEXT(e,34919)?(c+=f.getQueryObjectEXT(e,34918),k.push(e)):p[a++]=e;r[b+1]=c;v[b+1]=a}p.length=a;for(b=a=0;b<g.length;++b){var c=g[b],d=c.startQueryIndex,e=c.endQueryIndex;
c.sum+=r[e]-r[d];d=v[d];e=v[e];e===d?(c.stats.gpuTime+=c.sum/1E6,h.push(c)):(c.startQueryIndex=d,c.endQueryIndex=e,g[a++]=c)}g.length=a}},getNumPendingQueries:function(){return p.length},clear:function(){k.push.apply(k,p);for(var a=0;a<k.length;a++)f.deleteQueryEXT(k[a]);p.length=0;k.length=0},restore:function(){p.length=0;k.length=0}}};return function(a){function b(){if(0===E.length)x&&x.update(),ba=null;else{ba=Va.next(b);v();for(var a=E.length-1;0<=a;--a){var c=E[a];c&&c(M,null,0)}m.flush();x&&
x.update()}}function c(){!ba&&0<E.length&&(ba=Va.next(b))}function d(){ba&&(Va.cancel(b),ba=null)}function f(a){a.preventDefault();d();S.forEach(function(a){a()})}function k(a){m.getError();n.restore();N.restore();G.restore();z.restore();L.restore();I.restore();x&&x.restore();T.procs.refresh();c();U.forEach(function(a){a()})}function p(a){function b(a){var c={},d={};Object.keys(a).forEach(function(b){var e=a[b];ia.isDynamic(e)?d[b]=ia.unbox(e,b):c[b]=e});return{dynamic:d,"static":c}}function c(a){for(;m.length<
a;)m.push(null);return m}var d=b(a.context||{}),e=b(a.uniforms||{}),f=b(a.attributes||{}),g=b(function(a){function b(a){if(a in c){var d=c[a];delete c[a];Object.keys(d).forEach(function(b){c[a+"."+b]=d[b]})}}var c=A({},a);delete c.uniforms;delete c.attributes;delete c.context;"stencil"in c&&c.stencil.op&&(c.stencil.opBack=c.stencil.opFront=c.stencil.op,delete c.stencil.op);b("blend");b("depth");b("cull");b("stencil");b("polygonOffset");b("scissor");b("sample");return c}(a));a={gpuTime:0,cpuTime:0,
count:0};var d=T.compile(g,f,e,d,a),h=d.draw,k=d.batch,l=d.scope,m=[];return A(function(a,b){var d;if("function"===typeof a)return l.call(this,null,a,0);if("function"===typeof b)if("number"===typeof a)for(d=0;d<a;++d)l.call(this,null,b,d);else if(Array.isArray(a))for(d=0;d<a.length;++d)l.call(this,a[d],b,d);else return l.call(this,a,b,0);else if("number"===typeof a){if(0<a)return k.call(this,c(a|0),a|0)}else if(Array.isArray(a)){if(a.length)return k.call(this,a,a.length)}else return h.call(this,a)},
{stats:a})}function h(a,b){var c=0;T.procs.poll();var d=b.color;d&&(m.clearColor(+d[0]||0,+d[1]||0,+d[2]||0,+d[3]||0),c|=16384);"depth"in b&&(m.clearDepth(+b.depth),c|=256);"stencil"in b&&(m.clearStencil(b.stencil|0),c|=1024);m.clear(c)}function g(a){E.push(a);c();return{cancel:function(){function b(){var a=tb(E,b);E[a]=E[E.length-1];--E.length;0>=E.length&&d()}var c=tb(E,a);E[c]=b}}}function r(){var a=O.viewport,b=O.scissor_box;a[0]=a[1]=b[0]=b[1]=0;M.viewportWidth=M.framebufferWidth=M.drawingBufferWidth=
a[2]=b[2]=m.drawingBufferWidth;M.viewportHeight=M.framebufferHeight=M.drawingBufferHeight=a[3]=b[3]=m.drawingBufferHeight}function v(){M.tick+=1;M.time=u();r();T.procs.poll()}function l(){r();T.procs.refresh();x&&x.update()}function u(){return(ub()-C)/1E3}a=zb(a);if(!a)return null;var m=a.gl,e=m.getContextAttributes();m.isContextLost();var n=Ab(m,a);if(!n)return null;var D=wb(),w={bufferCount:0,elementsCount:0,framebufferCount:0,shaderCount:0,textureCount:0,cubeCount:0,renderbufferCount:0,maxTextureUnits:0},
t=n.extensions,x=Sb(m,t),C=ub(),B=m.drawingBufferWidth,J=m.drawingBufferHeight,M={tick:0,time:0,viewportWidth:B,viewportHeight:J,framebufferWidth:B,framebufferHeight:J,drawingBufferWidth:B,drawingBufferHeight:J,pixelRatio:a.pixelRatio},P=Qb(m,t),G=Bb(m,w,a),Q=Cb(m,t,G,w),B=Kb(m,t,P,G,D),N=Lb(m,D,w,a),z=Fb(m,t,P,function(){T.procs.poll()},M,w,a),L=Rb(m,t,P,w,a),I=Jb(m,t,P,z,L,w),T=Ob(m,D,t,P,G,Q,z,I,{},B,N,{elements:null,primitive:4,count:-1,offset:0,instances:-1},M,x,a),D=Mb(m,I,T.procs.poll,M,e,
t),O=T.next,H=m.canvas,E=[],S=[],U=[],R=[a.onDestroy],ba=null;H&&(H.addEventListener("webglcontextlost",f,!1),H.addEventListener("webglcontextrestored",k,!1));var Y=I.setFBO=p({framebuffer:ia.define.call(null,1,"framebuffer")});l();e=A(p,{clear:function(a){if("framebuffer"in a)if(a.framebuffer&&"framebufferCube"===a.framebuffer_reglType)for(var b=0;6>b;++b)Y(A({framebuffer:a.framebuffer.faces[b]},a),h);else Y(a,h);else h(null,a)},prop:ia.define.bind(null,1),context:ia.define.bind(null,2),"this":ia.define.bind(null,
3),draw:p({}),buffer:function(a){return G.create(a,34962,!1,!1)},elements:function(a){return Q.create(a,!1)},texture:z.create2D,cube:z.createCube,renderbuffer:L.create,framebuffer:I.create,framebufferCube:I.createCube,attributes:e,frame:g,on:function(a,b){var c;switch(a){case "frame":return g(b);case "lost":c=S;break;case "restore":c=U;break;case "destroy":c=R}c.push(b);return{cancel:function(){for(var a=0;a<c.length;++a)if(c[a]===b){c[a]=c[c.length-1];c.pop();break}}}},limits:P,hasExtension:function(a){return 0<=
P.extensions.indexOf(a.toLowerCase())},read:D,destroy:function(){E.length=0;d();H&&(H.removeEventListener("webglcontextlost",f),H.removeEventListener("webglcontextrestored",k));N.clear();I.clear();L.clear();z.clear();Q.clear();G.clear();x&&x.clear();R.forEach(function(a){a()})},_gl:m,_refresh:l,poll:function(){v();x&&x.update()},now:u,stats:w});a.onDone(null,e);return e}});

},{}],18:[function(require,module,exports){
 /* eslint-env node */
'use strict';

// SDP helpers.
var SDPUtils = {};

// Generate an alphanumeric identifier for cname or mids.
// TODO: use UUIDs instead? https://gist.github.com/jed/982883
SDPUtils.generateIdentifier = function() {
  return Math.random().toString(36).substr(2, 10);
};

// The RTCP CNAME used by all peerconnections from the same JS.
SDPUtils.localCName = SDPUtils.generateIdentifier();

// Splits SDP into lines, dealing with both CRLF and LF.
SDPUtils.splitLines = function(blob) {
  return blob.trim().split('\n').map(function(line) {
    return line.trim();
  });
};
// Splits SDP into sessionpart and mediasections. Ensures CRLF.
SDPUtils.splitSections = function(blob) {
  var parts = blob.split('\nm=');
  return parts.map(function(part, index) {
    return (index > 0 ? 'm=' + part : part).trim() + '\r\n';
  });
};

// Returns lines that start with a certain prefix.
SDPUtils.matchPrefix = function(blob, prefix) {
  return SDPUtils.splitLines(blob).filter(function(line) {
    return line.indexOf(prefix) === 0;
  });
};

// Parses an ICE candidate line. Sample input:
// candidate:702786350 2 udp 41819902 8.8.8.8 60769 typ relay raddr 8.8.8.8
// rport 55996"
SDPUtils.parseCandidate = function(line) {
  var parts;
  // Parse both variants.
  if (line.indexOf('a=candidate:') === 0) {
    parts = line.substring(12).split(' ');
  } else {
    parts = line.substring(10).split(' ');
  }

  var candidate = {
    foundation: parts[0],
    component: parts[1],
    protocol: parts[2].toLowerCase(),
    priority: parseInt(parts[3], 10),
    ip: parts[4],
    port: parseInt(parts[5], 10),
    // skip parts[6] == 'typ'
    type: parts[7]
  };

  for (var i = 8; i < parts.length; i += 2) {
    switch (parts[i]) {
      case 'raddr':
        candidate.relatedAddress = parts[i + 1];
        break;
      case 'rport':
        candidate.relatedPort = parseInt(parts[i + 1], 10);
        break;
      case 'tcptype':
        candidate.tcpType = parts[i + 1];
        break;
      default: // extension handling, in particular ufrag
        candidate[parts[i]] = parts[i + 1];
        break;
    }
  }
  return candidate;
};

// Translates a candidate object into SDP candidate attribute.
SDPUtils.writeCandidate = function(candidate) {
  var sdp = [];
  sdp.push(candidate.foundation);
  sdp.push(candidate.component);
  sdp.push(candidate.protocol.toUpperCase());
  sdp.push(candidate.priority);
  sdp.push(candidate.ip);
  sdp.push(candidate.port);

  var type = candidate.type;
  sdp.push('typ');
  sdp.push(type);
  if (type !== 'host' && candidate.relatedAddress &&
      candidate.relatedPort) {
    sdp.push('raddr');
    sdp.push(candidate.relatedAddress); // was: relAddr
    sdp.push('rport');
    sdp.push(candidate.relatedPort); // was: relPort
  }
  if (candidate.tcpType && candidate.protocol.toLowerCase() === 'tcp') {
    sdp.push('tcptype');
    sdp.push(candidate.tcpType);
  }
  return 'candidate:' + sdp.join(' ');
};

// Parses an ice-options line, returns an array of option tags.
// a=ice-options:foo bar
SDPUtils.parseIceOptions = function(line) {
  return line.substr(14).split(' ');
}

// Parses an rtpmap line, returns RTCRtpCoddecParameters. Sample input:
// a=rtpmap:111 opus/48000/2
SDPUtils.parseRtpMap = function(line) {
  var parts = line.substr(9).split(' ');
  var parsed = {
    payloadType: parseInt(parts.shift(), 10) // was: id
  };

  parts = parts[0].split('/');

  parsed.name = parts[0];
  parsed.clockRate = parseInt(parts[1], 10); // was: clockrate
  // was: channels
  parsed.numChannels = parts.length === 3 ? parseInt(parts[2], 10) : 1;
  return parsed;
};

// Generate an a=rtpmap line from RTCRtpCodecCapability or
// RTCRtpCodecParameters.
SDPUtils.writeRtpMap = function(codec) {
  var pt = codec.payloadType;
  if (codec.preferredPayloadType !== undefined) {
    pt = codec.preferredPayloadType;
  }
  return 'a=rtpmap:' + pt + ' ' + codec.name + '/' + codec.clockRate +
      (codec.numChannels !== 1 ? '/' + codec.numChannels : '') + '\r\n';
};

// Parses an a=extmap line (headerextension from RFC 5285). Sample input:
// a=extmap:2 urn:ietf:params:rtp-hdrext:toffset
// a=extmap:2/sendonly urn:ietf:params:rtp-hdrext:toffset
SDPUtils.parseExtmap = function(line) {
  var parts = line.substr(9).split(' ');
  return {
    id: parseInt(parts[0], 10),
    direction: parts[0].indexOf('/') > 0 ? parts[0].split('/')[1] : 'sendrecv',
    uri: parts[1]
  };
};

// Generates a=extmap line from RTCRtpHeaderExtensionParameters or
// RTCRtpHeaderExtension.
SDPUtils.writeExtmap = function(headerExtension) {
  return 'a=extmap:' + (headerExtension.id || headerExtension.preferredId) +
      (headerExtension.direction && headerExtension.direction !== 'sendrecv'
          ? '/' + headerExtension.direction
          : '') +
      ' ' + headerExtension.uri + '\r\n';
};

// Parses an ftmp line, returns dictionary. Sample input:
// a=fmtp:96 vbr=on;cng=on
// Also deals with vbr=on; cng=on
SDPUtils.parseFmtp = function(line) {
  var parsed = {};
  var kv;
  var parts = line.substr(line.indexOf(' ') + 1).split(';');
  for (var j = 0; j < parts.length; j++) {
    kv = parts[j].trim().split('=');
    parsed[kv[0].trim()] = kv[1];
  }
  return parsed;
};

// Generates an a=ftmp line from RTCRtpCodecCapability or RTCRtpCodecParameters.
SDPUtils.writeFmtp = function(codec) {
  var line = '';
  var pt = codec.payloadType;
  if (codec.preferredPayloadType !== undefined) {
    pt = codec.preferredPayloadType;
  }
  if (codec.parameters && Object.keys(codec.parameters).length) {
    var params = [];
    Object.keys(codec.parameters).forEach(function(param) {
      params.push(param + '=' + codec.parameters[param]);
    });
    line += 'a=fmtp:' + pt + ' ' + params.join(';') + '\r\n';
  }
  return line;
};

// Parses an rtcp-fb line, returns RTCPRtcpFeedback object. Sample input:
// a=rtcp-fb:98 nack rpsi
SDPUtils.parseRtcpFb = function(line) {
  var parts = line.substr(line.indexOf(' ') + 1).split(' ');
  return {
    type: parts.shift(),
    parameter: parts.join(' ')
  };
};
// Generate a=rtcp-fb lines from RTCRtpCodecCapability or RTCRtpCodecParameters.
SDPUtils.writeRtcpFb = function(codec) {
  var lines = '';
  var pt = codec.payloadType;
  if (codec.preferredPayloadType !== undefined) {
    pt = codec.preferredPayloadType;
  }
  if (codec.rtcpFeedback && codec.rtcpFeedback.length) {
    // FIXME: special handling for trr-int?
    codec.rtcpFeedback.forEach(function(fb) {
      lines += 'a=rtcp-fb:' + pt + ' ' + fb.type +
      (fb.parameter && fb.parameter.length ? ' ' + fb.parameter : '') +
          '\r\n';
    });
  }
  return lines;
};

// Parses an RFC 5576 ssrc media attribute. Sample input:
// a=ssrc:3735928559 cname:something
SDPUtils.parseSsrcMedia = function(line) {
  var sp = line.indexOf(' ');
  var parts = {
    ssrc: parseInt(line.substr(7, sp - 7), 10)
  };
  var colon = line.indexOf(':', sp);
  if (colon > -1) {
    parts.attribute = line.substr(sp + 1, colon - sp - 1);
    parts.value = line.substr(colon + 1);
  } else {
    parts.attribute = line.substr(sp + 1);
  }
  return parts;
};

// Extracts the MID (RFC 5888) from a media section.
// returns the MID or undefined if no mid line was found.
SDPUtils.getMid = function(mediaSection) {
  var mid = SDPUtils.matchPrefix(mediaSection, 'a=mid:')[0];
  if (mid) {
    return mid.substr(6);
  }
}

SDPUtils.parseFingerprint = function(line) {
  var parts = line.substr(14).split(' ');
  return {
    algorithm: parts[0].toLowerCase(), // algorithm is case-sensitive in Edge.
    value: parts[1]
  };
};

// Extracts DTLS parameters from SDP media section or sessionpart.
// FIXME: for consistency with other functions this should only
//   get the fingerprint line as input. See also getIceParameters.
SDPUtils.getDtlsParameters = function(mediaSection, sessionpart) {
  var lines = SDPUtils.matchPrefix(mediaSection + sessionpart,
      'a=fingerprint:');
  // Note: a=setup line is ignored since we use the 'auto' role.
  // Note2: 'algorithm' is not case sensitive except in Edge.
  return {
    role: 'auto',
    fingerprints: lines.map(SDPUtils.parseFingerprint)
  };
};

// Serializes DTLS parameters to SDP.
SDPUtils.writeDtlsParameters = function(params, setupType) {
  var sdp = 'a=setup:' + setupType + '\r\n';
  params.fingerprints.forEach(function(fp) {
    sdp += 'a=fingerprint:' + fp.algorithm + ' ' + fp.value + '\r\n';
  });
  return sdp;
};
// Parses ICE information from SDP media section or sessionpart.
// FIXME: for consistency with other functions this should only
//   get the ice-ufrag and ice-pwd lines as input.
SDPUtils.getIceParameters = function(mediaSection, sessionpart) {
  var lines = SDPUtils.splitLines(mediaSection);
  // Search in session part, too.
  lines = lines.concat(SDPUtils.splitLines(sessionpart));
  var iceParameters = {
    usernameFragment: lines.filter(function(line) {
      return line.indexOf('a=ice-ufrag:') === 0;
    })[0].substr(12),
    password: lines.filter(function(line) {
      return line.indexOf('a=ice-pwd:') === 0;
    })[0].substr(10)
  };
  return iceParameters;
};

// Serializes ICE parameters to SDP.
SDPUtils.writeIceParameters = function(params) {
  return 'a=ice-ufrag:' + params.usernameFragment + '\r\n' +
      'a=ice-pwd:' + params.password + '\r\n';
};

// Parses the SDP media section and returns RTCRtpParameters.
SDPUtils.parseRtpParameters = function(mediaSection) {
  var description = {
    codecs: [],
    headerExtensions: [],
    fecMechanisms: [],
    rtcp: []
  };
  var lines = SDPUtils.splitLines(mediaSection);
  var mline = lines[0].split(' ');
  for (var i = 3; i < mline.length; i++) { // find all codecs from mline[3..]
    var pt = mline[i];
    var rtpmapline = SDPUtils.matchPrefix(
        mediaSection, 'a=rtpmap:' + pt + ' ')[0];
    if (rtpmapline) {
      var codec = SDPUtils.parseRtpMap(rtpmapline);
      var fmtps = SDPUtils.matchPrefix(
          mediaSection, 'a=fmtp:' + pt + ' ');
      // Only the first a=fmtp:<pt> is considered.
      codec.parameters = fmtps.length ? SDPUtils.parseFmtp(fmtps[0]) : {};
      codec.rtcpFeedback = SDPUtils.matchPrefix(
          mediaSection, 'a=rtcp-fb:' + pt + ' ')
        .map(SDPUtils.parseRtcpFb);
      description.codecs.push(codec);
      // parse FEC mechanisms from rtpmap lines.
      switch (codec.name.toUpperCase()) {
        case 'RED':
        case 'ULPFEC':
          description.fecMechanisms.push(codec.name.toUpperCase());
          break;
        default: // only RED and ULPFEC are recognized as FEC mechanisms.
          break;
      }
    }
  }
  SDPUtils.matchPrefix(mediaSection, 'a=extmap:').forEach(function(line) {
    description.headerExtensions.push(SDPUtils.parseExtmap(line));
  });
  // FIXME: parse rtcp.
  return description;
};

// Generates parts of the SDP media section describing the capabilities /
// parameters.
SDPUtils.writeRtpDescription = function(kind, caps) {
  var sdp = '';

  // Build the mline.
  sdp += 'm=' + kind + ' ';
  sdp += caps.codecs.length > 0 ? '9' : '0'; // reject if no codecs.
  sdp += ' UDP/TLS/RTP/SAVPF ';
  sdp += caps.codecs.map(function(codec) {
    if (codec.preferredPayloadType !== undefined) {
      return codec.preferredPayloadType;
    }
    return codec.payloadType;
  }).join(' ') + '\r\n';

  sdp += 'c=IN IP4 0.0.0.0\r\n';
  sdp += 'a=rtcp:9 IN IP4 0.0.0.0\r\n';

  // Add a=rtpmap lines for each codec. Also fmtp and rtcp-fb.
  caps.codecs.forEach(function(codec) {
    sdp += SDPUtils.writeRtpMap(codec);
    sdp += SDPUtils.writeFmtp(codec);
    sdp += SDPUtils.writeRtcpFb(codec);
  });
  var maxptime = 0;
  caps.codecs.forEach(function(codec) {
    if (codec.maxptime > maxptime) {
      maxptime = codec.maxptime;
    }
  });
  if (maxptime > 0) {
    sdp += 'a=maxptime:' + maxptime + '\r\n';
  }
  sdp += 'a=rtcp-mux\r\n';

  caps.headerExtensions.forEach(function(extension) {
    sdp += SDPUtils.writeExtmap(extension);
  });
  // FIXME: write fecMechanisms.
  return sdp;
};

// Parses the SDP media section and returns an array of
// RTCRtpEncodingParameters.
SDPUtils.parseRtpEncodingParameters = function(mediaSection) {
  var encodingParameters = [];
  var description = SDPUtils.parseRtpParameters(mediaSection);
  var hasRed = description.fecMechanisms.indexOf('RED') !== -1;
  var hasUlpfec = description.fecMechanisms.indexOf('ULPFEC') !== -1;

  // filter a=ssrc:... cname:, ignore PlanB-msid
  var ssrcs = SDPUtils.matchPrefix(mediaSection, 'a=ssrc:')
  .map(function(line) {
    return SDPUtils.parseSsrcMedia(line);
  })
  .filter(function(parts) {
    return parts.attribute === 'cname';
  });
  var primarySsrc = ssrcs.length > 0 && ssrcs[0].ssrc;
  var secondarySsrc;

  var flows = SDPUtils.matchPrefix(mediaSection, 'a=ssrc-group:FID')
  .map(function(line) {
    var parts = line.split(' ');
    parts.shift();
    return parts.map(function(part) {
      return parseInt(part, 10);
    });
  });
  if (flows.length > 0 && flows[0].length > 1 && flows[0][0] === primarySsrc) {
    secondarySsrc = flows[0][1];
  }

  description.codecs.forEach(function(codec) {
    if (codec.name.toUpperCase() === 'RTX' && codec.parameters.apt) {
      var encParam = {
        ssrc: primarySsrc,
        codecPayloadType: parseInt(codec.parameters.apt, 10),
        rtx: {
          ssrc: secondarySsrc
        }
      };
      encodingParameters.push(encParam);
      if (hasRed) {
        encParam = JSON.parse(JSON.stringify(encParam));
        encParam.fec = {
          ssrc: secondarySsrc,
          mechanism: hasUlpfec ? 'red+ulpfec' : 'red'
        };
        encodingParameters.push(encParam);
      }
    }
  });
  if (encodingParameters.length === 0 && primarySsrc) {
    encodingParameters.push({
      ssrc: primarySsrc
    });
  }

  // we support both b=AS and b=TIAS but interpret AS as TIAS.
  var bandwidth = SDPUtils.matchPrefix(mediaSection, 'b=');
  if (bandwidth.length) {
    if (bandwidth[0].indexOf('b=TIAS:') === 0) {
      bandwidth = parseInt(bandwidth[0].substr(7), 10);
    } else if (bandwidth[0].indexOf('b=AS:') === 0) {
      bandwidth = parseInt(bandwidth[0].substr(5), 10);
    }
    encodingParameters.forEach(function(params) {
      params.maxBitrate = bandwidth;
    });
  }
  return encodingParameters;
};

// parses http://draft.ortc.org/#rtcrtcpparameters*
SDPUtils.parseRtcpParameters = function(mediaSection) {
  var rtcpParameters = {};

  var cname;
  // Gets the first SSRC. Note that with RTX there might be multiple
  // SSRCs.
  var remoteSsrc = SDPUtils.matchPrefix(mediaSection, 'a=ssrc:')
      .map(function(line) {
        return SDPUtils.parseSsrcMedia(line);
      })
      .filter(function(obj) {
        return obj.attribute === 'cname';
      })[0];
  if (remoteSsrc) {
    rtcpParameters.cname = remoteSsrc.value;
    rtcpParameters.ssrc = remoteSsrc.ssrc;
  }

  // Edge uses the compound attribute instead of reducedSize
  // compound is !reducedSize
  var rsize = SDPUtils.matchPrefix(mediaSection, 'a=rtcp-rsize');
  rtcpParameters.reducedSize = rsize.length > 0;
  rtcpParameters.compound = rsize.length === 0;

  // parses the rtcp-mux attrіbute.
  // Note that Edge does not support unmuxed RTCP.
  var mux = SDPUtils.matchPrefix(mediaSection, 'a=rtcp-mux');
  rtcpParameters.mux = mux.length > 0;

  return rtcpParameters;
};

// parses either a=msid: or a=ssrc:... msid lines and returns
// the id of the MediaStream and MediaStreamTrack.
SDPUtils.parseMsid = function(mediaSection) {
  var parts;
  var spec = SDPUtils.matchPrefix(mediaSection, 'a=msid:');
  if (spec.length === 1) {
    parts = spec[0].substr(7).split(' ');
    return {stream: parts[0], track: parts[1]};
  }
  var planB = SDPUtils.matchPrefix(mediaSection, 'a=ssrc:')
  .map(function(line) {
    return SDPUtils.parseSsrcMedia(line);
  })
  .filter(function(parts) {
    return parts.attribute === 'msid';
  });
  if (planB.length > 0) {
    parts = planB[0].value.split(' ');
    return {stream: parts[0], track: parts[1]};
  }
};

SDPUtils.writeSessionBoilerplate = function() {
  // FIXME: sess-id should be an NTP timestamp.
  return 'v=0\r\n' +
      'o=thisisadapterortc 8169639915646943137 2 IN IP4 127.0.0.1\r\n' +
      's=-\r\n' +
      't=0 0\r\n';
};

SDPUtils.writeMediaSection = function(transceiver, caps, type, stream) {
  var sdp = SDPUtils.writeRtpDescription(transceiver.kind, caps);

  // Map ICE parameters (ufrag, pwd) to SDP.
  sdp += SDPUtils.writeIceParameters(
      transceiver.iceGatherer.getLocalParameters());

  // Map DTLS parameters to SDP.
  sdp += SDPUtils.writeDtlsParameters(
      transceiver.dtlsTransport.getLocalParameters(),
      type === 'offer' ? 'actpass' : 'active');

  sdp += 'a=mid:' + transceiver.mid + '\r\n';

  if (transceiver.direction) {
    sdp += 'a=' + transceiver.direction + '\r\n';
  } else if (transceiver.rtpSender && transceiver.rtpReceiver) {
    sdp += 'a=sendrecv\r\n';
  } else if (transceiver.rtpSender) {
    sdp += 'a=sendonly\r\n';
  } else if (transceiver.rtpReceiver) {
    sdp += 'a=recvonly\r\n';
  } else {
    sdp += 'a=inactive\r\n';
  }

  if (transceiver.rtpSender) {
    // spec.
    var msid = 'msid:' + stream.id + ' ' +
        transceiver.rtpSender.track.id + '\r\n';
    sdp += 'a=' + msid;

    // for Chrome.
    sdp += 'a=ssrc:' + transceiver.sendEncodingParameters[0].ssrc +
        ' ' + msid;
    if (transceiver.sendEncodingParameters[0].rtx) {
      sdp += 'a=ssrc:' + transceiver.sendEncodingParameters[0].rtx.ssrc +
          ' ' + msid;
      sdp += 'a=ssrc-group:FID ' +
          transceiver.sendEncodingParameters[0].ssrc + ' ' +
          transceiver.sendEncodingParameters[0].rtx.ssrc +
          '\r\n';
    }
  }
  // FIXME: this should be written by writeRtpDescription.
  sdp += 'a=ssrc:' + transceiver.sendEncodingParameters[0].ssrc +
      ' cname:' + SDPUtils.localCName + '\r\n';
  if (transceiver.rtpSender && transceiver.sendEncodingParameters[0].rtx) {
    sdp += 'a=ssrc:' + transceiver.sendEncodingParameters[0].rtx.ssrc +
        ' cname:' + SDPUtils.localCName + '\r\n';
  }
  return sdp;
};

// Gets the direction from the mediaSection or the sessionpart.
SDPUtils.getDirection = function(mediaSection, sessionpart) {
  // Look for sendrecv, sendonly, recvonly, inactive, default to sendrecv.
  var lines = SDPUtils.splitLines(mediaSection);
  for (var i = 0; i < lines.length; i++) {
    switch (lines[i]) {
      case 'a=sendrecv':
      case 'a=sendonly':
      case 'a=recvonly':
      case 'a=inactive':
        return lines[i].substr(2);
      default:
        // FIXME: What should happen here?
    }
  }
  if (sessionpart) {
    return SDPUtils.getDirection(sessionpart);
  }
  return 'sendrecv';
};

SDPUtils.getKind = function(mediaSection) {
  var lines = SDPUtils.splitLines(mediaSection);
  var mline = lines[0].split(' ');
  return mline[0].substr(2);
};

SDPUtils.isRejected = function(mediaSection) {
  return mediaSection.split(' ', 2)[1] === '0';
};

// Expose public methods.
module.exports = SDPUtils;

},{}],19:[function(require,module,exports){
'use strict'

var parseUnit = require('parse-unit')

module.exports = toPX

var PIXELS_PER_INCH = 96

function getPropertyInPX(element, prop) {
  var parts = parseUnit(getComputedStyle(element).getPropertyValue(prop))
  return parts[0] * toPX(parts[1], element)
}

//This brutal hack is needed
function getSizeBrutal(unit, element) {
  var testDIV = document.createElement('div')
  testDIV.style['font-size'] = '128' + unit
  element.appendChild(testDIV)
  var size = getPropertyInPX(testDIV, 'font-size') / 128
  element.removeChild(testDIV)
  return size
}

function toPX(str, element) {
  element = element || document.body
  str = (str || 'px').trim().toLowerCase()
  if(element === window || element === document) {
    element = document.body 
  }
  switch(str) {
    case '%':  //Ambiguous, not sure if we should use width or height
      return element.clientHeight / 100.0
    case 'ch':
    case 'ex':
      return getSizeBrutal(str, element)
    case 'em':
      return getPropertyInPX(element, 'font-size')
    case 'rem':
      return getPropertyInPX(document.body, 'font-size')
    case 'vw':
      return window.innerWidth/100
    case 'vh':
      return window.innerHeight/100
    case 'vmin':
      return Math.min(window.innerWidth, window.innerHeight) / 100
    case 'vmax':
      return Math.max(window.innerWidth, window.innerHeight) / 100
    case 'in':
      return PIXELS_PER_INCH
    case 'cm':
      return PIXELS_PER_INCH / 2.54
    case 'mm':
      return PIXELS_PER_INCH / 25.4
    case 'pt':
      return PIXELS_PER_INCH / 72
    case 'pc':
      return PIXELS_PER_INCH / 6
  }
  return 1
}
},{"parse-unit":12}],20:[function(require,module,exports){
/*
 *  Copyright (c) 2016 The WebRTC project authors. All Rights Reserved.
 *
 *  Use of this source code is governed by a BSD-style license
 *  that can be found in the LICENSE file in the root of the source
 *  tree.
 */
 /* eslint-env node */

'use strict';

// Shimming starts here.
(function() {
  // Utils.
  var logging = require('./utils').log;
  var browserDetails = require('./utils').browserDetails;
  // Export to the adapter global object visible in the browser.
  module.exports.browserDetails = browserDetails;
  module.exports.extractVersion = require('./utils').extractVersion;
  module.exports.disableLog = require('./utils').disableLog;

  // Uncomment the line below if you want logging to occur, including logging
  // for the switch statement below. Can also be turned on in the browser via
  // adapter.disableLog(false), but then logging from the switch statement below
  // will not appear.
  // require('./utils').disableLog(false);

  // Browser shims.
  var chromeShim = require('./chrome/chrome_shim') || null;
  var edgeShim = require('./edge/edge_shim') || null;
  var firefoxShim = require('./firefox/firefox_shim') || null;
  var safariShim = require('./safari/safari_shim') || null;

  // Shim browser if found.
  switch (browserDetails.browser) {
    case 'opera': // fallthrough as it uses chrome shims
    case 'chrome':
      if (!chromeShim || !chromeShim.shimPeerConnection) {
        logging('Chrome shim is not included in this adapter release.');
        return;
      }
      logging('adapter.js shimming chrome.');
      // Export to the adapter global object visible in the browser.
      module.exports.browserShim = chromeShim;

      chromeShim.shimGetUserMedia();
      chromeShim.shimMediaStream();
      chromeShim.shimSourceObject();
      chromeShim.shimPeerConnection();
      chromeShim.shimOnTrack();
      break;
    case 'firefox':
      if (!firefoxShim || !firefoxShim.shimPeerConnection) {
        logging('Firefox shim is not included in this adapter release.');
        return;
      }
      logging('adapter.js shimming firefox.');
      // Export to the adapter global object visible in the browser.
      module.exports.browserShim = firefoxShim;

      firefoxShim.shimGetUserMedia();
      firefoxShim.shimSourceObject();
      firefoxShim.shimPeerConnection();
      firefoxShim.shimOnTrack();
      break;
    case 'edge':
      if (!edgeShim || !edgeShim.shimPeerConnection) {
        logging('MS edge shim is not included in this adapter release.');
        return;
      }
      logging('adapter.js shimming edge.');
      // Export to the adapter global object visible in the browser.
      module.exports.browserShim = edgeShim;

      edgeShim.shimGetUserMedia();
      edgeShim.shimPeerConnection();
      break;
    case 'safari':
      if (!safariShim) {
        logging('Safari shim is not included in this adapter release.');
        return;
      }
      logging('adapter.js shimming safari.');
      // Export to the adapter global object visible in the browser.
      module.exports.browserShim = safariShim;

      safariShim.shimGetUserMedia();
      break;
    default:
      logging('Unsupported browser!');
  }
})();

},{"./chrome/chrome_shim":21,"./edge/edge_shim":23,"./firefox/firefox_shim":25,"./safari/safari_shim":27,"./utils":28}],21:[function(require,module,exports){

/*
 *  Copyright (c) 2016 The WebRTC project authors. All Rights Reserved.
 *
 *  Use of this source code is governed by a BSD-style license
 *  that can be found in the LICENSE file in the root of the source
 *  tree.
 */
 /* eslint-env node */
'use strict';
var logging = require('../utils.js').log;
var browserDetails = require('../utils.js').browserDetails;

var chromeShim = {
  shimMediaStream: function() {
    window.MediaStream = window.MediaStream || window.webkitMediaStream;
  },

  shimOnTrack: function() {
    if (typeof window === 'object' && window.RTCPeerConnection && !('ontrack' in
        window.RTCPeerConnection.prototype)) {
      Object.defineProperty(window.RTCPeerConnection.prototype, 'ontrack', {
        get: function() {
          return this._ontrack;
        },
        set: function(f) {
          var self = this;
          if (this._ontrack) {
            this.removeEventListener('track', this._ontrack);
            this.removeEventListener('addstream', this._ontrackpoly);
          }
          this.addEventListener('track', this._ontrack = f);
          this.addEventListener('addstream', this._ontrackpoly = function(e) {
            // onaddstream does not fire when a track is added to an existing
            // stream. But stream.onaddtrack is implemented so we use that.
            e.stream.addEventListener('addtrack', function(te) {
              var event = new Event('track');
              event.track = te.track;
              event.receiver = {track: te.track};
              event.streams = [e.stream];
              self.dispatchEvent(event);
            });
            e.stream.getTracks().forEach(function(track) {
              var event = new Event('track');
              event.track = track;
              event.receiver = {track: track};
              event.streams = [e.stream];
              this.dispatchEvent(event);
            }.bind(this));
          }.bind(this));
        }
      });
    }
  },

  shimSourceObject: function() {
    if (typeof window === 'object') {
      if (window.HTMLMediaElement &&
        !('srcObject' in window.HTMLMediaElement.prototype)) {
        // Shim the srcObject property, once, when HTMLMediaElement is found.
        Object.defineProperty(window.HTMLMediaElement.prototype, 'srcObject', {
          get: function() {
            return this._srcObject;
          },
          set: function(stream) {
            var self = this;
            // Use _srcObject as a private property for this shim
            this._srcObject = stream;
            if (this.src) {
              URL.revokeObjectURL(this.src);
            }

            if (!stream) {
              this.src = '';
              return;
            }
            this.src = URL.createObjectURL(stream);
            // We need to recreate the blob url when a track is added or
            // removed. Doing it manually since we want to avoid a recursion.
            stream.addEventListener('addtrack', function() {
              if (self.src) {
                URL.revokeObjectURL(self.src);
              }
              self.src = URL.createObjectURL(stream);
            });
            stream.addEventListener('removetrack', function() {
              if (self.src) {
                URL.revokeObjectURL(self.src);
              }
              self.src = URL.createObjectURL(stream);
            });
          }
        });
      }
    }
  },

  shimPeerConnection: function() {
    // The RTCPeerConnection object.
    window.RTCPeerConnection = function(pcConfig, pcConstraints) {
      // Translate iceTransportPolicy to iceTransports,
      // see https://code.google.com/p/webrtc/issues/detail?id=4869
      logging('PeerConnection');
      if (pcConfig && pcConfig.iceTransportPolicy) {
        pcConfig.iceTransports = pcConfig.iceTransportPolicy;
      }

      var pc = new webkitRTCPeerConnection(pcConfig, pcConstraints);
      var origGetStats = pc.getStats.bind(pc);
      pc.getStats = function(selector, successCallback, errorCallback) {
        var self = this;
        var args = arguments;

        // If selector is a function then we are in the old style stats so just
        // pass back the original getStats format to avoid breaking old users.
        if (arguments.length > 0 && typeof selector === 'function') {
          return origGetStats(selector, successCallback);
        }

        var fixChromeStats_ = function(response) {
          var standardReport = {};
          var reports = response.result();
          reports.forEach(function(report) {
            var standardStats = {
              id: report.id,
              timestamp: report.timestamp,
              type: report.type
            };
            report.names().forEach(function(name) {
              standardStats[name] = report.stat(name);
            });
            standardReport[standardStats.id] = standardStats;
          });

          return standardReport;
        };

        // shim getStats with maplike support
        var makeMapStats = function(stats, legacyStats) {
          var map = new Map(Object.keys(stats).map(function(key) {
            return[key, stats[key]];
          }));
          legacyStats = legacyStats || stats;
          Object.keys(legacyStats).forEach(function(key) {
            map[key] = legacyStats[key];
          });
          return map;
        };

        if (arguments.length >= 2) {
          var successCallbackWrapper_ = function(response) {
            args[1](makeMapStats(fixChromeStats_(response)));
          };

          return origGetStats.apply(this, [successCallbackWrapper_,
              arguments[0]]);
        }

        // promise-support
        return new Promise(function(resolve, reject) {
          if (args.length === 1 && typeof selector === 'object') {
            origGetStats.apply(self, [
              function(response) {
                resolve(makeMapStats(fixChromeStats_(response)));
              }, reject]);
          } else {
            // Preserve legacy chrome stats only on legacy access of stats obj
            origGetStats.apply(self, [
              function(response) {
                resolve(makeMapStats(fixChromeStats_(response),
                    response.result()));
              }, reject]);
          }
        }).then(successCallback, errorCallback);
      };

      return pc;
    };
    window.RTCPeerConnection.prototype = webkitRTCPeerConnection.prototype;

    // wrap static methods. Currently just generateCertificate.
    if (webkitRTCPeerConnection.generateCertificate) {
      Object.defineProperty(window.RTCPeerConnection, 'generateCertificate', {
        get: function() {
          return webkitRTCPeerConnection.generateCertificate;
        }
      });
    }

    ['createOffer', 'createAnswer'].forEach(function(method) {
      var nativeMethod = webkitRTCPeerConnection.prototype[method];
      webkitRTCPeerConnection.prototype[method] = function() {
        var self = this;
        if (arguments.length < 1 || (arguments.length === 1 &&
            typeof arguments[0] === 'object')) {
          var opts = arguments.length === 1 ? arguments[0] : undefined;
          return new Promise(function(resolve, reject) {
            nativeMethod.apply(self, [resolve, reject, opts]);
          });
        }
        return nativeMethod.apply(this, arguments);
      };
    });

    // add promise support -- natively available in Chrome 51
    if (browserDetails.version < 51) {
      ['setLocalDescription', 'setRemoteDescription', 'addIceCandidate']
          .forEach(function(method) {
            var nativeMethod = webkitRTCPeerConnection.prototype[method];
            webkitRTCPeerConnection.prototype[method] = function() {
              var args = arguments;
              var self = this;
              var promise = new Promise(function(resolve, reject) {
                nativeMethod.apply(self, [args[0], resolve, reject]);
              });
              if (args.length < 2) {
                return promise;
              }
              return promise.then(function() {
                args[1].apply(null, []);
              },
              function(err) {
                if (args.length >= 3) {
                  args[2].apply(null, [err]);
                }
              });
            };
          });
    }

    // support for addIceCandidate(null)
    var nativeAddIceCandidate =
        RTCPeerConnection.prototype.addIceCandidate;
    RTCPeerConnection.prototype.addIceCandidate = function() {
      return arguments[0] === null ? Promise.resolve()
          : nativeAddIceCandidate.apply(this, arguments);
    };

    // shim implicit creation of RTCSessionDescription/RTCIceCandidate
    ['setLocalDescription', 'setRemoteDescription', 'addIceCandidate']
        .forEach(function(method) {
          var nativeMethod = webkitRTCPeerConnection.prototype[method];
          webkitRTCPeerConnection.prototype[method] = function() {
            arguments[0] = new ((method === 'addIceCandidate') ?
                RTCIceCandidate : RTCSessionDescription)(arguments[0]);
            return nativeMethod.apply(this, arguments);
          };
        });
  },

  // Attach a media stream to an element.
  attachMediaStream: function(element, stream) {
    logging('DEPRECATED, attachMediaStream will soon be removed.');
    if (browserDetails.version >= 43) {
      element.srcObject = stream;
    } else if (typeof element.src !== 'undefined') {
      element.src = URL.createObjectURL(stream);
    } else {
      logging('Error attaching stream to element.');
    }
  },

  reattachMediaStream: function(to, from) {
    logging('DEPRECATED, reattachMediaStream will soon be removed.');
    if (browserDetails.version >= 43) {
      to.srcObject = from.srcObject;
    } else {
      to.src = from.src;
    }
  }
};


// Expose public methods.
module.exports = {
  shimMediaStream: chromeShim.shimMediaStream,
  shimOnTrack: chromeShim.shimOnTrack,
  shimSourceObject: chromeShim.shimSourceObject,
  shimPeerConnection: chromeShim.shimPeerConnection,
  shimGetUserMedia: require('./getusermedia'),
  attachMediaStream: chromeShim.attachMediaStream,
  reattachMediaStream: chromeShim.reattachMediaStream
};

},{"../utils.js":28,"./getusermedia":22}],22:[function(require,module,exports){
/*
 *  Copyright (c) 2016 The WebRTC project authors. All Rights Reserved.
 *
 *  Use of this source code is governed by a BSD-style license
 *  that can be found in the LICENSE file in the root of the source
 *  tree.
 */
 /* eslint-env node */
'use strict';
var logging = require('../utils.js').log;

// Expose public methods.
module.exports = function() {
  var constraintsToChrome_ = function(c) {
    if (typeof c !== 'object' || c.mandatory || c.optional) {
      return c;
    }
    var cc = {};
    Object.keys(c).forEach(function(key) {
      if (key === 'require' || key === 'advanced' || key === 'mediaSource') {
        return;
      }
      var r = (typeof c[key] === 'object') ? c[key] : {ideal: c[key]};
      if (r.exact !== undefined && typeof r.exact === 'number') {
        r.min = r.max = r.exact;
      }
      var oldname_ = function(prefix, name) {
        if (prefix) {
          return prefix + name.charAt(0).toUpperCase() + name.slice(1);
        }
        return (name === 'deviceId') ? 'sourceId' : name;
      };
      if (r.ideal !== undefined) {
        cc.optional = cc.optional || [];
        var oc = {};
        if (typeof r.ideal === 'number') {
          oc[oldname_('min', key)] = r.ideal;
          cc.optional.push(oc);
          oc = {};
          oc[oldname_('max', key)] = r.ideal;
          cc.optional.push(oc);
        } else {
          oc[oldname_('', key)] = r.ideal;
          cc.optional.push(oc);
        }
      }
      if (r.exact !== undefined && typeof r.exact !== 'number') {
        cc.mandatory = cc.mandatory || {};
        cc.mandatory[oldname_('', key)] = r.exact;
      } else {
        ['min', 'max'].forEach(function(mix) {
          if (r[mix] !== undefined) {
            cc.mandatory = cc.mandatory || {};
            cc.mandatory[oldname_(mix, key)] = r[mix];
          }
        });
      }
    });
    if (c.advanced) {
      cc.optional = (cc.optional || []).concat(c.advanced);
    }
    return cc;
  };

  var shimConstraints_ = function(constraints, func) {
    constraints = JSON.parse(JSON.stringify(constraints));
    if (constraints && constraints.audio) {
      constraints.audio = constraintsToChrome_(constraints.audio);
    }
    if (constraints && typeof constraints.video === 'object') {
      // Shim facingMode for mobile, where it defaults to "user".
      var face = constraints.video.facingMode;
      face = face && ((typeof face === 'object') ? face : {ideal: face});

      if ((face && (face.exact === 'user' || face.exact === 'environment' ||
                    face.ideal === 'user' || face.ideal === 'environment')) &&
          !(navigator.mediaDevices.getSupportedConstraints &&
            navigator.mediaDevices.getSupportedConstraints().facingMode)) {
        delete constraints.video.facingMode;
        if (face.exact === 'environment' || face.ideal === 'environment') {
          // Look for "back" in label, or use last cam (typically back cam).
          return navigator.mediaDevices.enumerateDevices()
          .then(function(devices) {
            devices = devices.filter(function(d) {
              return d.kind === 'videoinput';
            });
            var back = devices.find(function(d) {
              return d.label.toLowerCase().indexOf('back') !== -1;
            }) || (devices.length && devices[devices.length - 1]);
            if (back) {
              constraints.video.deviceId = face.exact ? {exact: back.deviceId} :
                                                        {ideal: back.deviceId};
            }
            constraints.video = constraintsToChrome_(constraints.video);
            logging('chrome: ' + JSON.stringify(constraints));
            return func(constraints);
          });
        }
      }
      constraints.video = constraintsToChrome_(constraints.video);
    }
    logging('chrome: ' + JSON.stringify(constraints));
    return func(constraints);
  };

  var shimError_ = function(e) {
    return {
      name: {
        PermissionDeniedError: 'NotAllowedError',
        ConstraintNotSatisfiedError: 'OverconstrainedError'
      }[e.name] || e.name,
      message: e.message,
      constraint: e.constraintName,
      toString: function() {
        return this.name + (this.message && ': ') + this.message;
      }
    };
  };

  var getUserMedia_ = function(constraints, onSuccess, onError) {
    shimConstraints_(constraints, function(c) {
      navigator.webkitGetUserMedia(c, onSuccess, function(e) {
        onError(shimError_(e));
      });
    });
  };

  navigator.getUserMedia = getUserMedia_;

  // Returns the result of getUserMedia as a Promise.
  var getUserMediaPromise_ = function(constraints) {
    return new Promise(function(resolve, reject) {
      navigator.getUserMedia(constraints, resolve, reject);
    });
  };

  if (!navigator.mediaDevices) {
    navigator.mediaDevices = {
      getUserMedia: getUserMediaPromise_,
      enumerateDevices: function() {
        return new Promise(function(resolve) {
          var kinds = {audio: 'audioinput', video: 'videoinput'};
          return MediaStreamTrack.getSources(function(devices) {
            resolve(devices.map(function(device) {
              return {label: device.label,
                      kind: kinds[device.kind],
                      deviceId: device.id,
                      groupId: ''};
            }));
          });
        });
      }
    };
  }

  // A shim for getUserMedia method on the mediaDevices object.
  // TODO(KaptenJansson) remove once implemented in Chrome stable.
  if (!navigator.mediaDevices.getUserMedia) {
    navigator.mediaDevices.getUserMedia = function(constraints) {
      return getUserMediaPromise_(constraints);
    };
  } else {
    // Even though Chrome 45 has navigator.mediaDevices and a getUserMedia
    // function which returns a Promise, it does not accept spec-style
    // constraints.
    var origGetUserMedia = navigator.mediaDevices.getUserMedia.
        bind(navigator.mediaDevices);
    navigator.mediaDevices.getUserMedia = function(cs) {
      return shimConstraints_(cs, function(c) {
        return origGetUserMedia(c).catch(function(e) {
          return Promise.reject(shimError_(e));
        });
      });
    };
  }

  // Dummy devicechange event methods.
  // TODO(KaptenJansson) remove once implemented in Chrome stable.
  if (typeof navigator.mediaDevices.addEventListener === 'undefined') {
    navigator.mediaDevices.addEventListener = function() {
      logging('Dummy mediaDevices.addEventListener called.');
    };
  }
  if (typeof navigator.mediaDevices.removeEventListener === 'undefined') {
    navigator.mediaDevices.removeEventListener = function() {
      logging('Dummy mediaDevices.removeEventListener called.');
    };
  }
};

},{"../utils.js":28}],23:[function(require,module,exports){
/*
 *  Copyright (c) 2016 The WebRTC project authors. All Rights Reserved.
 *
 *  Use of this source code is governed by a BSD-style license
 *  that can be found in the LICENSE file in the root of the source
 *  tree.
 */
 /* eslint-env node */
'use strict';

var SDPUtils = require('sdp');
var logging = require('../utils').log;

var edgeShim = {
  shimPeerConnection: function() {
    if (window.RTCIceGatherer) {
      // ORTC defines an RTCIceCandidate object but no constructor.
      // Not implemented in Edge.
      if (!window.RTCIceCandidate) {
        window.RTCIceCandidate = function(args) {
          return args;
        };
      }
      // ORTC does not have a session description object but
      // other browsers (i.e. Chrome) that will support both PC and ORTC
      // in the future might have this defined already.
      if (!window.RTCSessionDescription) {
        window.RTCSessionDescription = function(args) {
          return args;
        };
      }
    }

    window.RTCPeerConnection = function(config) {
      var self = this;

      var _eventTarget = document.createDocumentFragment();
      ['addEventListener', 'removeEventListener', 'dispatchEvent']
          .forEach(function(method) {
            self[method] = _eventTarget[method].bind(_eventTarget);
          });

      this.onicecandidate = null;
      this.onaddstream = null;
      this.ontrack = null;
      this.onremovestream = null;
      this.onsignalingstatechange = null;
      this.oniceconnectionstatechange = null;
      this.onnegotiationneeded = null;
      this.ondatachannel = null;

      this.localStreams = [];
      this.remoteStreams = [];
      this.getLocalStreams = function() {
        return self.localStreams;
      };
      this.getRemoteStreams = function() {
        return self.remoteStreams;
      };

      this.localDescription = new RTCSessionDescription({
        type: '',
        sdp: ''
      });
      this.remoteDescription = new RTCSessionDescription({
        type: '',
        sdp: ''
      });
      this.signalingState = 'stable';
      this.iceConnectionState = 'new';
      this.iceGatheringState = 'new';

      this.iceOptions = {
        gatherPolicy: 'all',
        iceServers: []
      };
      if (config && config.iceTransportPolicy) {
        switch (config.iceTransportPolicy) {
          case 'all':
          case 'relay':
            this.iceOptions.gatherPolicy = config.iceTransportPolicy;
            break;
          case 'none':
            // FIXME: remove once implementation and spec have added this.
            throw new TypeError('iceTransportPolicy "none" not supported');
          default:
            // don't set iceTransportPolicy.
            break;
        }
      }
      this.usingBundle = config && config.bundlePolicy === 'max-bundle';

      if (config && config.iceServers) {
        // Edge does not like
        // 1) stun:
        // 2) turn: that does not have all of turn:host:port?transport=udp
        var iceServers = JSON.parse(JSON.stringify(config.iceServers));
        this.iceOptions.iceServers = iceServers.filter(function(server) {
          if (server && server.urls) {
            var urls = server.urls;
            if (typeof urls === 'string') {
              urls = [urls];
            }
            urls = urls.filter(function(url) {
              return url.indexOf('turn:') === 0 &&
                  url.indexOf('transport=udp') !== -1;
            })[0];
            return !!urls;
          }
          return false;
        });
      }

      // per-track iceGathers, iceTransports, dtlsTransports, rtpSenders, ...
      // everything that is needed to describe a SDP m-line.
      this.transceivers = [];

      // since the iceGatherer is currently created in createOffer but we
      // must not emit candidates until after setLocalDescription we buffer
      // them in this array.
      this._localIceCandidatesBuffer = [];
    };

    window.RTCPeerConnection.prototype._emitBufferedCandidates = function() {
      var self = this;
      var sections = SDPUtils.splitSections(self.localDescription.sdp);
      // FIXME: need to apply ice candidates in a way which is async but
      // in-order
      this._localIceCandidatesBuffer.forEach(function(event) {
        var end = !event.candidate || Object.keys(event.candidate).length === 0;
        if (end) {
          for (var j = 1; j < sections.length; j++) {
            if (sections[j].indexOf('\r\na=end-of-candidates\r\n') === -1) {
              sections[j] += 'a=end-of-candidates\r\n';
            }
          }
        } else if (event.candidate.candidate.indexOf('typ endOfCandidates')
            === -1) {
          sections[event.candidate.sdpMLineIndex + 1] +=
              'a=' + event.candidate.candidate + '\r\n';
        }
        self.localDescription.sdp = sections.join('');
        self.dispatchEvent(event);
        if (self.onicecandidate !== null) {
          self.onicecandidate(event);
        }
        if (!event.candidate && self.iceGatheringState !== 'complete') {
          var complete = self.transceivers.every(function(transceiver) {
            return transceiver.iceGatherer &&
                transceiver.iceGatherer.state === 'completed';
          });
          if (complete) {
            self.iceGatheringState = 'complete';
          }
        }
      });
      this._localIceCandidatesBuffer = [];
    };

    window.RTCPeerConnection.prototype.addStream = function(stream) {
      // Clone is necessary for local demos mostly, attaching directly
      // to two different senders does not work (build 10547).
      this.localStreams.push(stream.clone());
      this._maybeFireNegotiationNeeded();
    };

    window.RTCPeerConnection.prototype.removeStream = function(stream) {
      var idx = this.localStreams.indexOf(stream);
      if (idx > -1) {
        this.localStreams.splice(idx, 1);
        this._maybeFireNegotiationNeeded();
      }
    };

    window.RTCPeerConnection.prototype.getSenders = function() {
      return this.transceivers.filter(function(transceiver) {
        return !!transceiver.rtpSender;
      })
      .map(function(transceiver) {
        return transceiver.rtpSender;
      });
    };

    window.RTCPeerConnection.prototype.getReceivers = function() {
      return this.transceivers.filter(function(transceiver) {
        return !!transceiver.rtpReceiver;
      })
      .map(function(transceiver) {
        return transceiver.rtpReceiver;
      });
    };

    // Determines the intersection of local and remote capabilities.
    window.RTCPeerConnection.prototype._getCommonCapabilities =
        function(localCapabilities, remoteCapabilities) {
          var commonCapabilities = {
            codecs: [],
            headerExtensions: [],
            fecMechanisms: []
          };
          localCapabilities.codecs.forEach(function(lCodec) {
            for (var i = 0; i < remoteCapabilities.codecs.length; i++) {
              var rCodec = remoteCapabilities.codecs[i];
              if (lCodec.name.toLowerCase() === rCodec.name.toLowerCase() &&
                  lCodec.clockRate === rCodec.clockRate &&
                  lCodec.numChannels === rCodec.numChannels) {
                // push rCodec so we reply with offerer payload type
                commonCapabilities.codecs.push(rCodec);

                // FIXME: also need to determine intersection between
                // .rtcpFeedback and .parameters
                break;
              }
            }
          });

          localCapabilities.headerExtensions
              .forEach(function(lHeaderExtension) {
                for (var i = 0; i < remoteCapabilities.headerExtensions.length;
                     i++) {
                  var rHeaderExtension = remoteCapabilities.headerExtensions[i];
                  if (lHeaderExtension.uri === rHeaderExtension.uri) {
                    commonCapabilities.headerExtensions.push(rHeaderExtension);
                    break;
                  }
                }
              });

          // FIXME: fecMechanisms
          return commonCapabilities;
        };

    // Create ICE gatherer, ICE transport and DTLS transport.
    window.RTCPeerConnection.prototype._createIceAndDtlsTransports =
        function(mid, sdpMLineIndex) {
          var self = this;
          var iceGatherer = new RTCIceGatherer(self.iceOptions);
          var iceTransport = new RTCIceTransport(iceGatherer);
          iceGatherer.onlocalcandidate = function(evt) {
            var event = new Event('icecandidate');
            event.candidate = {sdpMid: mid, sdpMLineIndex: sdpMLineIndex};

            var cand = evt.candidate;
            var end = !cand || Object.keys(cand).length === 0;
            // Edge emits an empty object for RTCIceCandidateComplete‥
            if (end) {
              // polyfill since RTCIceGatherer.state is not implemented in
              // Edge 10547 yet.
              if (iceGatherer.state === undefined) {
                iceGatherer.state = 'completed';
              }

              // Emit a candidate with type endOfCandidates to make the samples
              // work. Edge requires addIceCandidate with this empty candidate
              // to start checking. The real solution is to signal
              // end-of-candidates to the other side when getting the null
              // candidate but some apps (like the samples) don't do that.
              event.candidate.candidate =
                  'candidate:1 1 udp 1 0.0.0.0 9 typ endOfCandidates';
            } else {
              // RTCIceCandidate doesn't have a component, needs to be added
              cand.component = iceTransport.component === 'RTCP' ? 2 : 1;
              event.candidate.candidate = SDPUtils.writeCandidate(cand);
            }

            // update local description.
            var sections = SDPUtils.splitSections(self.localDescription.sdp);
            if (event.candidate.candidate.indexOf('typ endOfCandidates')
                === -1) {
              sections[event.candidate.sdpMLineIndex + 1] +=
                  'a=' + event.candidate.candidate + '\r\n';
            } else {
              sections[event.candidate.sdpMLineIndex + 1] +=
                  'a=end-of-candidates\r\n';
            }
            self.localDescription.sdp = sections.join('');

            var complete = self.transceivers.every(function(transceiver) {
              return transceiver.iceGatherer &&
                  transceiver.iceGatherer.state === 'completed';
            });

            // Emit candidate if localDescription is set.
            // Also emits null candidate when all gatherers are complete.
            switch (self.iceGatheringState) {
              case 'new':
                self._localIceCandidatesBuffer.push(event);
                if (end && complete) {
                  self._localIceCandidatesBuffer.push(
                      new Event('icecandidate'));
                }
                break;
              case 'gathering':
                self._emitBufferedCandidates();
                self.dispatchEvent(event);
                if (self.onicecandidate !== null) {
                  self.onicecandidate(event);
                }
                if (complete) {
                  self.dispatchEvent(new Event('icecandidate'));
                  if (self.onicecandidate !== null) {
                    self.onicecandidate(new Event('icecandidate'));
                  }
                  self.iceGatheringState = 'complete';
                }
                break;
              case 'complete':
                // should not happen... currently!
                break;
              default: // no-op.
                break;
            }
          };
          iceTransport.onicestatechange = function() {
            self._updateConnectionState();
          };

          var dtlsTransport = new RTCDtlsTransport(iceTransport);
          dtlsTransport.ondtlsstatechange = function() {
            self._updateConnectionState();
          };
          dtlsTransport.onerror = function() {
            // onerror does not set state to failed by itself.
            dtlsTransport.state = 'failed';
            self._updateConnectionState();
          };

          return {
            iceGatherer: iceGatherer,
            iceTransport: iceTransport,
            dtlsTransport: dtlsTransport
          };
        };

    // Start the RTP Sender and Receiver for a transceiver.
    window.RTCPeerConnection.prototype._transceive = function(transceiver,
        send, recv) {
      var params = this._getCommonCapabilities(transceiver.localCapabilities,
          transceiver.remoteCapabilities);
      if (send && transceiver.rtpSender) {
        params.encodings = transceiver.sendEncodingParameters;
        params.rtcp = {
          cname: SDPUtils.localCName
        };
        if (transceiver.recvEncodingParameters.length) {
          params.rtcp.ssrc = transceiver.recvEncodingParameters[0].ssrc;
        }
        transceiver.rtpSender.send(params);
      }
      if (recv && transceiver.rtpReceiver) {
        params.encodings = transceiver.recvEncodingParameters;
        params.rtcp = {
          cname: transceiver.cname
        };
        if (transceiver.sendEncodingParameters.length) {
          params.rtcp.ssrc = transceiver.sendEncodingParameters[0].ssrc;
        }
        transceiver.rtpReceiver.receive(params);
      }
    };

    window.RTCPeerConnection.prototype.setLocalDescription =
        function(description) {
          var self = this;
          var sections;
          var sessionpart;
          if (description.type === 'offer') {
            // FIXME: What was the purpose of this empty if statement?
            // if (!this._pendingOffer) {
            // } else {
            if (this._pendingOffer) {
              // VERY limited support for SDP munging. Limited to:
              // * changing the order of codecs
              sections = SDPUtils.splitSections(description.sdp);
              sessionpart = sections.shift();
              sections.forEach(function(mediaSection, sdpMLineIndex) {
                var caps = SDPUtils.parseRtpParameters(mediaSection);
                self._pendingOffer[sdpMLineIndex].localCapabilities = caps;
              });
              this.transceivers = this._pendingOffer;
              delete this._pendingOffer;
            }
          } else if (description.type === 'answer') {
            sections = SDPUtils.splitSections(self.remoteDescription.sdp);
            sessionpart = sections.shift();
            var isIceLite = SDPUtils.matchPrefix(sessionpart,
                'a=ice-lite').length > 0;
            sections.forEach(function(mediaSection, sdpMLineIndex) {
              var transceiver = self.transceivers[sdpMLineIndex];
              var iceGatherer = transceiver.iceGatherer;
              var iceTransport = transceiver.iceTransport;
              var dtlsTransport = transceiver.dtlsTransport;
              var localCapabilities = transceiver.localCapabilities;
              var remoteCapabilities = transceiver.remoteCapabilities;
              var rejected = mediaSection.split('\n', 1)[0]
                  .split(' ', 2)[1] === '0';

              if (!rejected) {
                var remoteIceParameters = SDPUtils.getIceParameters(
                    mediaSection, sessionpart);
                if (isIceLite) {
                  var cands = SDPUtils.matchPrefix(mediaSection, 'a=candidate:')
                  .map(function(cand) {
                    return SDPUtils.parseCandidate(cand);
                  })
                  .filter(function(cand) {
                    return cand.component === '1';
                  });
                  // ice-lite only includes host candidates in the SDP so we can
                  // use setRemoteCandidates (which implies an
                  // RTCIceCandidateComplete)
                  if (cands.length) {
                    iceTransport.setRemoteCandidates(cands);
                  }
                }
                var remoteDtlsParameters = SDPUtils.getDtlsParameters(
                    mediaSection, sessionpart);
                if (isIceLite) {
                  remoteDtlsParameters.role = 'server';
                }

                if (!self.usingBundle || sdpMLineIndex === 0) {
                  iceTransport.start(iceGatherer, remoteIceParameters,
                      isIceLite ? 'controlling' : 'controlled');
                  dtlsTransport.start(remoteDtlsParameters);
                }

                // Calculate intersection of capabilities.
                var params = self._getCommonCapabilities(localCapabilities,
                    remoteCapabilities);

                // Start the RTCRtpSender. The RTCRtpReceiver for this
                // transceiver has already been started in setRemoteDescription.
                self._transceive(transceiver,
                    params.codecs.length > 0,
                    false);
              }
            });
          }

          this.localDescription = {
            type: description.type,
            sdp: description.sdp
          };
          switch (description.type) {
            case 'offer':
              this._updateSignalingState('have-local-offer');
              break;
            case 'answer':
              this._updateSignalingState('stable');
              break;
            default:
              throw new TypeError('unsupported type "' + description.type +
                  '"');
          }

          // If a success callback was provided, emit ICE candidates after it
          // has been executed. Otherwise, emit callback after the Promise is
          // resolved.
          var hasCallback = arguments.length > 1 &&
            typeof arguments[1] === 'function';
          if (hasCallback) {
            var cb = arguments[1];
            window.setTimeout(function() {
              cb();
              if (self.iceGatheringState === 'new') {
                self.iceGatheringState = 'gathering';
              }
              self._emitBufferedCandidates();
            }, 0);
          }
          var p = Promise.resolve();
          p.then(function() {
            if (!hasCallback) {
              if (self.iceGatheringState === 'new') {
                self.iceGatheringState = 'gathering';
              }
              // Usually candidates will be emitted earlier.
              window.setTimeout(self._emitBufferedCandidates.bind(self), 500);
            }
          });
          return p;
        };

    window.RTCPeerConnection.prototype.setRemoteDescription =
        function(description) {
          var self = this;
          var stream = new MediaStream();
          var receiverList = [];
          var sections = SDPUtils.splitSections(description.sdp);
          var sessionpart = sections.shift();
          var isIceLite = SDPUtils.matchPrefix(sessionpart,
              'a=ice-lite').length > 0;
          this.usingBundle = SDPUtils.matchPrefix(sessionpart,
              'a=group:BUNDLE ').length > 0;
          sections.forEach(function(mediaSection, sdpMLineIndex) {
            var lines = SDPUtils.splitLines(mediaSection);
            var mline = lines[0].substr(2).split(' ');
            var kind = mline[0];
            var rejected = mline[1] === '0';
            var direction = SDPUtils.getDirection(mediaSection, sessionpart);

            var transceiver;
            var iceGatherer;
            var iceTransport;
            var dtlsTransport;
            var rtpSender;
            var rtpReceiver;
            var sendEncodingParameters;
            var recvEncodingParameters;
            var localCapabilities;

            var track;
            // FIXME: ensure the mediaSection has rtcp-mux set.
            var remoteCapabilities = SDPUtils.parseRtpParameters(mediaSection);
            var remoteIceParameters;
            var remoteDtlsParameters;
            if (!rejected) {
              remoteIceParameters = SDPUtils.getIceParameters(mediaSection,
                  sessionpart);
              remoteDtlsParameters = SDPUtils.getDtlsParameters(mediaSection,
                  sessionpart);
              remoteDtlsParameters.role = 'client';
            }
            recvEncodingParameters =
                SDPUtils.parseRtpEncodingParameters(mediaSection);

            var mid = SDPUtils.matchPrefix(mediaSection, 'a=mid:');
            if (mid.length) {
              mid = mid[0].substr(6);
            } else {
              mid = SDPUtils.generateIdentifier();
            }

            var cname;
            // Gets the first SSRC. Note that with RTX there might be multiple
            // SSRCs.
            var remoteSsrc = SDPUtils.matchPrefix(mediaSection, 'a=ssrc:')
                .map(function(line) {
                  return SDPUtils.parseSsrcMedia(line);
                })
                .filter(function(obj) {
                  return obj.attribute === 'cname';
                })[0];
            if (remoteSsrc) {
              cname = remoteSsrc.value;
            }

            var isComplete = SDPUtils.matchPrefix(mediaSection,
                'a=end-of-candidates').length > 0;
            var cands = SDPUtils.matchPrefix(mediaSection, 'a=candidate:')
                .map(function(cand) {
                  return SDPUtils.parseCandidate(cand);
                })
                .filter(function(cand) {
                  return cand.component === '1';
                });
            if (description.type === 'offer' && !rejected) {
              var transports = self.usingBundle && sdpMLineIndex > 0 ? {
                iceGatherer: self.transceivers[0].iceGatherer,
                iceTransport: self.transceivers[0].iceTransport,
                dtlsTransport: self.transceivers[0].dtlsTransport
              } : self._createIceAndDtlsTransports(mid, sdpMLineIndex);

              if (isComplete) {
                transports.iceTransport.setRemoteCandidates(cands);
              }

              localCapabilities = RTCRtpReceiver.getCapabilities(kind);
              sendEncodingParameters = [{
                ssrc: (2 * sdpMLineIndex + 2) * 1001
              }];

              rtpReceiver = new RTCRtpReceiver(transports.dtlsTransport, kind);

              track = rtpReceiver.track;
              receiverList.push([track, rtpReceiver]);
              // FIXME: not correct when there are multiple streams but that is
              // not currently supported in this shim.
              stream.addTrack(track);

              // FIXME: look at direction.
              if (self.localStreams.length > 0 &&
                  self.localStreams[0].getTracks().length >= sdpMLineIndex) {
                // FIXME: actually more complicated, needs to match types etc
                var localtrack = self.localStreams[0]
                    .getTracks()[sdpMLineIndex];
                rtpSender = new RTCRtpSender(localtrack,
                    transports.dtlsTransport);
              }

              self.transceivers[sdpMLineIndex] = {
                iceGatherer: transports.iceGatherer,
                iceTransport: transports.iceTransport,
                dtlsTransport: transports.dtlsTransport,
                localCapabilities: localCapabilities,
                remoteCapabilities: remoteCapabilities,
                rtpSender: rtpSender,
                rtpReceiver: rtpReceiver,
                kind: kind,
                mid: mid,
                cname: cname,
                sendEncodingParameters: sendEncodingParameters,
                recvEncodingParameters: recvEncodingParameters
              };
              // Start the RTCRtpReceiver now. The RTPSender is started in
              // setLocalDescription.
              self._transceive(self.transceivers[sdpMLineIndex],
                  false,
                  direction === 'sendrecv' || direction === 'sendonly');
            } else if (description.type === 'answer' && !rejected) {
              transceiver = self.transceivers[sdpMLineIndex];
              iceGatherer = transceiver.iceGatherer;
              iceTransport = transceiver.iceTransport;
              dtlsTransport = transceiver.dtlsTransport;
              rtpSender = transceiver.rtpSender;
              rtpReceiver = transceiver.rtpReceiver;
              sendEncodingParameters = transceiver.sendEncodingParameters;
              localCapabilities = transceiver.localCapabilities;

              self.transceivers[sdpMLineIndex].recvEncodingParameters =
                  recvEncodingParameters;
              self.transceivers[sdpMLineIndex].remoteCapabilities =
                  remoteCapabilities;
              self.transceivers[sdpMLineIndex].cname = cname;

              if ((isIceLite || isComplete) && cands.length) {
                iceTransport.setRemoteCandidates(cands);
              }
              if (!self.usingBundle || sdpMLineIndex === 0) {
                iceTransport.start(iceGatherer, remoteIceParameters,
                    'controlling');
                dtlsTransport.start(remoteDtlsParameters);
              }

              self._transceive(transceiver,
                  direction === 'sendrecv' || direction === 'recvonly',
                  direction === 'sendrecv' || direction === 'sendonly');

              if (rtpReceiver &&
                  (direction === 'sendrecv' || direction === 'sendonly')) {
                track = rtpReceiver.track;
                receiverList.push([track, rtpReceiver]);
                stream.addTrack(track);
              } else {
                // FIXME: actually the receiver should be created later.
                delete transceiver.rtpReceiver;
              }
            }
          });

          this.remoteDescription = {
            type: description.type,
            sdp: description.sdp
          };
          switch (description.type) {
            case 'offer':
              this._updateSignalingState('have-remote-offer');
              break;
            case 'answer':
              this._updateSignalingState('stable');
              break;
            default:
              throw new TypeError('unsupported type "' + description.type +
                  '"');
          }
          if (stream.getTracks().length) {
            self.remoteStreams.push(stream);
            window.setTimeout(function() {
              var event = new Event('addstream');
              event.stream = stream;
              self.dispatchEvent(event);
              if (self.onaddstream !== null) {
                window.setTimeout(function() {
                  self.onaddstream(event);
                }, 0);
              }

              receiverList.forEach(function(item) {
                var track = item[0];
                var receiver = item[1];
                var trackEvent = new Event('track');
                trackEvent.track = track;
                trackEvent.receiver = receiver;
                trackEvent.streams = [stream];
                self.dispatchEvent(event);
                if (self.ontrack !== null) {
                  window.setTimeout(function() {
                    self.ontrack(trackEvent);
                  }, 0);
                }
              });
            }, 0);
          }
          if (arguments.length > 1 && typeof arguments[1] === 'function') {
            window.setTimeout(arguments[1], 0);
          }
          return Promise.resolve();
        };

    window.RTCPeerConnection.prototype.close = function() {
      this.transceivers.forEach(function(transceiver) {
        /* not yet
        if (transceiver.iceGatherer) {
          transceiver.iceGatherer.close();
        }
        */
        if (transceiver.iceTransport) {
          transceiver.iceTransport.stop();
        }
        if (transceiver.dtlsTransport) {
          transceiver.dtlsTransport.stop();
        }
        if (transceiver.rtpSender) {
          transceiver.rtpSender.stop();
        }
        if (transceiver.rtpReceiver) {
          transceiver.rtpReceiver.stop();
        }
      });
      // FIXME: clean up tracks, local streams, remote streams, etc
      this._updateSignalingState('closed');
    };

    // Update the signaling state.
    window.RTCPeerConnection.prototype._updateSignalingState =
        function(newState) {
          this.signalingState = newState;
          var event = new Event('signalingstatechange');
          this.dispatchEvent(event);
          if (this.onsignalingstatechange !== null) {
            this.onsignalingstatechange(event);
          }
        };

    // Determine whether to fire the negotiationneeded event.
    window.RTCPeerConnection.prototype._maybeFireNegotiationNeeded =
        function() {
          // Fire away (for now).
          var event = new Event('negotiationneeded');
          this.dispatchEvent(event);
          if (this.onnegotiationneeded !== null) {
            this.onnegotiationneeded(event);
          }
        };

    // Update the connection state.
    window.RTCPeerConnection.prototype._updateConnectionState = function() {
      var self = this;
      var newState;
      var states = {
        'new': 0,
        closed: 0,
        connecting: 0,
        checking: 0,
        connected: 0,
        completed: 0,
        failed: 0
      };
      this.transceivers.forEach(function(transceiver) {
        states[transceiver.iceTransport.state]++;
        states[transceiver.dtlsTransport.state]++;
      });
      // ICETransport.completed and connected are the same for this purpose.
      states.connected += states.completed;

      newState = 'new';
      if (states.failed > 0) {
        newState = 'failed';
      } else if (states.connecting > 0 || states.checking > 0) {
        newState = 'connecting';
      } else if (states.disconnected > 0) {
        newState = 'disconnected';
      } else if (states.new > 0) {
        newState = 'new';
      } else if (states.connected > 0 || states.completed > 0) {
        newState = 'connected';
      }

      if (newState !== self.iceConnectionState) {
        self.iceConnectionState = newState;
        var event = new Event('iceconnectionstatechange');
        this.dispatchEvent(event);
        if (this.oniceconnectionstatechange !== null) {
          this.oniceconnectionstatechange(event);
        }
      }
    };

    window.RTCPeerConnection.prototype.createOffer = function() {
      var self = this;
      if (this._pendingOffer) {
        throw new Error('createOffer called while there is a pending offer.');
      }
      var offerOptions;
      if (arguments.length === 1 && typeof arguments[0] !== 'function') {
        offerOptions = arguments[0];
      } else if (arguments.length === 3) {
        offerOptions = arguments[2];
      }

      var tracks = [];
      var numAudioTracks = 0;
      var numVideoTracks = 0;
      // Default to sendrecv.
      if (this.localStreams.length) {
        numAudioTracks = this.localStreams[0].getAudioTracks().length;
        numVideoTracks = this.localStreams[0].getVideoTracks().length;
      }
      // Determine number of audio and video tracks we need to send/recv.
      if (offerOptions) {
        // Reject Chrome legacy constraints.
        if (offerOptions.mandatory || offerOptions.optional) {
          throw new TypeError(
              'Legacy mandatory/optional constraints not supported.');
        }
        if (offerOptions.offerToReceiveAudio !== undefined) {
          numAudioTracks = offerOptions.offerToReceiveAudio;
        }
        if (offerOptions.offerToReceiveVideo !== undefined) {
          numVideoTracks = offerOptions.offerToReceiveVideo;
        }
      }
      if (this.localStreams.length) {
        // Push local streams.
        this.localStreams[0].getTracks().forEach(function(track) {
          tracks.push({
            kind: track.kind,
            track: track,
            wantReceive: track.kind === 'audio' ?
                numAudioTracks > 0 : numVideoTracks > 0
          });
          if (track.kind === 'audio') {
            numAudioTracks--;
          } else if (track.kind === 'video') {
            numVideoTracks--;
          }
        });
      }
      // Create M-lines for recvonly streams.
      while (numAudioTracks > 0 || numVideoTracks > 0) {
        if (numAudioTracks > 0) {
          tracks.push({
            kind: 'audio',
            wantReceive: true
          });
          numAudioTracks--;
        }
        if (numVideoTracks > 0) {
          tracks.push({
            kind: 'video',
            wantReceive: true
          });
          numVideoTracks--;
        }
      }

      var sdp = SDPUtils.writeSessionBoilerplate();
      var transceivers = [];
      tracks.forEach(function(mline, sdpMLineIndex) {
        // For each track, create an ice gatherer, ice transport,
        // dtls transport, potentially rtpsender and rtpreceiver.
        var track = mline.track;
        var kind = mline.kind;
        var mid = SDPUtils.generateIdentifier();

        var transports = self.usingBundle && sdpMLineIndex > 0 ? {
          iceGatherer: transceivers[0].iceGatherer,
          iceTransport: transceivers[0].iceTransport,
          dtlsTransport: transceivers[0].dtlsTransport
        } : self._createIceAndDtlsTransports(mid, sdpMLineIndex);

        var localCapabilities = RTCRtpSender.getCapabilities(kind);
        var rtpSender;
        var rtpReceiver;

        // generate an ssrc now, to be used later in rtpSender.send
        var sendEncodingParameters = [{
          ssrc: (2 * sdpMLineIndex + 1) * 1001
        }];
        if (track) {
          rtpSender = new RTCRtpSender(track, transports.dtlsTransport);
        }

        if (mline.wantReceive) {
          rtpReceiver = new RTCRtpReceiver(transports.dtlsTransport, kind);
        }

        transceivers[sdpMLineIndex] = {
          iceGatherer: transports.iceGatherer,
          iceTransport: transports.iceTransport,
          dtlsTransport: transports.dtlsTransport,
          localCapabilities: localCapabilities,
          remoteCapabilities: null,
          rtpSender: rtpSender,
          rtpReceiver: rtpReceiver,
          kind: kind,
          mid: mid,
          sendEncodingParameters: sendEncodingParameters,
          recvEncodingParameters: null
        };
      });
      if (this.usingBundle) {
        sdp += 'a=group:BUNDLE ' + transceivers.map(function(t) {
          return t.mid;
        }).join(' ') + '\r\n';
      }
      tracks.forEach(function(mline, sdpMLineIndex) {
        var transceiver = transceivers[sdpMLineIndex];
        sdp += SDPUtils.writeMediaSection(transceiver,
            transceiver.localCapabilities, 'offer', self.localStreams[0]);
      });

      this._pendingOffer = transceivers;
      var desc = new RTCSessionDescription({
        type: 'offer',
        sdp: sdp
      });
      if (arguments.length && typeof arguments[0] === 'function') {
        window.setTimeout(arguments[0], 0, desc);
      }
      return Promise.resolve(desc);
    };

    window.RTCPeerConnection.prototype.createAnswer = function() {
      var self = this;

      var sdp = SDPUtils.writeSessionBoilerplate();
      if (this.usingBundle) {
        sdp += 'a=group:BUNDLE ' + this.transceivers.map(function(t) {
          return t.mid;
        }).join(' ') + '\r\n';
      }
      this.transceivers.forEach(function(transceiver) {
        // Calculate intersection of capabilities.
        var commonCapabilities = self._getCommonCapabilities(
            transceiver.localCapabilities,
            transceiver.remoteCapabilities);

        sdp += SDPUtils.writeMediaSection(transceiver, commonCapabilities,
            'answer', self.localStreams[0]);
      });

      var desc = new RTCSessionDescription({
        type: 'answer',
        sdp: sdp
      });
      if (arguments.length && typeof arguments[0] === 'function') {
        window.setTimeout(arguments[0], 0, desc);
      }
      return Promise.resolve(desc);
    };

    window.RTCPeerConnection.prototype.addIceCandidate = function(candidate) {
      if (candidate === null) {
        this.transceivers.forEach(function(transceiver) {
          transceiver.iceTransport.addRemoteCandidate({});
        });
      } else {
        var mLineIndex = candidate.sdpMLineIndex;
        if (candidate.sdpMid) {
          for (var i = 0; i < this.transceivers.length; i++) {
            if (this.transceivers[i].mid === candidate.sdpMid) {
              mLineIndex = i;
              break;
            }
          }
        }
        var transceiver = this.transceivers[mLineIndex];
        if (transceiver) {
          var cand = Object.keys(candidate.candidate).length > 0 ?
              SDPUtils.parseCandidate(candidate.candidate) : {};
          // Ignore Chrome's invalid candidates since Edge does not like them.
          if (cand.protocol === 'tcp' && cand.port === 0) {
            return;
          }
          // Ignore RTCP candidates, we assume RTCP-MUX.
          if (cand.component !== '1') {
            return;
          }
          // A dirty hack to make samples work.
          if (cand.type === 'endOfCandidates') {
            cand = {};
          }
          transceiver.iceTransport.addRemoteCandidate(cand);

          // update the remoteDescription.
          var sections = SDPUtils.splitSections(this.remoteDescription.sdp);
          sections[mLineIndex + 1] += (cand.type ? candidate.candidate.trim()
              : 'a=end-of-candidates') + '\r\n';
          this.remoteDescription.sdp = sections.join('');
        }
      }
      if (arguments.length > 1 && typeof arguments[1] === 'function') {
        window.setTimeout(arguments[1], 0);
      }
      return Promise.resolve();
    };

    window.RTCPeerConnection.prototype.getStats = function() {
      var promises = [];
      this.transceivers.forEach(function(transceiver) {
        ['rtpSender', 'rtpReceiver', 'iceGatherer', 'iceTransport',
            'dtlsTransport'].forEach(function(method) {
              if (transceiver[method]) {
                promises.push(transceiver[method].getStats());
              }
            });
      });
      var cb = arguments.length > 1 && typeof arguments[1] === 'function' &&
          arguments[1];
      return new Promise(function(resolve) {
        // shim getStats with maplike support
        var results = new Map();
        Promise.all(promises).then(function(res) {
          res.forEach(function(result) {
            Object.keys(result).forEach(function(id) {
              results.set(id, result[id]);
              results[id] = result[id];
            });
          });
          if (cb) {
            window.setTimeout(cb, 0, results);
          }
          resolve(results);
        });
      });
    };
  },

  // Attach a media stream to an element.
  attachMediaStream: function(element, stream) {
    logging('DEPRECATED, attachMediaStream will soon be removed.');
    element.srcObject = stream;
  },

  reattachMediaStream: function(to, from) {
    logging('DEPRECATED, reattachMediaStream will soon be removed.');
    to.srcObject = from.srcObject;
  }
};

// Expose public methods.
module.exports = {
  shimPeerConnection: edgeShim.shimPeerConnection,
  shimGetUserMedia: require('./getusermedia'),
  attachMediaStream: edgeShim.attachMediaStream,
  reattachMediaStream: edgeShim.reattachMediaStream
};

},{"../utils":28,"./getusermedia":24,"sdp":18}],24:[function(require,module,exports){
/*
 *  Copyright (c) 2016 The WebRTC project authors. All Rights Reserved.
 *
 *  Use of this source code is governed by a BSD-style license
 *  that can be found in the LICENSE file in the root of the source
 *  tree.
 */
 /* eslint-env node */
'use strict';

// Expose public methods.
module.exports = function() {
  var shimError_ = function(e) {
    return {
      name: {PermissionDeniedError: 'NotAllowedError'}[e.name] || e.name,
      message: e.message,
      constraint: e.constraint,
      toString: function() {
        return this.name;
      }
    };
  };

  // getUserMedia error shim.
  var origGetUserMedia = navigator.mediaDevices.getUserMedia.
      bind(navigator.mediaDevices);
  navigator.mediaDevices.getUserMedia = function(c) {
    return origGetUserMedia(c).catch(function(e) {
      return Promise.reject(shimError_(e));
    });
  };
};

},{}],25:[function(require,module,exports){
/*
 *  Copyright (c) 2016 The WebRTC project authors. All Rights Reserved.
 *
 *  Use of this source code is governed by a BSD-style license
 *  that can be found in the LICENSE file in the root of the source
 *  tree.
 */
 /* eslint-env node */
'use strict';

var logging = require('../utils').log;
var browserDetails = require('../utils').browserDetails;

var firefoxShim = {
  shimOnTrack: function() {
    if (typeof window === 'object' && window.RTCPeerConnection && !('ontrack' in
        window.RTCPeerConnection.prototype)) {
      Object.defineProperty(window.RTCPeerConnection.prototype, 'ontrack', {
        get: function() {
          return this._ontrack;
        },
        set: function(f) {
          if (this._ontrack) {
            this.removeEventListener('track', this._ontrack);
            this.removeEventListener('addstream', this._ontrackpoly);
          }
          this.addEventListener('track', this._ontrack = f);
          this.addEventListener('addstream', this._ontrackpoly = function(e) {
            e.stream.getTracks().forEach(function(track) {
              var event = new Event('track');
              event.track = track;
              event.receiver = {track: track};
              event.streams = [e.stream];
              this.dispatchEvent(event);
            }.bind(this));
          }.bind(this));
        }
      });
    }
  },

  shimSourceObject: function() {
    // Firefox has supported mozSrcObject since FF22, unprefixed in 42.
    if (typeof window === 'object') {
      if (window.HTMLMediaElement &&
        !('srcObject' in window.HTMLMediaElement.prototype)) {
        // Shim the srcObject property, once, when HTMLMediaElement is found.
        Object.defineProperty(window.HTMLMediaElement.prototype, 'srcObject', {
          get: function() {
            return this.mozSrcObject;
          },
          set: function(stream) {
            this.mozSrcObject = stream;
          }
        });
      }
    }
  },

  shimPeerConnection: function() {
    if (typeof window !== 'object' || !(window.RTCPeerConnection ||
        window.mozRTCPeerConnection)) {
      return; // probably media.peerconnection.enabled=false in about:config
    }
    // The RTCPeerConnection object.
    if (!window.RTCPeerConnection) {
      window.RTCPeerConnection = function(pcConfig, pcConstraints) {
        if (browserDetails.version < 38) {
          // .urls is not supported in FF < 38.
          // create RTCIceServers with a single url.
          if (pcConfig && pcConfig.iceServers) {
            var newIceServers = [];
            for (var i = 0; i < pcConfig.iceServers.length; i++) {
              var server = pcConfig.iceServers[i];
              if (server.hasOwnProperty('urls')) {
                for (var j = 0; j < server.urls.length; j++) {
                  var newServer = {
                    url: server.urls[j]
                  };
                  if (server.urls[j].indexOf('turn') === 0) {
                    newServer.username = server.username;
                    newServer.credential = server.credential;
                  }
                  newIceServers.push(newServer);
                }
              } else {
                newIceServers.push(pcConfig.iceServers[i]);
              }
            }
            pcConfig.iceServers = newIceServers;
          }
        }
        return new mozRTCPeerConnection(pcConfig, pcConstraints);
      };
      window.RTCPeerConnection.prototype = mozRTCPeerConnection.prototype;

      // wrap static methods. Currently just generateCertificate.
      if (mozRTCPeerConnection.generateCertificate) {
        Object.defineProperty(window.RTCPeerConnection, 'generateCertificate', {
          get: function() {
            return mozRTCPeerConnection.generateCertificate;
          }
        });
      }

      window.RTCSessionDescription = mozRTCSessionDescription;
      window.RTCIceCandidate = mozRTCIceCandidate;
    }

    // shim away need for obsolete RTCIceCandidate/RTCSessionDescription.
    ['setLocalDescription', 'setRemoteDescription', 'addIceCandidate']
        .forEach(function(method) {
          var nativeMethod = RTCPeerConnection.prototype[method];
          RTCPeerConnection.prototype[method] = function() {
            arguments[0] = new ((method === 'addIceCandidate') ?
                RTCIceCandidate : RTCSessionDescription)(arguments[0]);
            return nativeMethod.apply(this, arguments);
          };
        });

    // support for addIceCandidate(null)
    var nativeAddIceCandidate =
        RTCPeerConnection.prototype.addIceCandidate;
    RTCPeerConnection.prototype.addIceCandidate = function() {
      return arguments[0] === null ? Promise.resolve()
          : nativeAddIceCandidate.apply(this, arguments);
    };

    // shim getStats with maplike support
    var makeMapStats = function(stats) {
      var map = new Map();
      Object.keys(stats).forEach(function(key) {
        map.set(key, stats[key]);
        map[key] = stats[key];
      });
      return map;
    };

    var nativeGetStats = RTCPeerConnection.prototype.getStats;
    RTCPeerConnection.prototype.getStats = function(selector, onSucc, onErr) {
      return nativeGetStats.apply(this, [selector || null])
        .then(function(stats) {
          return makeMapStats(stats);
        })
        .then(onSucc, onErr);
    };
  },

  // Attach a media stream to an element.
  attachMediaStream: function(element, stream) {
    logging('DEPRECATED, attachMediaStream will soon be removed.');
    element.srcObject = stream;
  },

  reattachMediaStream: function(to, from) {
    logging('DEPRECATED, reattachMediaStream will soon be removed.');
    to.srcObject = from.srcObject;
  }
};

// Expose public methods.
module.exports = {
  shimOnTrack: firefoxShim.shimOnTrack,
  shimSourceObject: firefoxShim.shimSourceObject,
  shimPeerConnection: firefoxShim.shimPeerConnection,
  shimGetUserMedia: require('./getusermedia'),
  attachMediaStream: firefoxShim.attachMediaStream,
  reattachMediaStream: firefoxShim.reattachMediaStream
};

},{"../utils":28,"./getusermedia":26}],26:[function(require,module,exports){
/*
 *  Copyright (c) 2016 The WebRTC project authors. All Rights Reserved.
 *
 *  Use of this source code is governed by a BSD-style license
 *  that can be found in the LICENSE file in the root of the source
 *  tree.
 */
 /* eslint-env node */
'use strict';

var logging = require('../utils').log;
var browserDetails = require('../utils').browserDetails;

// Expose public methods.
module.exports = function() {
  var shimError_ = function(e) {
    return {
      name: {
        SecurityError: 'NotAllowedError',
        PermissionDeniedError: 'NotAllowedError'
      }[e.name] || e.name,
      message: {
        'The operation is insecure.': 'The request is not allowed by the ' +
        'user agent or the platform in the current context.'
      }[e.message] || e.message,
      constraint: e.constraint,
      toString: function() {
        return this.name + (this.message && ': ') + this.message;
      }
    };
  };

  // getUserMedia constraints shim.
  var getUserMedia_ = function(constraints, onSuccess, onError) {
    var constraintsToFF37_ = function(c) {
      if (typeof c !== 'object' || c.require) {
        return c;
      }
      var require = [];
      Object.keys(c).forEach(function(key) {
        if (key === 'require' || key === 'advanced' || key === 'mediaSource') {
          return;
        }
        var r = c[key] = (typeof c[key] === 'object') ?
            c[key] : {ideal: c[key]};
        if (r.min !== undefined ||
            r.max !== undefined || r.exact !== undefined) {
          require.push(key);
        }
        if (r.exact !== undefined) {
          if (typeof r.exact === 'number') {
            r. min = r.max = r.exact;
          } else {
            c[key] = r.exact;
          }
          delete r.exact;
        }
        if (r.ideal !== undefined) {
          c.advanced = c.advanced || [];
          var oc = {};
          if (typeof r.ideal === 'number') {
            oc[key] = {min: r.ideal, max: r.ideal};
          } else {
            oc[key] = r.ideal;
          }
          c.advanced.push(oc);
          delete r.ideal;
          if (!Object.keys(r).length) {
            delete c[key];
          }
        }
      });
      if (require.length) {
        c.require = require;
      }
      return c;
    };
    constraints = JSON.parse(JSON.stringify(constraints));
    if (browserDetails.version < 38) {
      logging('spec: ' + JSON.stringify(constraints));
      if (constraints.audio) {
        constraints.audio = constraintsToFF37_(constraints.audio);
      }
      if (constraints.video) {
        constraints.video = constraintsToFF37_(constraints.video);
      }
      logging('ff37: ' + JSON.stringify(constraints));
    }
    return navigator.mozGetUserMedia(constraints, onSuccess, function(e) {
      onError(shimError_(e));
    });
  };

  // Returns the result of getUserMedia as a Promise.
  var getUserMediaPromise_ = function(constraints) {
    return new Promise(function(resolve, reject) {
      getUserMedia_(constraints, resolve, reject);
    });
  };

  // Shim for mediaDevices on older versions.
  if (!navigator.mediaDevices) {
    navigator.mediaDevices = {getUserMedia: getUserMediaPromise_,
      addEventListener: function() { },
      removeEventListener: function() { }
    };
  }
  navigator.mediaDevices.enumerateDevices =
      navigator.mediaDevices.enumerateDevices || function() {
        return new Promise(function(resolve) {
          var infos = [
            {kind: 'audioinput', deviceId: 'default', label: '', groupId: ''},
            {kind: 'videoinput', deviceId: 'default', label: '', groupId: ''}
          ];
          resolve(infos);
        });
      };

  if (browserDetails.version < 41) {
    // Work around http://bugzil.la/1169665
    var orgEnumerateDevices =
        navigator.mediaDevices.enumerateDevices.bind(navigator.mediaDevices);
    navigator.mediaDevices.enumerateDevices = function() {
      return orgEnumerateDevices().then(undefined, function(e) {
        if (e.name === 'NotFoundError') {
          return [];
        }
        throw e;
      });
    };
  }
  if (browserDetails.version < 49) {
    var origGetUserMedia = navigator.mediaDevices.getUserMedia.
        bind(navigator.mediaDevices);
    navigator.mediaDevices.getUserMedia = function(c) {
      return origGetUserMedia(c).catch(function(e) {
        return Promise.reject(shimError_(e));
      });
    };
  }
  navigator.getUserMedia = function(constraints, onSuccess, onError) {
    if (browserDetails.version < 44) {
      return getUserMedia_(constraints, onSuccess, onError);
    }
    // Replace Firefox 44+'s deprecation warning with unprefixed version.
    console.warn('navigator.getUserMedia has been replaced by ' +
                 'navigator.mediaDevices.getUserMedia');
    navigator.mediaDevices.getUserMedia(constraints).then(onSuccess, onError);
  };
};

},{"../utils":28}],27:[function(require,module,exports){
/*
 *  Copyright (c) 2016 The WebRTC project authors. All Rights Reserved.
 *
 *  Use of this source code is governed by a BSD-style license
 *  that can be found in the LICENSE file in the root of the source
 *  tree.
 */
'use strict';
var safariShim = {
  // TODO: DrAlex, should be here, double check against LayoutTests
  // shimOnTrack: function() { },

  // TODO: DrAlex
  // attachMediaStream: function(element, stream) { },
  // reattachMediaStream: function(to, from) { },

  // TODO: once the back-end for the mac port is done, add.
  // TODO: check for webkitGTK+
  // shimPeerConnection: function() { },

  shimGetUserMedia: function() {
    navigator.getUserMedia = navigator.webkitGetUserMedia;
  }
};

// Expose public methods.
module.exports = {
  shimGetUserMedia: safariShim.shimGetUserMedia
  // TODO
  // shimOnTrack: safariShim.shimOnTrack,
  // shimPeerConnection: safariShim.shimPeerConnection,
  // attachMediaStream: safariShim.attachMediaStream,
  // reattachMediaStream: safariShim.reattachMediaStream
};

},{}],28:[function(require,module,exports){
/*
 *  Copyright (c) 2016 The WebRTC project authors. All Rights Reserved.
 *
 *  Use of this source code is governed by a BSD-style license
 *  that can be found in the LICENSE file in the root of the source
 *  tree.
 */
 /* eslint-env node */
'use strict';

var logDisabled_ = true;

// Utility methods.
var utils = {
  disableLog: function(bool) {
    if (typeof bool !== 'boolean') {
      return new Error('Argument type: ' + typeof bool +
          '. Please use a boolean.');
    }
    logDisabled_ = bool;
    return (bool) ? 'adapter.js logging disabled' :
        'adapter.js logging enabled';
  },

  log: function() {
    if (typeof window === 'object') {
      if (logDisabled_) {
        return;
      }
      if (typeof console !== 'undefined' && typeof console.log === 'function') {
        console.log.apply(console, arguments);
      }
    }
  },

  /**
   * Extract browser version out of the provided user agent string.
   *
   * @param {!string} uastring userAgent string.
   * @param {!string} expr Regular expression used as match criteria.
   * @param {!number} pos position in the version string to be returned.
   * @return {!number} browser version.
   */
  extractVersion: function(uastring, expr, pos) {
    var match = uastring.match(expr);
    return match && match.length >= pos && parseInt(match[pos], 10);
  },

  /**
   * Browser detector.
   *
   * @return {object} result containing browser, version and minVersion
   *     properties.
   */
  detectBrowser: function() {
    // Returned result object.
    var result = {};
    result.browser = null;
    result.version = null;
    result.minVersion = null;

    // Fail early if it's not a browser
    if (typeof window === 'undefined' || !window.navigator) {
      result.browser = 'Not a browser.';
      return result;
    }

    // Firefox.
    if (navigator.mozGetUserMedia) {
      result.browser = 'firefox';
      result.version = this.extractVersion(navigator.userAgent,
          /Firefox\/([0-9]+)\./, 1);
      result.minVersion = 31;

    // all webkit-based browsers
    } else if (navigator.webkitGetUserMedia) {
      // Chrome, Chromium, Webview, Opera, all use the chrome shim for now
      if (window.webkitRTCPeerConnection) {
        result.browser = 'chrome';
        result.version = this.extractVersion(navigator.userAgent,
          /Chrom(e|ium)\/([0-9]+)\./, 2);
        result.minVersion = 38;

      // Safari or unknown webkit-based
      // for the time being Safari has support for MediaStreams but not webRTC
      } else {
        // Safari UA substrings of interest for reference:
        // - webkit version:           AppleWebKit/602.1.25 (also used in Op,Cr)
        // - safari UI version:        Version/9.0.3 (unique to Safari)
        // - safari UI webkit version: Safari/601.4.4 (also used in Op,Cr)
        //
        // if the webkit version and safari UI webkit versions are equals,
        // ... this is a stable version.
        //
        // only the internal webkit version is important today to know if
        // media streams are supported
        //
        if (navigator.userAgent.match(/Version\/(\d+).(\d+)/)) {
          result.browser = 'safari';
          result.version = this.extractVersion(navigator.userAgent,
            /AppleWebKit\/([0-9]+)\./, 1);
          result.minVersion = 602;

        // unknown webkit-based browser
        } else {
          result.browser = 'Unsupported webkit-based browser ' +
              'with GUM support but no WebRTC support.';
          return result;
        }
      }

    // Edge.
    } else if (navigator.mediaDevices &&
        navigator.userAgent.match(/Edge\/(\d+).(\d+)$/)) {
      result.browser = 'edge';
      result.version = this.extractVersion(navigator.userAgent,
          /Edge\/(\d+).(\d+)$/, 2);
      result.minVersion = 10547;

    // Default fallthrough: not supported.
    } else {
      result.browser = 'Not a supported browser.';
      return result;
    }

    // Warn if version is less than minVersion.
    if (result.version < result.minVersion) {
      utils.log('Browser: ' + result.browser + ' Version: ' + result.version +
          ' < minimum supported version: ' + result.minVersion +
          '\n some things might not work!');
    }

    return result;
  }
};

// Export.
module.exports = {
  log: utils.log,
  disableLog: utils.disableLog,
  browserDetails: utils.detectBrowser(),
  extractVersion: utils.extractVersion
};

},{}]},{},[2]);
