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