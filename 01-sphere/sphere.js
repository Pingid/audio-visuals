const glslify = require('glslify');
const icosphere = require('icosphere');

module.exports = (regl, size, detail, center) => {
	const maxPitch = 1000;
	const sphere = icosphere(detail);
	const extend = arr => Array(sphere.positions.length).fill(1)
		.map((x, i) => arr[i % arr.length]);	
	return regl({
		vert: glslify`
		  precision mediump float;

		  attribute float que, pitch, freq;
		  attribute vec3 pos;

			uniform mat4 projection, view;
		  uniform float beats[16];
		  uniform float tick, volume;

		  varying vec3 vpos;
		  varying float vpitch, vfreq, intensity;

		  #pragma glslify: snoise4 = require(glsl-noise/simplex/4d);

		  mat4 rotationMatrix(vec3 axis, float angle)
			{
			    axis = normalize(axis);
			    float s = sin(angle);
			    float c = cos(angle);
			    float oc = 1.0 - c;
			    
			    return mat4(oc * axis.x * axis.x + c,           oc * axis.x * axis.y - axis.z * s,  oc * axis.z * axis.x + axis.y * s,  0.0,
			                oc * axis.x * axis.y + axis.z * s,  oc * axis.y * axis.y + c,           oc * axis.y * axis.z - axis.x * s,  0.0,
			                oc * axis.z * axis.x - axis.y * s,  oc * axis.y * axis.z + axis.x * s,  oc * axis.z * axis.z + c,           0.0,
			                0.0,                                0.0,                                0.0,                                1.0);
			}

		  void main () {
		    vpos = pos;

		    intensity = 0.0;
				float bin = floor(8.0 * (1.0 + pos.x));
		    for (int i = 0; i < 16; ++i) { intensity += beats[i]; }

		    gl_PointSize = 4.0;

		  	// vec3 rotation = rotate(vpos, vec3(1,1,1), 0.1 * tick);
		  	vec3 noise = vpos * snoise4(vec4(vpos, tick * 0.1) * 0.2) * freq;
		  	vec3 intesDist = vpos * intensity * 0.01;
		  	vec3 freqDist = vpos * freq * 0.5;
		  	vec3 pitchDist = vpos * vpitch * 0.1;

		  	mat4 trans = getMatrix('translate(40px, 20px)');

		    gl_Position = trans * projection * view * vec4(vpos * 0.5 + freqDist + intesDist, 1.0);
		  }
	  `,

	  frag: glslify`
	    precision mediump float;
	    uniform vec4 pitches;
	    uniform float tick;

	    varying vec3 vpos;
	    varying float vpitch, vfreq, intensity;

	    #pragma glslify: snoise4 = require(glsl-noise/simplex/4d);

	    void main () {
	    	vec3 color = vec3(.2 - abs(vpos * 1.5).x, .2 - abs(vpos * 1.5).y, 1.0 * sin(tick * 0.001));
	    	// vec3 color = vec3(0,0,0);
	    	vec3 noise = color * snoise4(vec4(color * 3.0, tick * 0.01));
	      gl_FragColor = vec4(color * 5.0 + noise, 1);
	    }
	  `,

	  attributes: {
	  	pos: sphere.positions
	  		.map(([x, y, z]) => ([x * size, y * size, z * size])),
	    que: ({ cepstrum }) => new Float32Array(extend(cepstrum)),
	    pitch: ({ pitches }) => new Float32Array(extend(pitches.map(x => x / maxPitch))),
	    freq: ({ freq }) => new Float32Array(extend(freq.map(x => x / 100)))
	  },
	  uniforms: {
	    pitches: ({ pitches }) => pitches.slice(0, 4).map(x => x / maxPitch),
	    tick: regl.context('tick'),
	    volume: regl.context('volume'),
	  },
	  elements: sphere.cells,
	  primitive: 'points'
	})
}