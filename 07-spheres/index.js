const glsl = require('glslify');
const regl = require('regl')()
const camera = require('regl-camera')(regl, { 
	distance: 1.5,
	theta: Math.PI * .3,
	mouse: true,
	center: [0, 0, 0]
})
const icosphere = require('icosphere');

const sphere = require('./sphere');

const dim = 5;
// const spheres = Array(dim).fill(Array(dim).fill(Array(dim).fill(0)))
// 	.map((x, i) => x.map((y, j) => y.map((_, k) => {
// 		return sphere(regl, .2, 3, [i - dim / 2, j - dim / 2, k - dim / 2])
// 	})))
// 	.reduce((a, b) => a.concat(b))
// 	.reduce((a, b) => a.concat(b))
const spheres = icosphere(1).positions.map(pos => sphere(regl, .05, 2, pos));

const drawSphere = sphere(regl, .5, 2, [0, 0, 0]);

require('regl-audio/microphone')({
  regl,
  beats: 16,
  name: '',
  maxPitch: 1000,
  minPitch: 500,
  pitches: 400,
  // beatThreshold: .1,
  done: (microphone) => {
    regl.frame(() => {	
			microphone(({ freqTexture, timeTexture, beats, pitches, volume, cepstrum }) => {
				camera(() => {
					regl.clear({ color: [0, 0, 0, 1], depth: true })
          // drawSphere();
          spheres.forEach(thing => thing());
					// points,point,lines,line,triangles,triangle,line loop,line strip,triangle strip,triangle fan
				})
			})
		})
  }
})