const glsl = require('glslify');
const regl = require('regl')()
const camera = require('regl-camera')(regl, { 
	distance: 1.5,
	theta: Math.PI * .5,
	mouse: true,
	center: [0, 0, 0]
})

const sphere = require('./sphere');

const drawSphere = sphere(regl, .5, 4, [0, 0, 0]);

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
          drawSphere();
					// points,point,lines,line,triangles,triangle,line loop,line strip,triangle strip,triangle fan
				})
			})
		})
  }
})