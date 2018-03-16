function sgn(val) {
  if (val > 0) return 1;
  if (val < 0) return -1;
  return 0;   
}

const rotate2D = (theta, vec3) =>({
  x: vec3.x * Math.cos(theta) - vec3.y * Math.sin(theta),
  y: vec3.y * Math.cos(theta) + vec3.x * Math.sin(theta),
  z: vec3.z  
});

const rotate3D = (rotations, vec3) => {
  return rotations.reduce(({ x, y, z }, theta, index) => {
    const { s0, c0 } = { s0: Math.sin(theta), c0: Math.cos(theta) }
    switch (index) {
      case 0: return { x: x, y: y * c0 - z * s0, z: z * c0 + y * s0 };
      case 1: return { x: x * c0 - z * s0, y: y, z: z * c0 + x * s0 };
      case 2: return { x: x * c0 - y * s0, y: y * c0 + x * s0, z: z };
      default: return p
    }
  }, vec3)
}

function generateCircle(r, points) {
  const { x, y } = { x: 0, y: r };
  const theta = i => (i / points) * 2 * Math.PI;
  return Array.from(new Array(points))
    .map((_, i) => ({ 
      x: x * Math.cos(theta(i)) - y * Math.sin(theta(i)),
      y: y * Math.cos(theta(i)) + x * Math.sin(theta(i)),
      z: 0
    }))
}

let mic, fft;
let shape;
let line;
let settings = {
  time: 0
}

function setup() {
  createCanvas(windowWidth, windowHeight);
  // background(color(0, 0, 255));

  // AUDIO
  mic = new p5.AudioIn()
  mic.start();
  fft = new p5.FFT(.7, 1024);
  fft.setInput(mic)

  // const gui = new dat.GUI();
}

function draw(){
  settings.time += (1 / 60);
  clear(window.innerWidth, window.innerHeight)
  background(20)
  push();
  translate(window.innerWidth / 2, window.innerHeight / 2)

  const spec = fft.analyze().map(x => x * .2);
  const wav = fft.waveform().map(x => x * 100);
  if (mouseIsPressed) { console.log(spec, wav )}

  noFill()
  stroke(color(100, 100, 255))
  strokeWeight(2)
  const circleDetail = 300
  for (let j = 0; j < 10; j++) {
    beginShape()
    generateCircle(spec[j] * spec[j], circleDetail)
    .map(({ x, y }, i) => ({ 
      x: x + x * (wav[i] * .01), 
      y: y + x * ((wav[i] * .01) * y / x)
    }))
    // .map(p => { ellipse(p.x, p.y, 10, 10); return p })
    .reduce((a, b, i) => {
      stroke(color(255, cos(settings.time) * 255, 255))
      strokeWeight(10)
      vertex(b.x, b.y);
      if (i === (circleDetail - 1)) { vertex(a[0].x, a[0].y) }
      return [].concat(a, [b]);
    }, [])
    endShape()
  }
  beginShape()
    generateCircle(300, circleDetail)
    .map(({ x, y }, i) => ({ 
      x: x + x * (wav[i] * .01), 
      y: y + x * ((wav[i] * .01) * y / x)
    }))
    // .map(p => { ellipse(p.x, p.y, 10, 10); return p })
    .reduce((a, b, i) => {
      vertex(b.x, b.y);
      if (i === (circleDetail - 1)) { vertex(a[0].x, a[0].y) }
      return [].concat(a, [b]);
    }, [])
  endShape()
    // .forEach((p, i) => vertex(p.x, p.y))

  pop()

}
