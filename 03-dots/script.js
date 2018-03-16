let acc;
let radius = 300;

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

function generateSpiral(rotations, detail, radius) {
  const theta = i => rotations * (i / detail) * 2 * Math.PI;
  const step = i => i / detail * radius;
  return Array.from(new Array(detail))
    .map((_, i) => rotate2D(theta(i), { x: 0, y: step(i), z: i * .1 }))
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

function generateSphere(r, points) {
  const theta = i => (i / points) * 2 * Math.PI;
  return Array.from(new Array(points))
    .map((_, i) => 
      generateCircle(r, points).map(vec => rotate3D([0, theta(i), 0], vec))
    );
}

function generateSuperellipse(radius, theta, detail) {
  const n = ((Math.sin(theta) + 1) / 2) * 10 + .1
  const a = radius;
  const b = a;
  const angle = i => (i / detail) * 2 * Math.PI;
  return Array.from(new Array(detail))
    .map((_, i) => ({
      x: Math.pow(Math.abs(Math.cos(angle(i))), 2 / n) * a * sgn(Math.cos(angle(i))),
      y: Math.pow(Math.abs(Math.sin(angle(i))), 2 / n) * b * sgn(Math.sin(angle(i))),
      z: Math.pow(Math.abs(Math.cos(angle(i))), 2 / n) * a * sgn(Math.cos(angle(i)))
    }))
}

function generateGrid(detail, width, height) {
  return Array.from(new Array(detail))
    .map((_, i) => 
      Array.from(new Array(detail)).map((_2, i2) => 
        ({ 
          x: (width / detail * (i2 + .5)) - width / 2,
          y: (height / detail * (i + .5)) - height / 2
        })
      )
    ).reduce((a, b) => [].concat(a, b))
}


let mic, fft;
let shape;
let line;
let settings = {
}

function setup() {
  createCanvas(windowWidth, windowHeight);
  // background(color(0, 0, 255));

  // Geometry
  shape = generateGrid(10, window.innerWidth, window.innerHeight);
  line = Array.from(new Array(1000))
          .map((_, i) => ({ x: (window.innerWidth / 1000) * i - (window.innerWidth / 2), y: 0}))

  // AUDIO
  mic = new p5.AudioIn()
  mic.start();
  fft = new p5.FFT(.7, 1024);
  fft.setInput(mic)

  // const gui = new dat.GUI();
}

function draw(){
  clear(window.innerWidth, window.innerHeight)
  background(20)
  push();
  translate(window.innerWidth / 2, window.innerHeight / 2)
  settings.acc += settings.accSpeed


  const spec = fft.analyze().map(x => x * .2);
  const wav = fft.waveform().map(x => x * 100);

  beginShape()
  line
    .map((p, i) => ({ x: p.x, y: p.y + wav[i % wav.length] }))
    .forEach(p => vertex(p.x, p.y))
  noFill();
  stroke(255, 204, 0)
  endShape()


  shape.map((p, i) => ellipse(p.x, p.y, spec[i % spec.length], spec[i % spec.length]))
  
  pop()

}
