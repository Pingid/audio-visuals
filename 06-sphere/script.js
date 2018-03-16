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

function generateSphere(R, D) {
  return Array.from(new Array(D))
    .map((_, m) => 
      Array.from(new Array(D)).map((_, n) => ({
          x: sin(Math.PI * m/D) * cos(2 * Math.PI * n/D) * R, 
          y: sin(Math.PI * m/D) * sin(2 * Math.PI * n/D) * R, 
          z: cos(Math.PI * m/D) * R
      }))
    )
    // .reduce((a, b) => [].concat(a, b))
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

let time = 0;
let mic, fft, amplitude;
let shapes;
let mySound;
let settings = {
}

function preload() {
  // soundFormats('mp3', 'ogg');
  // mySound = loadSound('raumakustik-raider.mp3');
}


function setup() {
  createCanvas(windowWidth, windowHeight, WEBGL);
  // background(color(0, 0, 255));

  // Geometry
  shapes = {
    sphere: generateSphere(width/2, 100),
    circle: generateCircle(250, 40).map(x => rotate3D([0, 0, 0], x)),
    grid: generateGrid(30, window.innerWidth, window.innerHeight),
    line: Array.from(new Array(1000))
            .map((_, i) => ({ x: (window.innerWidth / 1000) * i - (window.innerWidth / 2), y: 0}))
  }   

  // AUDIO
  // mySound.play();

  mic = new p5.AudioIn()
  mic.start();

  fft = new p5.FFT(.7, 1024);
  fft.setInput(mic)

  amplitude = new p5.Amplitude();

}

function draw(){
  time += (1 / 60);
  rotateY(HALF_PI * sin(time))
  rotateX(HALF_PI * sin(time))

  // rotateY(mouseY / height)
  const spec = fft.analyze().map(x => x * .2);
  const wav = fft.waveform().map(x => x * 100);

  background(0)
  ambientLight(200 * sin(time), 200 * sin(time + 2), 200 * sin(time + 4));
  directionalLight(255,255,255,0,0,2)

  beginShape()
  const level = mic.getLevel()
  shapes.sphere.forEach((lat, m) => lat.forEach((p, n) => {
    const getNoise = n => n / 300 * .5 + time * .5 + level * .5;
    const nos = noise(getNoise(p.x), getNoise(p.y), getNoise(p.z));
    const w = 0
    // ambientMaterial(200, wav[(m * n) % wav.length] * 200, 100);
    vertex(p.x * nos + p.x * w, p.y * nos + p.y * w, p.z * nos + p.z * w)
  }))
  endShape()


}
