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