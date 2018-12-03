const {readFileSync} = require('fs')

const lines = readFileSync('inputs/day3.txt').toString().split('\n')

const toRect = line => {
  const [_, rest] = line.split('@')
  const [locations, dimensions] = rest.trim().split(':');
  const [x, y] = locations.trim().split(',');
  const [width, height] = dimensions.trim().split('x')
  return {
    x: Number(x),
    y: Number(y),
    width: Number(width),
    height: Number(height)
  };
};
const rects = lines.map(toRect);

const points = [...Array(1000)].map(() => [...Array(1000)].fill(0));

for (const rect of rects) {
  for (let i = rect.x; i < rect.x + rect.width; i++) {
    for (let j = rect.y; j < rect.y + rect.height; j++) {
      points[i][j] += 1;
    }
  }
}

const part1 =
    points.reduce((acc, row, y) => acc + row.reduce((prev, item, x) => {
      if (item > 1) {
        return prev + 1
      }
      return prev
    }, 0), 0);

console.log(part1)