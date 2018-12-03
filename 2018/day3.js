const {readFileSync} = require('fs')

const lines = readFileSync('inputs/day3.txt').toString().split('\n')

const toRect = line => {
  const [idThingy, rest] = line.split('@')
  const [_, id] = idThingy.split('');
  const [locations, dimensions] = rest.trim().split(':');
  const [x, y] = locations.trim().split(',');
  const [width, height] = dimensions.trim().split('x')
  return {
    x: Number(x),
    y: Number(y),
    width: Number(width),
    height: Number(height),
    id
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

const isLone =
    (rect) => {
      let count = 0;
      for (let i = rect.x; i < rect.x + rect.width; i++) {
        for (let j = rect.y; j < rect.y + rect.height; j++) {
          count += points[i][j];
        }
      }
      return count == rect.width * rect.height;
    }

const valid = rects.filter(isLone);

console.log(valid)