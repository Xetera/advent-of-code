import { promises as fs } from "fs";
import { EOL } from "os";
import { countBy } from "lodash";

const numLength = 12;

function bitIsOne(num: number, i: number) {
  const mask = 0b1;
  const shifted = num >> i;
  return (shifted & mask) == mask;
}

function countNums(nums: number[], i: number): [number, number] {
  const out = countBy(nums, (num) => bitIsOne(num, i));
  // Pls lodash I just want a mix of partition + countBy
  return [out["true"], out["false"]];
}

function solvePartTwo(
  nums: number[],
  f: (ones: number, twos: number) => boolean
): number {
  let pool = nums;
  for (let i = numLength; i > 0; i--) {
    const [ones, zeros] = countNums(pool, i - 1);
    pool = pool.filter((num) => {
      if (f(ones, zeros)) {
        return bitIsOne(num, i - 1);
      } else {
        return !bitIsOne(num, i - 1);
      }
    });
    if (pool.length === 1) {
      return pool[0];
    }
  }
  throw Error("Not possible?");
}

async function main() {
  const nums = (await fs.readFile("./inputs/day3.txt"))
    .toString()
    .split(EOL)
    .map((e) => parseInt(e, 2));
  let gamma = 0;
  for (let i = 0; i < numLength; i++) {
    const [one, zero] = countNums(nums, i);
    gamma += one > zero ? 1 << i : 0;
  }
  const epsilon = 0b111111111111 ^ gamma;
  const partOne = gamma * epsilon;
  console.log(partOne);
  const oxygen = solvePartTwo(nums, (ones, zeros) => {
    return ones >= zeros;
  });
  const co2 = solvePartTwo(nums, (ones, zeros) => {
    return ones < zeros;
  });
  const partTwo = co2 * oxygen;
  console.log(partTwo);
}

main();
