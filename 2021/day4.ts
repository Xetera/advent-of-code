import _ from "lodash";
import { promises as fs } from "fs";
import { EOL } from "node:os";

const BOARD_DIMENSION = 5;
type Board = number[][];

function readBoard(str: string): Board {
  return str
    .trim()
    .split(EOL)
    .map((line) => line.trim().replace(/ +/g, " ").split(" ").map(Number));
}

function* draw(nums: number[]) {
  let i = 1;
  while (true) {
    if (i > nums.length) {
      return;
    }
    yield nums.slice(0, i++);
  }
}

function* range(end: number) {
  for (let i = 0; i < end; ++i) {
    yield i;
  }
}

function checkBoard(board: Board, drawn: number[]): boolean {
  const horizontal = board.some((row) =>
    row.every((num) => drawn.includes(num))
  );
  if (horizontal) {
    return true;
  }
  const vertical = [...range(BOARD_DIMENSION)].some((i) =>
    [...range(BOARD_DIMENSION)].every((j) => drawn.includes(board[j][i]))
  );
  if (vertical) {
    return true;
  }
  return false;
}

const file = await fs.readFile("inputs/day4.txt", "utf-8");

const boards = file
  .slice(file.indexOf(EOL + EOL))
  .trim()
  .split(EOL + EOL)
  .map(readBoard);

const [numbersStr] = file.split(EOL, 1);
const numsPool = numbersStr.split(",").map(Number);

function findWinningBoard(
  boards: Board[],
  pool: number[]
): [Board, number[]] | undefined {
  for (const nums of draw(pool)) {
    for (const board of boards) {
      if (checkBoard(board, nums)) {
        return [board, nums];
      }
    }
  }
}

function findLastWinningBoard(
  boards: Board[],
  pool: number[]
): [Board, number[]] | undefined {
  const toIterate = [...range(boards.length)];
  for (const nums of draw(pool)) {
    for (const index of toIterate) {
      const board = boards[index];
      if (checkBoard(board, nums)) {
        _.pull(toIterate, index);
        if (toIterate.length === 0) {
          return [board, nums];
        }
      }
    }
  }
}

function sumUnmarked(board: Board, drawn: number[]): number {
  return _.sum(board.flat().filter((num) => !drawn.includes(num)));
}

function calculateResult(board: Board, drawn: number[]): number {
  const winning = drawn[drawn.length - 1];
  return sumUnmarked(board, drawn) * winning;
}

const first = findWinningBoard(boards, numsPool);
if (!first) {
  throw Error("wtf?");
}
const [board, pool] = first;
const partOne = calculateResult(board, pool);
console.log(partOne);

const second = findLastWinningBoard(boards, numsPool);
if (!second) {
  throw Error("wtf?");
}
const [lastBoard, lastPool] = second;
const partTwo = calculateResult(lastBoard, lastPool);
console.log(partTwo);
