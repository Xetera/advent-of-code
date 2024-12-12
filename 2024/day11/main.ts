import { LRUCache } from "npm:lru-cache";

type Stone = bigint;
const cache = new LRUCache<string, bigint>({
  max: 1_000_000,
});

function* next(stone: Stone): Generator<Stone> {
  if (stone === 0n) {
    return yield 1n;
  }
  const digitsCount = BigInt(stone.toString(10).length);
  if (digitsCount % 2n === 0n) {
    const divisor = 10n ** (digitsCount / 2n);
    yield stone / divisor;
    yield stone % divisor;
    return;
  }
  return yield stone * 2024n;
}

const iterate = (stone: Stone, remaining: number): bigint => {
  if (remaining === 0) {
    return 1n;
  }
  const cacheKey = `${stone}-${remaining}`;
  const existing = cache.get(cacheKey);
  if (existing) {
    return existing;
  }
  const output = next(stone).reduce(
    (acc, nextStone) => {
      return iterate(nextStone, remaining - 1) + acc;
    },
    0n,
  );
  cache.set(cacheKey, output);
  return output;
};

const solve = (puzzle: Stone[], n: number) =>
  puzzle.reduce((acc, piece) => iterate(piece, n) + acc, 0n);
const bytes = await Deno.readTextFile("input.txt");
const puzzle: Stone[] = bytes.split(" ").map(BigInt);
const time = Date.now();
console.log(solve(puzzle, 25));
console.log(solve(puzzle, 75));
