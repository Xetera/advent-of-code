const { readFileSync } = require('fs');
const moment = require('moment')

const lines = readFileSync('inputs/day4.txt').toString().split('\n').sort();
const parts = lines.map(line => {
	// literally disgusting
	const [_, years, months, days, hours, minutes, yeet, id] = line.match(/\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (Guard #(\d+))?/);
	const waking = line.includes('wake');
	return {
		years,
		months,
		days,
		hours,
		minutes,
		id,
		// i hate myself
		status:  id ? 'switch' : waking ? 'wake' : 'sleep'
	}
});

const counts = parts.reduce(({ prevs, last }, guard, i) => {
	const current = guard.id
		? { id: guard.id, sleeps: [], total: 0, sleep: 0 }
		: last;

	if (guard.status === 'sleep') {
		current.sleeps.push(guard.minutes);
		current.sleep = guard.minutes;
	}
	else if (guard.status === 'wake') {
		current.total += guard.minutes - current.sleep;
	}
	prevs[current.id] = current;
	return { prevs, last: current }
}, { prevs: {}, current: { } });

// console.log(lines.sort())
console.log(counts)
