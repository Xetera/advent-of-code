--- DAY 2 ---
create temporary table day2 (a int2, b int2, c int2, d int2, e int2, f int2, g int2, h int2);
\copy day2(a,b,c,d,e,f,g,h) FROM '/Users/xetera/advent-of-code/2024/day2/clean_data.txt' (format csv, delimiter ' ', null 'NULL');

-- PART 1
create or replace function day2_is_passing(a int2, b int2, c int2, d int2, e int2, f int2, g int2, h int2) returns boolean as $$
begin
  return ((abs(a - b) between 1 and 3)
  and (c is null or abs(b - c) between 1 and 3)
  and (d is null or abs(c - d) between 1 and 3)
  and (e is null or abs(d - e) between 1 and 3)
  and (f is null or abs(e - f) between 1 and 3)
  and (g is null or abs(f - g) between 1 and 3)
  and (h is null or abs(g - h) between 1 and 3)
  and ((a > b and b > c and (d is null or c > d) and (e is null or d > e) and (f is null or e > f) and (g is null or f > g) and (h is null or g > h))
    or (a < b and b < c and (d is null or c < d) and (e is null or d < e) and (f is null or e < f) and (g is null or f < g) and (h is null or g < h))));
end;
$$ language plpgsql;

-- PART 1 SOLUTION
select count(*) part1 from day2 where day2_is_passing(a,b,c,d,e,f,g,h);

-- PART 2 SOLUTION
select count(*) as part2 from day2 where (
  day2_is_passing(a, b, c, d, e, f, g, h)
  or day2_is_passing(b, c, d, e, f, g, h, NULL)
  or day2_is_passing(a, c, d, e, f, g, h, NULL)
  or day2_is_passing(a, b, d, e, f, g, h, NULL)
  or day2_is_passing(a, b, c, e, f, g, h, NULL)
  or day2_is_passing(a, b, c, d, f, g, h, NULL)
  or day2_is_passing(a, b, c, d, e, g, h, NULL)
  or day2_is_passing(a, b, c, d, e, f, h, NULL)
  or day2_is_passing(a, b, c, d, e, f, g, NULL)
);
