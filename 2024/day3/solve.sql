create temporary table day3 (data varchar);

\copy day3 from '/Users/xetera/advent-of-code/2024/day3/input.txt';

-- PART 1 --
select regexp_matches(data, 'mul\((\d+),(\d+)\)', 'g') as tup, sum(tup[1]::int * tup[2]::int) from day3;

-- PART 2 --
drop function day3_iterate;
create or replace function day3_iterate() returns setof int as $$
declare
  enabled boolean = true;
  matching text[];
  a int;
  b int;
begin
  for matching in
    select regexp_matches(data, '(mul\((\d+),(\d+)\))|(do\(\))|(don''t\(\))', 'g') from day3
  loop
    a := matching[2]::int;
    b := matching[3]::int;
    case
      when matching[4] is not null then enabled = true;
      when matching[5] is not null then enabled = false;
      else null;
    end case;
    if enabled and a is not null and b is not null then
      return next (a * b);
    else
      return next 0;
    end if;
  end loop;
  return;
end; $$ language plpgsql;

explain analyze select sum(t.a) from (select * from day3_iterate()) as t(a);
