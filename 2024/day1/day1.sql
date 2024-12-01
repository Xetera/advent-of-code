create table day1(first integer, second integer);
copy day1 FROM '<<INPUT>>' CSV;

-- Part 1
with s1 as (
  select row_number() over (order by first) as join_key, first from day1
), s2 as (
  select row_number() over (order by second) as join_key, second from day1
)
select sum(abs(s1.first - s2.second))
  from s1
join s2 on s1.join_key = s2.join_key;

-- Part 2
with s2 as (
  select second, count(second) as cnt from day1 group by second
)
select sum(s1.first * s2.cnt)
  from day1 s1
join s2 on s1.first = s2.second;