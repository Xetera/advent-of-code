sed -e 's/   /,/g' day1.txt | tee day1-clean.txt


cat "day1.sql" | sed -e "s|<<INPUT>>|$PWD/day1-clean.txt|g" | psql postgres
