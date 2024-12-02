awk '{for (i=NF+1; i<=8; i++) $i="NULL"} 1' OFS=' ' input.txt > clean_data.txt

