import re
import string

lines = []
with open('Day2string.txt') as fp:
    for line in fp:

        lines.append(line.split('\t'))

for i in lines:
    if '\n' in i:
        lines[lines.index(i)] = "replacement string"

print(lines)
total = []
start.replace('\t', '')

print(start)
everything = start.split('\t')
for i in everything:
    total.append([i for i in everything])
    if '\n' in i:
        print(i)

for i in row:
    temp = []
    for char in i:
        if char == " ":
            continue
        temp.append(int(char))

    total.append(max(temp) - min(temp))


print(total)
# total.append(sum([int(num) for num in i if num != " "]))
# sexy code