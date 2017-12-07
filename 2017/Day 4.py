from collections import Counter
from input.day_4_input import source

lines = source.split('\n')
print(len(lines))
everything = []
for x,i in enumerate(lines):
    phrase = i.split()
    everything.append(phrase)
    if x > 4:
        break

new = []
duplicates = []
for line in everything:
    for word in line:
        row = []
        new.append(row)
        new_word = ("".join(sorted(word)))
        row.append(new_word)

for i in new:
    duplicates.append([k for k, v in Counter(i).items() if v > 1])
print(duplicates)
print(new)
# works!
