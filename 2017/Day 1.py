number = 1111
numberList = [int(i) for i in str(number)]

matching_numbers = []
for i, number in enumerate(numberList):
    try:
        if number == numberList[i+1]:
            matching_numbers.append(number)
    except IndexError:
        if number == numberList[0]:
            matching_numbers.append(number)
print(sum(matching_numbers))

# Works!
