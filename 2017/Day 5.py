with open('input/day5', 'r+') as fp:
    for i, line in enumerate(fp):
        # i helps us keep track of what line we're on
        number = int(line.strip())
        print(str(number))
        fp.write(line.replace(line, str(number + 1)))
        break