import sys

def main():
    if (len(sys.argv) != 2):
        print("expected 1 argument: input file")
        return

    filename = sys.argv[1]
    with open(filename, "r") as fp:
        entries = [int(line[:-1]) for line in fp.readlines()]

    for x in entries:
        for y in entries:
            if x == y:
                continue
            if x + y == 2020:
                print(x * y)
                return

if __name__ == "__main__":
    main()
