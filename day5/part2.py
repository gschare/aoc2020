# Ideally I should work directly with binary numbers.
# Effectively we have 7 bit rows and 3 bit columns.
# We want to translate F and L into 0s and B and R into 1s.
# Then just read off the two resulting binary numbers in decimal.

# The efficient way to do this would be:
# Prepare a null byte.
# For each of the 7 bits, prepare a mask with only that bit turned on.
# For each of those masks, bitwise AND it with 0 for F/L and 127 (or whatever the all-1's thing is) for B/R.
# Bitwise OR all the masks together.
# Read that number.

import sys

ROW_BITS = 7
COL_BITS = 3

MAX_ROW = pow(2, ROW_BITS) - 1
MAX_COL = pow(2, COL_BITS) - 1

def char_to_binary(c, i):
    bit = {'F':0, 'L':0, 'B':MAX_ROW, 'R':MAX_COL}.get(c)
    if bit == None:
        raise Exception(f"invalid passport encoding at bit {i}: {c}")
    return pow(2, i) & bit

def the_fun_way(p):
    row_string = p[:ROW_BITS]
    col_string = p[ROW_BITS:ROW_BITS+COL_BITS]

    row = 0
    for i, c in enumerate(row_string):
        bit_index = ROW_BITS - i - 1
        row |= char_to_binary(c, bit_index)

    col = 0
    for i, c in enumerate(col_string):
        bit_index = COL_BITS - i - 1
        col |= char_to_binary(c, bit_index)

    return row, col

def the_lazy_way(p):
    transtable = str.maketrans("FBLR", "0101")

    row_string = p[:ROW_BITS]
    col_string = p[ROW_BITS:ROW_BITS+COL_BITS]

    row = int(row_string.translate(transtable), 2)
    col = int(col_string.translate(transtable), 2)

    return row, col

def the_stupid_way(p):
    row_string = p[:ROW_BITS]
    col_string = p[ROW_BITS:ROW_BITS+COL_BITS]

    row = 0
    for i, c in enumerate(row_string):
        row += c == 'B' and pow(2, ROW_BITS - i - 1)

    col = 0
    for i, c in enumerate(col_string):
        col += c == 'R' and pow(2, COL_BITS - i - 1)

    return row, col

def main():
    if len(sys.argv) != 2:
        print("missing required argument: input file")
        return

    with open(sys.argv[1], 'r') as fp:
        passes = fp.readlines()

    ids = [row * 8 + col for row,col in [the_fun_way(p) for p in passes]]
    max_id = max(ids)
    missing = [x for x in range(max_id) if x not in ids]
    print(missing)
    print([x for x in missing if x+1 not in missing and x-1 not in missing])

if __name__ == "__main__":
    main()
