import sys
from functools import reduce
from operator import mul

N = 3

def combinations(k, S):
    n = len(S)
    if k>n:
        print("error: choose smaller k")
        return
    if n==0 or k==0:
        return []
    if k==1:
        return [[x] for x in S]
    result = []
    idx = list(range(k))
    while True:
        result.append([S[i] for i in idx])
        if idx[-1] < n-1:
            idx[-1] += 1
        else:
            j = k - 2
            while j >= 0:
                if idx[j] < idx[j+1] - 1:
                    idx[j] += 1
                    for i in range(j+1, k):
                        idx[i] = idx[j] + j - i + 1
                    break
                j -= 1
            if j < 0:
                break
    return result

def fcombs(n, xs):
    if n==0:
        return [[]]
    if xs==[]:
        return []
    return list(map(lambda ys: [xs[0]] + ys, fcombs(n-1, xs[1:]))) + fcombs(n, xs[1:])

def main():
    if (len(sys.argv) != 2):
        print("expected 1 argument: input file")
        return

    filename = sys.argv[1]
    with open(filename, "r") as fp:
        entries = [int(line[:-1]) for line in fp.readlines()]

    print([reduce(mul, L, 1) for L in combinations(N, entries) if sum(L)==2020])

if __name__ == "__main__":
    main()
