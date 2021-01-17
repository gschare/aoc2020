#include <stdio.h>
#include <stdlib.h>

#define HASHSIZE 1000000

struct table {
    struct table *next;
    int val;
    int age;
};

static struct table *hashtab[HASHSIZE];

unsigned hash(int val) {
    unsigned hashval = val;
    return hashval % HASHSIZE;
}

struct table *lookup(int val) {
    struct table *np;
    for (np = hashtab[hash(val)]; np != NULL; np = np->next) {
        if (val == np->val) return np;
    }
    return NULL;  // Not found.
}

struct table *insert(int val, int age) {
    struct table *np;
    unsigned hashval;
    if ((np = lookup(val)) == NULL) {
        np = (struct table *) malloc(sizeof(*np));
        if (np == NULL) {
            return NULL;
        }
        np->val = val;
        hashval = hash(val);
        np->next = hashtab[hashval];
        hashtab[hashval] = np;
    } else {
        free((void *) np->age);
    }
    np->age = age;
    return np;
}

int next(int prev, int turn) {
    struct table *np = lookup(prev);
    if (np == NULL) {
        insert(prev, -turn + 1);
        return 0;
    } else {
        int result = np->age + turn - 1;
        np->age = -turn + 1;
        return result;
    }
}

int main(int argc, char *argv[]) {

    if (argc < 2) {
        printf("Not enough arguments. Correct usage: ./part2 <TIMES> a1 a2 a3 ...\n");
        return 1;
    }

    int times = atoi(argv[1]);

    for (int i = 2; i < argc; i++) {
        insert(atoi(argv[i]), 2 - i);
        printf("Turn %i: %i\n", i - 1, atoi(argv[i]));
    }

    int prev = atoi(argv[argc - 1]);
    int n;
    for (n = argc - 2; n < times; n++) {
        prev = next(prev, n);
        //if (n % 100000 == 0) printf("Turn %i: %i\n", n+1, prev);
    }

    printf("Turn %i: %i\n", n, prev);

    return 0;
}
