#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int checkLine(char *line) {
    int idx1;
    int idx2;
    char letter;
    char password[40];
    sscanf(line, "%i-%i %c: %s", &idx1, &idx2, &letter, password);
    ssize_t len = strlen(password);

    if (idx1 - 1 >= len || idx2 - 1 >= len) {
        printf("Index out of bounds");
        exit(1);
    }
    return ((password[idx1-1]==letter) ^ (password[idx2-1]==letter));
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        return 1;
    }

    int valid = 0;
    int total = 0;

    FILE *fp;
    char *line = NULL;
    size_t linecap = 0;
    ssize_t linelen;

    fp = fopen(argv[1], "r");
    if (fp == NULL) {
        return 1;
    }

    while ((linelen = getline(&line, &linecap, fp)) != -1) {
        total++;
        valid += checkLine(line);
    }

    free(line);
    fclose(fp);

    printf("Valid passwords: %i/%i\n", valid, total);

    return 0;
}
