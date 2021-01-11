#include <stdio.h>
#include <stdlib.h>

int checkLine(char *line) {
    int min;
    int max;
    char letter;
    char password[40];
    sscanf(line, "%i-%i %c: %s", &min, &max, &letter, password);

    int count = 0;
    for (int i=0; password[i]!='\0'; i++) {
        if (password[i] == letter) {
            if (++count > max) {
                printf("%s: too many %c (needed %i-%i, got %i)\n", password, letter, min, max, count);
                return 0;
            }
        }
    }
    if (count < min) {
        printf("%s: too few %c (needed %i-%i, got %i)\n", password, letter, min, max, count);
        return 0;
    }
    printf("%s: valid %c (needed %i-%i, got %i)\n", password, letter, min, max, count);
    return 1;
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
