#include<math.h>
#include<stdio.h>
#include<stdlib.h>

char * fmt_itos(int i) {
    int length = ceil(log10(i)) + 1;
    char * s = malloc(length * sizeof(char));
    sprintf(s, "%d", i);

    return s;
}

void fmt_println(char * str) {
    printf("%s\n", str);
}
