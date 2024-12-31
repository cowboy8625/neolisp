#include <stdio.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void neolisp_printf(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vprintf(fmt, args);
    va_end(args);
}

const char* neolisp_input() {
    char buf[1024];
    scanf("%1023s", buf);
    char* result = malloc(strlen(buf) + 1);
    if (!result) {
        return NULL;
    }
    strcpy(result, buf);
    return result;
}

int add(int a, int b) {
    printf("%d + %d\n", a, b);
    return a + b;
}

int sub(int a, int b) {
    return a - b;
}

void testing_string(const char *str) {
    printf("%s\n", str);
}
