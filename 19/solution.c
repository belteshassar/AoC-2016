#include <stdio.h>
#include <math.h>


int part1 (int n) {
    int k;

    k = log2(n);
    return (1 + 2*n - 2*pow(2.0, k));
}


int part2(int n) {
    int w = 1;
    int i;

    for (i=1; i<n; i++) {
        w = w % i + 1;
        if (w > (i + 1)/2) {
            w++;
        }
    }
    return (w);
}


int main(int argc, char *argv[]) {
    int n;

    if (argc < 2) {
        printf ("error - expected an integer\n");
        return (-1);
    }

    if (sscanf (argv[1], "%i", &n)!=1) {
        printf ("error - not an integer\n");
        return (-1);
    }
    printf ("Answer part 1: %d\n", part1(n));
    printf ("Answer part 2: %d\n", part2(n));
    return (0);
}
