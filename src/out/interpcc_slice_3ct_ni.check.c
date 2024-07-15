/*****************************************
Emitting C Generated Code
*******************************************/
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
/************* Function Declarations **************/
void x1(int*);
void x6(int*);
/************* Functions **************/
void x1(int* x2) {
  int x3 = x2[6] + 1;
  x2[6] = x3;
  int x4 = x2[0] + 10;
  x2[2] = x2[4] == x4 ? x2[5] : ({
    int x5 = x2[x4];
    x2[6] = x3 + 1 + 1;
    x2[4] = x4;
    x2[5] = x5;
    x5;
  });
}
void x6(int* x7) {
  int x8 = x7[6] + 1;
  x7[6] = x8;
  int x9 = x7[0] + 10;
  x7[1] = x7[4] == x9 ? x7[5] : ({
    int x10 = x7[x9];
    x7[6] = x8 + 1 + 1;
    x7[4] = x9;
    x7[5] = x10;
    x10;
  });
  x1(x7);
}
/**************** Snippet ****************/
void Snippet(int* x0) {
  x6(x0);
}
/*****************************************
End of C Generated Code
*******************************************/
int init(int* s) {
  for (int i=0; i<100; i++) {
    s[i] = 0;
  }
  return 0;
}
/*@ requires low < high;
ensures low < \result && \result < high;
assigns \result \from low, high;
*/
int bounded(int low, int high);
int main(int argc, char* argv[]) {
  int s[100];
  init(s);
  int x = bounded(0, 20);
  s[0] = x;
  int i;
  for (i=0; i<20; i++) {
    s[i+10] = bounded(0, 20);
  }
  Snippet(s);
  return 0;
}
