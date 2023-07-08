/*****************************************
Emitting C Generated Code
*******************************************/
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
/**************** Snippet ****************/
int Snippet(int* x0) {
  state->scratch = 0;
  int x1 = 0;
  while (x1 != 16) {
    state->scratch = state->scratch + x0[x1];
    x1 = x1 + 1;
  }
  return state->scratch;
}
/*****************************************
End of C Generated Code
*******************************************/
