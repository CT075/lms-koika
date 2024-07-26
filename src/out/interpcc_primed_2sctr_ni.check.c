/*****************************************
Emitting C Generated Code
*******************************************/
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
/*********** Datastructures ***********/
struct stateT {
  int* regs;
  int* saved_regs;
  int* mem;
  int* cache_keys;
  int* cache_vals;
  int timer;
};
/************* Function Declarations **************/
struct stateT x1(struct stateT);
/************* Functions **************/
struct stateT x1(struct stateT x2) {
  x2.timer = x2.timer + 1;
  x2.timer = x2.timer + 1;
  x2.saved_regs[1] = x2.regs[1];
  int x3 = x2.regs[0];
  int x4 = x2.cache_keys[0] == x3 ? x2.cache_vals[0] : (x2.cache_keys[1] == x3 ? ({
    int x5 = x2.cache_vals[1];
    x2.cache_keys[1] = x2.cache_keys[0];
    x2.cache_vals[1] = x2.cache_vals[0];
    x2.cache_keys[0] = x3;
    x2.cache_vals[0] = x5;
    x5;
  }) : ({
    int x6 = x2.mem[x3];
    x2.timer = x2.timer + 1;
    x2.timer = x2.timer + 1;
    if (x2.cache_keys[0] != x3) {
      x2.cache_keys[1] = x2.cache_keys[0];
      x2.cache_vals[1] = x2.cache_vals[0];
    }
    x2.cache_keys[0] = x3;
    x2.cache_vals[0] = x6;
    x6;
  }));
  x2.regs[1] = x4;
  x2.timer = x2.timer + 1;
  x2.saved_regs[2] = x2.regs[2];
  int x7 = 4 + x2.regs[1];
  int x8 = x2.cache_keys[0] == x7 ? x2.cache_vals[0] : (x2.cache_keys[1] == x7 ? ({
    int x9 = x2.cache_vals[1];
    x2.cache_keys[1] = x2.cache_keys[0];
    x2.cache_vals[1] = x2.cache_vals[0];
    x2.cache_keys[0] = x7;
    x2.cache_vals[0] = x9;
    x9;
  }) : ({
    int x10 = x2.mem[x7];
    x2.timer = x2.timer + 1;
    x2.timer = x2.timer + 1;
    if (x2.cache_keys[0] != x7) {
      x2.cache_keys[1] = x2.cache_keys[0];
      x2.cache_vals[1] = x2.cache_vals[0];
    }
    x2.cache_keys[0] = x7;
    x2.cache_vals[0] = x10;
    x10;
  }));
  x2.regs[2] = x8;
  x2.timer = x2.timer + 1;
  if (x2.regs[0] == 0) {
    x2.regs[1] = x2.saved_regs[1];
    x2.regs[2] = x2.saved_regs[2];
  }
  return x2;
}
/**************** Snippet ****************/
struct stateT Snippet(struct stateT x0) {
  return x1(x0);
}
/*****************************************
End of C Generated Code
*******************************************/
#define NUM_REGS 3
#define MEM_SIZE 30
#define SECRET_SIZE 20
#define SECRET_OFFSET 10
#define CACHE_SIZE 2
int bounded(int low, int high) {
  int x = nondet_uint();
  if (x < low) {
    x = low;
  }
  if (x > high) {
    x = high;
  }
  return x;
}
int init(struct stateT *s) {
  s->regs = calloc(sizeof(int), NUM_REGS);
  s->cache_keys = calloc(sizeof(int), CACHE_SIZE);
  s->cache_vals = calloc(sizeof(int), CACHE_SIZE);
  for (int i=0; i<CACHE_SIZE; i++) {
    s->cache_keys[i] = -1;
    s->cache_vals[i] = 0;
  }
  s->timer = 0;
  s->mem = calloc(sizeof(int), NUM_REGS);
  for (int i=0; i<MEM_SIZE; i++) {
    s->mem[i] = 0;
  }
  return 0;
}
void prime_cache(struct stateT *s, int k1, int k2) {
  s->cache_keys[0] = k1;
  s->cache_vals[0] = s->mem[k1];
  s->cache_keys[1] = k2;
  s->cache_vals[1] = s->mem[k2];
}
int main(int argc, char* argv[]) {
  struct stateT s1;
  init(&s1);
  struct stateT s2;
  init(&s2);
  int x = bounded(0, 20);
  s1.regs[0] = x;
  s2.regs[0] = x;
  int i;
  for (i=0; i<SECRET_SIZE; i++) {
    s1.mem[i+SECRET_OFFSET] = bounded(0, 20);
    s2.mem[i+SECRET_OFFSET] = bounded(0, 20);
  }
  int k1 = bounded(0, MEM_SIZE);
  int k2 = bounded(0, MEM_SIZE);
  if (k1 == k2) {
    k2 = (k1 + 1) % MEM_SIZE;
  }
  __CPROVER_assert(k1 != k2, "BUG: hot cache keys are not distinct");
  prime_cache(&s1, k1, k2);
  prime_cache(&s2, k1, k2);
  struct stateT s1_ = Snippet(s1);
  struct stateT s2_ = Snippet(s2);
  __CPROVER_assert(s1_.timer==s2_.timer, "timing leak");
  return 0;
}
