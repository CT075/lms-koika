/*****************************************
Emitting C Generated Code
*******************************************/
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
/*********** Datastructures ***********/
struct InstructionS;
struct MemoryUnit;
struct Frontend;
struct BTBEntry;
struct Scheduler;
struct PendingBranch;
struct State;
struct ALU;
struct InstructionS {
  int tag;
  int v1;
  int v2;
  int v3;
};
struct MemoryUnit {
  int requestedAddr;
  bool writeRequested;
  int destReg;
  int bus;
  int waitCtr;
  bool done;
  int timestamp;
  int output;
};
struct Frontend {
  struct BTBEntry* btb;
  bool fetchPCReady;
  int fetchPC;
  bool instructionReady;
  struct InstructionS instructionOut;
  struct PendingBranch* pendingBranchList;
  int pcOut;
  bool done;
};
struct BTBEntry {
  int tag;
  bool valid;
  int target;
  int timestamp;
};
struct Scheduler {
  struct InstructionS enqueued;
  bool ready;
  bool full;
};
struct PendingBranch {
  int pc;
  bool valid;
  int predictedTarget;
};
struct State {
  struct Frontend frontend;
  struct Scheduler scheduler;
  struct MemoryUnit mu;
  struct ALU alu;
  int* regFile;
  int* memory;
  int pc;
  int ticks;
};
struct ALU {
  int uop;
  int vl;
  int vr;
  int destReg;
  int waitCtr;
  bool done;
  int timestamp;
  int result;
};
/**************** Snippet ****************/
struct State Snippet(struct State x0) {
  while (x0.ticks < 500 && !(!x0.mu.done || !x0.alu.done || ({
    struct Scheduler x1 = x0.scheduler;
    x1.full && x1.ready;
  }) || ({
    struct Frontend x2 = x0.frontend;
    !x2.done && x2.instructionReady && x2.fetchPCReady;
  }))) {
    x0.ticks = x0.ticks + 1;
    struct MemoryUnit x3 = x0.mu;
    x3.done = true;
    struct ALU x4 = x0.alu;
    x4.done = true;
    if (x3.timestamp < x4.timestamp) {
      x0.regFile[x0.mu.destReg] = x3.bus;
      x0.regFile[x0.alu.destReg] = x4.result;
    } else {
      x0.regFile[x0.alu.destReg] = x4.result;
      x0.regFile[x0.mu.destReg] = x3.bus;
    }
    bool x5 = false;
    int x6 = x0.pc + 1;
    if (x0.scheduler.ready) if (x0.alu.done) {
      struct Scheduler x7 = x0.scheduler;
      x7.ready = false;
      x7.full = false;
      struct InstructionS x8 = x7.enqueued;
      struct ALU x9 = x0.alu;
      x9.vl = x8.v2;
      x9.vr = x8.v3;
      x9.uop = x8.tag;
      x9.destReg = x8.v1;
      if (x9.uop == 2) x9.waitCtr = 7;
      else x9.waitCtr = 0;
    } else if (x0.mu.done) {
      struct Scheduler x10 = x0.scheduler;
      x10.ready = false;
      x10.full = false;
      struct InstructionS x11 = x10.enqueued;
      if (x11.tag == 3) {
        struct MemoryUnit x12 = x0.mu;
        x12.done = false;
        x12.waitCtr = 10;
        x12.requestedAddr = x11.v2 + x11.v3;
        x12.destReg = x11.v1;
        x12.writeRequested = false;
      } else if (x11.tag == 4) {
        struct MemoryUnit x13 = x0.mu;
        x13.done = false;
        x13.waitCtr = 10;
        x13.requestedAddr = x11.v1 + x11.v2;
        x13.bus = x11.v3;
        x13.writeRequested = true;
      }
    } else {
      struct Scheduler x14 = x0.scheduler;
      x14.ready = false;
      x14.full = false;
      struct InstructionS x15 = x14.enqueued;
      if (x15.tag == 5) x6 = x0.pc + x15.v3;
      else if (x15.tag == 6) if (x0.regFile[x15.v1] == x0.regFile[x15.v2]) x6 = x0.pc + x15.v3;
      else if (x15.tag == 7) if (x0.regFile[x15.v1] != x0.regFile[x15.v2]) x6 = x0.pc + x15.v3;
      else if (x15.tag == 8) if (x0.regFile[x15.v1] < x0.regFile[x15.v2]) x6 = x0.pc + x15.v3;
      else if (x15.tag == 9) if (x0.regFile[x15.v1] >= x0.regFile[x15.v2]) x6 = x0.pc + x15.v3;
      struct Frontend x16 = x0.frontend;
      int x17 = x0.pc;
      int x18 = x6;
      bool x19 = true;
      struct PendingBranch x20 = x16.pendingBranchList[0];
      if (x20.valid && x20.pc == x17) {
        x20.valid = false;
        x19 = x20.predictedTarget == x18;
      }
      struct PendingBranch x21 = x16.pendingBranchList[1];
      if (x21.valid && x21.pc == x17) {
        x21.valid = false;
        x19 = x21.predictedTarget == x18;
      }
      struct PendingBranch x22 = x16.pendingBranchList[2];
      if (x22.valid && x22.pc == x17) {
        x22.valid = false;
        x19 = x22.predictedTarget == x18;
      }
      struct PendingBranch x23 = x16.pendingBranchList[3];
      if (x23.valid && x23.pc == x17) {
        x23.valid = false;
        x19 = x23.predictedTarget == x18;
      }
      struct PendingBranch x24 = x16.pendingBranchList[4];
      if (x24.valid && x24.pc == x17) {
        x24.valid = false;
        x19 = x24.predictedTarget == x18;
      }
      struct PendingBranch x25 = x16.pendingBranchList[5];
      if (x25.valid && x25.pc == x17) {
        x25.valid = false;
        x19 = x25.predictedTarget == x18;
      }
      struct PendingBranch x26 = x16.pendingBranchList[6];
      if (x26.valid && x26.pc == x17) {
        x26.valid = false;
        x19 = x26.predictedTarget == x18;
      }
      struct PendingBranch x27 = x16.pendingBranchList[7];
      if (x27.valid && x27.pc == x17) {
        x27.valid = false;
        x19 = x27.predictedTarget == x18;
      }
      int x28 = 0;
      int x29 = 500;
      bool x30 = false;
      bool x31 = false;
      struct BTBEntry x32 = x16.btb[0];
      if (x32.tag == x17) {
        x30 = true;
        x28 = 0;
      } else if (!x32.valid) {
        x31 = true;
        x28 = 0;
      } else if (x32.timestamp < 500) {
        x28 = 0;
        x29 = x32.timestamp;
      }
      struct BTBEntry x33 = x16.btb[1];
      if (x33.tag == x17) {
        x30 = true;
        x28 = 1;
      } else if (!x33.valid && !x30) {
        x31 = true;
        x28 = 1;
      } else if (x33.timestamp < x29 && !x30 && !x31) {
        x28 = 1;
        x29 = x33.timestamp;
      }
      struct BTBEntry x34 = x16.btb[2];
      if (x34.tag == x17) {
        x30 = true;
        x28 = 2;
      } else if (!x34.valid && !x30) {
        x31 = true;
        x28 = 2;
      } else if (x34.timestamp < x29 && !x30 && !x31) {
        x28 = 2;
        x29 = x34.timestamp;
      }
      struct BTBEntry x35 = x16.btb[3];
      if (x35.tag == x17) {
        x30 = true;
        x28 = 3;
      } else if (!x35.valid && !x30) {
        x31 = true;
        x28 = 3;
      } else if (x35.timestamp < x29 && !x30 && !x31) {
        x28 = 3;
        x29 = x35.timestamp;
      }
      struct BTBEntry x36 = x16.btb[4];
      if (x36.tag == x17) {
        x30 = true;
        x28 = 4;
      } else if (!x36.valid && !x30) {
        x31 = true;
        x28 = 4;
      } else if (x36.timestamp < x29 && !x30 && !x31) {
        x28 = 4;
        x29 = x36.timestamp;
      }
      struct BTBEntry x37 = x16.btb[5];
      if (x37.tag == x17) {
        x30 = true;
        x28 = 5;
      } else if (!x37.valid && !x30) {
        x31 = true;
        x28 = 5;
      } else if (x37.timestamp < x29 && !x30 && !x31) {
        x28 = 5;
        x29 = x37.timestamp;
      }
      struct BTBEntry x38 = x16.btb[6];
      if (x38.tag == x17) {
        x30 = true;
        x28 = 6;
      } else if (!x38.valid && !x30) {
        x31 = true;
        x28 = 6;
      } else if (x38.timestamp < x29 && !x30 && !x31) {
        x28 = 6;
        x29 = x38.timestamp;
      }
      struct BTBEntry x39 = x16.btb[7];
      if (x39.tag == x17) {
        x30 = true;
        x28 = 7;
      } else if (!x39.valid && !x30) {
        x31 = true;
        x28 = 7;
      } else if (x39.timestamp < x29 && !x30 && !x31) {
        x28 = 7;
        x29 = x39.timestamp;
      }
      struct BTBEntry x40 = x16.btb[8];
      if (x40.tag == x17) {
        x30 = true;
        x28 = 8;
      } else if (!x40.valid && !x30) {
        x31 = true;
        x28 = 8;
      } else if (x40.timestamp < x29 && !x30 && !x31) {
        x28 = 8;
        x29 = x40.timestamp;
      }
      struct BTBEntry x41 = x16.btb[9];
      if (x41.tag == x17) {
        x30 = true;
        x28 = 9;
      } else if (!x41.valid && !x30) {
        x31 = true;
        x28 = 9;
      } else if (x41.timestamp < x29 && !x30 && !x31) {
        x28 = 9;
        x29 = x41.timestamp;
      }
      struct BTBEntry x42 = x16.btb[10];
      if (x42.tag == x17) {
        x30 = true;
        x28 = 10;
      } else if (!x42.valid && !x30) {
        x31 = true;
        x28 = 10;
      } else if (x42.timestamp < x29 && !x30 && !x31) {
        x28 = 10;
        x29 = x42.timestamp;
      }
      struct BTBEntry x43 = x16.btb[11];
      if (x43.tag == x17) {
        x30 = true;
        x28 = 11;
      } else if (!x43.valid && !x30) {
        x31 = true;
        x28 = 11;
      } else if (x43.timestamp < x29 && !x30 && !x31) {
        x28 = 11;
        x29 = x43.timestamp;
      }
      struct BTBEntry x44 = x16.btb[12];
      if (x44.tag == x17) {
        x30 = true;
        x28 = 12;
      } else if (!x44.valid && !x30) {
        x31 = true;
        x28 = 12;
      } else if (x44.timestamp < x29 && !x30 && !x31) {
        x28 = 12;
        x29 = x44.timestamp;
      }
      struct BTBEntry x45 = x16.btb[13];
      if (x45.tag == x17) {
        x30 = true;
        x28 = 13;
      } else if (!x45.valid && !x30) {
        x31 = true;
        x28 = 13;
      } else if (x45.timestamp < x29 && !x30 && !x31) {
        x28 = 13;
        x29 = x45.timestamp;
      }
      struct BTBEntry x46 = x16.btb[14];
      if (x46.tag == x17) {
        x30 = true;
        x28 = 14;
      } else if (!x46.valid && !x30) {
        x31 = true;
        x28 = 14;
      } else if (x46.timestamp < x29 && !x30 && !x31) {
        x28 = 14;
        x29 = x46.timestamp;
      }
      struct BTBEntry x47 = x16.btb[15];
      if (x47.tag == x17) {
        x30 = true;
        x28 = 15;
      } else if (!x47.valid && !x30) {
        x31 = true;
        x28 = 15;
      } else if (x47.timestamp < x29 && !x30 && !x31) {
        x28 = 15;
        x29 = x47.timestamp;
      }
      struct BTBEntry x48 = x16.btb[x28];
      x48.tag = x17;
      x48.valid = true;
      x48.target = x18;
      x48.timestamp = x0.ticks;
      x5 = x19;
    }
    if (!x0.scheduler.full && ({
      struct Frontend x49 = x0.frontend;
      x49.instructionReady && !x49.done;
    })) {
      struct Frontend x50 = x0.frontend;
      x50.instructionReady = false;
      x0.pc = x50.pcOut;
      struct Scheduler x51 = x0.scheduler;
      x51.enqueued = x50.instructionOut;
      x51.full = true;
      x51.ready = true;
    }
    struct Frontend x52 = x0.frontend;
    if (!x52.fetchPCReady) {
      x52.fetchPC = x52.btb[0].target;
      x52.fetchPCReady = true;
    }
    if (x5) {
      struct Scheduler x53 = x0.scheduler;
      x53.full = false;
      x53.ready = false;
      struct Frontend x54 = x0.frontend;
      x54.fetchPC = x6;
      x54.fetchPCReady = true;
      x54.instructionReady = false;
      x54.done = false;
    }
  }
  return x0;
}
/*****************************************
End of C Generated Code
*******************************************/
#ifndef CBMC
#define __CPROVER_assert(b,s) 0
#define nondet_uint() 0
#define __CPROVER_assume(b) 0
#else
unsigned int nondet_uint();
#endif
int bounded(int low, int high) {
  int x = nondet_uint();
  __CPROVER_assume(low <= x && x <= high);
  return x;
}
int fact(int i) {
  __CPROVER_assert(0 <= i, "bad domain (fact)");
  if (i == 0) { return 1; }
  return i * fact(i-1);
}
int main(int argc, char *argv[]) {
  if (argc != 2) {
    printf("usage: %s <arg>\n", argv[0]);
    return 0;
  }
  struct StateT state;
  state.regFile = calloc(8, sizeof(int));
  state.memory = calloc(32, sizeof(int));
  state.ticks = 0;
  state.frontend.btb = calloc(16, sizeof(struct BTBEntry));
  state.frontend.pendingBranchList = calloc(8, sizeof(struct BTBEntry));
  state.frontend.fetchPCReady = true;
  state.frontend.fetchPC = 0;
  state.alu.done = true;
  state.mu.done = true;
  int input = bounded(0, 5);
  state.regs[0] = input;
  for (int i = 1; i < 8; i += 1) {
    state.regFile[i] = 0;
  }
  for (int i = 0; i < 32; i += 1) {
    state.memory[i] = 0;
  }
  state = Snippet(state);
  __CPROVER_assert(state.regFile[2] == fact(input), "correct evaluation");
  return 0;
}
