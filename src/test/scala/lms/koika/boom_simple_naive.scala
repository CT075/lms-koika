package lms.koika

import scala.collection.immutable.Map
import scala.collection.mutable.{Map => MutableMap}

import lms.core.stub._
import lms.core.virtualize
import lms.macros.SourceContext
import lms.macros.RefinedManifest

import lms.collection.mutable._

object RiscVStripped {
  case class Reg(i: Int)

  abstract sealed class Cmp
  case object Eq extends Cmp
  case object Ne extends Cmp
  case object Lt extends Cmp
  case object Ge extends Cmp

  abstract sealed class Instruction
  case class Add(rd: Reg, rs1: Reg, rs2: Reg) extends Instruction
  case class Addi(rd: Reg, rs1: Reg, imm: Int) extends Instruction
  case class Mul(rd: Reg, rs1: Reg, rs2: Reg) extends Instruction
  // ldr rd, [rs, offs]
  case class Load(rd: Reg, rs: Reg, offs: Int) extends Instruction
  // str rv, [rd, offs]
  case class Store(rd: Reg, offs: Int, rv: Reg) extends Instruction
  case class B(cmp: Option[Cmp], rs1: Reg, rs2: Reg, offs: Int) extends Instruction
}

// Naive approach, stage the entire cycle pipeline loop into the residue.
@virtualize
class BoomNaiveTests extends TutorialFunSuite {
  val under = "boom_naive_"

  override def exec(label: String, code: String, suffix: String = "c") =
    super.exec(label, code, suffix)

  override def check(label: String, code: String, suffix: String = "c") =
    super.check(label, code, suffix)

  import RiscVStripped._

  // CR cam: Look more deeply into instruction decode. Do we need to track the
  // actual word being read?
  @CStruct
  case class InstructionS
    ( tag: Int
    , v1: Int
    , v2: Int
    , v3: Int
    )

  // CR-soon cam: Can we generate these?
  object InsTags {
    private[this] var __c = 0;
    private[this] def iota() = {
      val result = __c
      __c += 1
      result
    }
    val add = iota;
    val addi = iota;
    val mul = iota;

    val ldr = iota;

    val str = iota;

    val b = iota;
    val beq = iota;
    val bne = iota;
    val blt = iota;
    val bge = iota;
  }

  import InsTags._

  val noop = InstructionS(addi, 0, 0, 0)

  object InstructionOps {
    def encode(i: Instruction): InstructionS = i match {
      case Add(rd: Reg, rs1: Reg, rs2: Reg) => InstructionS(add, rd.i, rs1.i, rs2.i)
      case Addi(rd: Reg, rs1: Reg, imm: Int) => InstructionS(addi, rd.i, rs1.i, imm)
      case Mul(rd: Reg, rs1: Reg, rs2: Reg) => InstructionS(mul, rd.i, rs1.i, rs2.i)
      case Load(rd: Reg, rs: Reg, offs: Int) => InstructionS(ldr, rd.i, rs.i, offs)
      case Store(rd: Reg, offs: Int, rv: Reg) => InstructionS(str, rd.i, offs, rv.i)
      case B(None, rs1: Reg, rs2: Reg, offs: Int) => InstructionS(b, rs1.i, rs2.i, offs)
      case B(Some(Eq), rs1: Reg, rs2: Reg, offs: Int) => InstructionS(beq, rs1.i, rs2.i, offs)
      case B(Some(Ne), rs1: Reg, rs2: Reg, offs: Int) => InstructionS(bne, rs1.i, rs2.i, offs)
      case B(Some(Lt), rs1: Reg, rs2: Reg, offs: Int) => InstructionS(blt, rs1.i, rs2.i, offs)
      case B(Some(Ge), rs1: Reg, rs2: Reg, offs: Int) => InstructionS(bge, rs1.i, rs2.i, offs)
    }
  }

  // CR cam: reinstate caches
  // CR cam: superscalar fetching

  @CStruct
  case class BTBEntry
    ( tag: Int
    , valid: Boolean
    , target: Int
    , timestamp : Int
    )

  trait BTBOps extends BTBEntryOps with ArrayOps

  @CStruct
  case class PendingBranch
    ( pc: Int
    , valid: Boolean
    , predictedTarget: Int
    )

  @CStruct
  case class Frontend
    ( btb: Array[BTBEntry]
    , fetchPCReady: Boolean
    , fetchPC: Int
    , instructionReady: Boolean
    , instructionOut: InstructionS
    , pendingBranchList: Array[PendingBranch]
    , pcOut: Int
    , done: Boolean
    )

  // CR cam: OOO and renaming doesn't work yet
  @CStruct
  case class Scheduler
    ( enqueued: InstructionS
      // currently, ready and full are always the same, but a more complex
      // scheduler can have them vary
    , ready: Boolean
    , full: Boolean
    )

  @CStruct
  case class ALU
    ( uop: Int
    , vl: Int
    , vr: Int
    , destReg: Int
    , waitCtr: Int
    , done: Boolean
    , timestamp: Int
    , result: Int
    )

  @CStruct
  case class MemoryUnit
    ( requestedAddr: Int
    , writeRequested: Boolean
    , destReg: Int
    , bus: Int
      // simulates taking multiple cycles to load or store
    , waitCtr: Int
    , done: Boolean
    , timestamp: Int
    , output: Int
    )

  // The true BOOM pipeline has a pretty complex interleaved execution/commit
  // pipeline that involves a series of abstract (possibly themselves-pipelined)
  // execution units. We will make a simplifying assumption that there is
  // one dedicated ALU (used for register math) and that branch target
  // resolution/memory addressing is free.
  @CStruct
  case class State
    ( frontend: Frontend
    , scheduler: Scheduler
    , mu: MemoryUnit
    , alu: ALU
    , regFile: Array[Int]
    , memory: Array[Int]
      // This pc is the pc of the instruction being executed (in other words,
      // the instruction immediately before the point of no return).
    , pc: Int
    , ticks: Int
    )

  trait DslUtil extends Dsl {
    def member(needle: Rep[Int], haystack: Int*): Rep[Boolean] = {
      for (i <- haystack) {
        if (unit(i) == needle) {
          return unit(true)
        }
      }

      return unit(false)
    }

    def isArithmetic(tag: Rep[Int]): Rep[Boolean] = {
      member(tag, add, addi, mul)
    }

    def isMemory(tag: Rep[Int]): Rep[Boolean] = {
      member(tag, ldr, str)
    }

    def isBranch(tag: Rep[Int]): Rep[Boolean] = {
      member(tag, b, beq, bne, bge, blt)
    }
  }

  abstract class BOOMRunner {
    val program: Array[Instruction]
    val btbSize: Int
    val numArchRegs: Int
    val numPhysRegs: Int
    val memSize: Int
    val limit: Int

    // Because [StructOps] uses mutation under the hood and we don't expose any
    // primitives for allocation, we need to be very careful about how the
    // stages compose to avoid accidentally leaking information into a further
    // stage. The ordering is very tricky, because the information flow graph
    // is inherently cyclic (fetch->decode->execute->fetch).

    // With a 4-cycle fetch, I'm pretty sure it's impossible to be under
    // more than 4 branches at once. But this is not a "real" chip so it is
    // probably cheap to just allocate twice as many slots in the branch
    // checker.
    val numSpeculativeBranches = 8

    trait FrontDsl extends DslUtil
      with FrontendOps
      with BTBOps
      with InstructionSOps
      with PendingBranchOps

    trait InterpFrontend extends FrontDsl {
      def queryBTB(state: Rep[Frontend]): Rep[Int] = {
        for (i <- (0 until btbSize): Range) {
          val entry: Rep[BTBEntry] = state.btb(i)
          if (entry.valid && entry.tag == state.fetchPC) {
            return entry.target
          }
        }

        return state.fetchPC+1
      }

      def loadInstr(state: Rep[Frontend]): (Rep[Boolean], Rep[Boolean], Rep[InstructionS]) = {
        if (state.fetchPCReady) {
          if (state.fetchPC >= program.length || state.fetchPC < 0) {
            return (unit(true), unit(false), unit(noop))
          }
          else {
            for (i <- (0 until program.length): Range) {
              if (i == state.fetchPC) {
                return (unit(false), unit(true), unit(InstructionOps.encode(program(i))))
              }
            }
          }
        }
        else {
          return (unit(false), unit(false), unit(noop))
        }

        (unit(true), unit(false), unit(noop))
      }

      def fetchReady(state: Rep[Frontend]): Rep[Boolean] = {
        state.instructionReady && !state.done
      }

      // Pass F3 to the output
      // requires: fetchReady
      def fetch(state: Rep[Frontend]): (Rep[InstructionS], Rep[Int]) = {
        state.instructionReady = false
        (state.instructionOut, state.pcOut)
      }

      def stepFetch(state: Rep[Frontend]): Rep[Unit] = {
        // pass F1/F2 output to F3
        // CR cam: We actually need to delay a cycle here
        val (done, instrAvailable, instr) = loadInstr(state)
        if (!state.instructionReady && instrAvailable) {
          state.instructionReady = true
          state.instructionOut = instr
          state.done = done
          state.fetchPCReady = false
        }
        val nextPC = queryBTB(state)

        // add entry to BTB
        if (instrAvailable && isBranch(instr.tag)) {
          for (i <- (0 until numSpeculativeBranches): Range) {
            val entry = state.pendingBranchList(i)
            if (!entry.valid) {
              entry.pc = state.pcOut
              entry.valid = true
              entry.predictedTarget = nextPC
            }
          }
        }

        // Set the next PC
        if (!state.fetchPCReady) {
          state.fetchPC = nextPC
          state.fetchPCReady = true
        }
      }

      def notifyBranchTrueResult
        ( state: Rep[Frontend]
        , pc: Rep[Int]
        , trueTarget: Rep[Int]
        , timestamp: Rep[Int]
        ): Rep[Boolean] =
      {
        var result = __newVar(true)

        // look up result

        for (i <- (0 until numSpeculativeBranches): Range) {
          val entry = state.pendingBranchList(i)
          if (entry.valid && entry.pc == pc) {
            entry.valid = false
            result = entry.predictedTarget == trueTarget
          }
        }

        // update BTB

        // BTB is run as an LRU table, but an entry will always try to replace
        // a slot using the same tag first, if possible.
        var slot = __newVar(0)
        var oldest = __newVar(limit)
        var foundSelf = __newVar(false)
        var foundInvalid = __newVar(false)
        for (i <- (0 until btbSize): Range) {
          val btbentry = state.btb(i)
          if (btbentry.tag == pc) {
            foundSelf = true
            slot = i
          }
          else if (!btbentry.valid && !foundSelf) {
            foundInvalid = true
            slot = i
          }
          else if (btbentry.timestamp < oldest && !foundSelf && !foundInvalid) {
            slot = i
            oldest = btbentry.timestamp
          }
        }

        val newbtbentry = state.btb(slot)
        newbtbentry.tag = pc
        newbtbentry.valid = true
        newbtbentry.target = trueTarget
        newbtbentry.timestamp = timestamp

        result
      }

      def squash(state: Rep[Frontend], truePC: Rep[Int]) {
        state.fetchPC = truePC
        state.fetchPCReady = true
        state.instructionReady = false
        state.done = false
      }

      def fetchWorkRemaining(state: Rep[Frontend]): Rep[Boolean] = {
        !state.done && state.instructionReady && state.fetchPCReady
      }
    }

    trait SchedulerDsl extends Dsl
      with SchedulerOps
      with InstructionSOps

    trait InterpScheduler extends SchedulerDsl {
      def peek(scheduler: Rep[Scheduler]): (Rep[Boolean], Rep[Int]) = {
        (scheduler.ready, scheduler.enqueued.tag)
      }

      // requires: scheduler.ready
      def dequeue(scheduler: Rep[Scheduler]): Rep[InstructionS] = {
        scheduler.ready = false
        scheduler.full = false
        scheduler.enqueued
      }

      // requires: schedulerWaiting
      def schedule
        ( scheduler: Rep[Scheduler]
        , ins: Rep[InstructionS]
        ): Rep[Unit] =
      {
        scheduler.enqueued = ins
        scheduler.full = true
        scheduler.ready = true
      }

      def stepScheduler(scheduler: Rep[Scheduler]): Rep[Unit] = unit(())

      def schedulerWaiting(scheduler: Rep[Scheduler]): Rep[Boolean] = {
        !scheduler.full
      }

      def squash(scheduler: Rep[Scheduler]): Rep[Unit] = {
        scheduler.full = false
        scheduler.ready = false
      }

      def schedulerWorkRemaining(scheduler: Rep[Scheduler]): Rep[Boolean] = {
        scheduler.full && scheduler.ready
      }
    }

    trait InterpALU extends Dsl with ALUOps {
      def checkALU(alu: Rep[ALU]): (Rep[Boolean], Rep[Int], Rep[Int]) = {
        if (!alu.done && alu.waitCtr == 0) {
          alu.done = true
          return (unit(true), alu.result, alu.timestamp)
        }

        (unit(false), unit(0), unit(0))
      }

      def stepALU(alu: Rep[ALU]): Rep[Unit] = {
        if (alu.done) {
          return unit(())
        }

        if (alu.waitCtr > 0) {
          alu.waitCtr -= 1
        }
        else {
          if (alu.uop == add) {
            alu.result = alu.vl + alu.vr
          }
          else if (alu.uop == addi) {
            alu.result = alu.vl - alu.vr
          }
          else if (alu.uop == mul) {
            alu.result = alu.vl * alu.vr
          }
        }
      }

      // requires: alu.done
      def requestUOp
        ( alu: Rep[ALU]
        , uop: Rep[Int]
        , dst: Rep[Int]
        , vl: Rep[Int]
        , vr: Rep[Int]
        ): Rep[Unit] =
      {
        alu.vl = vl
        alu.vr = vr
        alu.uop = uop
        alu.destReg = dst

        if (alu.uop == mul) {
          alu.waitCtr = 7
        }
        else {
          alu.waitCtr = 0
        }
      }

      def aluWorkRemaining(alu: Rep[ALU]): Rep[Boolean] = {
        !alu.done
      }
    }

    trait MemoryUnitDsl extends Dsl
      with ArrayOps
      with MemoryUnitOps

    trait InterpMemoryUnit extends MemoryUnitDsl {
      def checkMemoryLoad(mu: Rep[MemoryUnit]): (Rep[Boolean], Rep[Int], Rep[Int]) = {
        if (!mu.done && !mu.writeRequested && mu.waitCtr == 0) {
          mu.done = true
          return (unit(true), mu.bus, mu.timestamp)
        }

        (unit(false), unit(0), unit(0))
      }

      def stepMU(mu: Rep[MemoryUnit], memory: Rep[Array[Int]]): Rep[Unit] = {
        if (mu.done) {
          return unit(())
        }

        if (mu.waitCtr > 0) {
          mu.waitCtr -= 1
        }
        else {
          if (mu.writeRequested) {
            memory(mu.requestedAddr) = mu.bus
          }
          else {
            mu.bus = memory(mu.requestedAddr)
          }
        }
      }

      // requires: mu.done
      def requestWrite(mu: Rep[MemoryUnit], addr: Rep[Int], v: Rep[Int]): Rep[Unit] = {
        mu.done = false
        mu.waitCtr = 10
        mu.requestedAddr = addr
        mu.bus = v
        mu.writeRequested = true
      }

      // requires: mu.done
      def requestRead(mu: Rep[MemoryUnit], addr: Rep[Int], dst: Rep[Int]): Rep[Unit] = {
        mu.done = false
        mu.waitCtr = 10
        mu.requestedAddr = addr
        mu.destReg = dst
        mu.writeRequested = false
      }

      def muWorkRemaining(mu: Rep[MemoryUnit]): Rep[Boolean] = {
        !mu.done
      }
    }

    trait CommitDsl extends Dsl
      with InstructionSOps
      with ArrayOps

    trait InterpCommit extends CommitDsl {
      def commit(regFile: Rep[Array[Int]], dst: Rep[Int], v: Rep[Int]): Rep[Unit] = {
        regFile(dst) = v
      }
    }

    trait Interp extends StateOps
      // We assume decode is constant time and thus ignore it.
      with InterpFrontend
      with InterpScheduler
      with InterpMemoryUnit
      with InterpALU
      with InterpCommit
    {
      // If, after a full pipeline run, there is no more work in-progress, we're done.
      def workRemaining(state: Rep[State]): Rep[Boolean] = {
        muWorkRemaining(state.mu) ||
        aluWorkRemaining(state.alu) ||
        schedulerWorkRemaining(state.scheduler) ||
        fetchWorkRemaining(state.frontend)
      }

      def run(state: Rep[State]): Rep[State] = {
        while (state.ticks < limit && !workRemaining(state)) {
          state.ticks += 1

          // To ensure that component stalls  are respected, we iterate over
          // components *backwards*, where the final stage (commit) requests
          // information from the previous stage (and does nothing if that
          // stage is not ready). In other words, the pipeline is demand-driven,
          // with the commit stage pulling information forwards until it
          // receives the done signal.

          // Commit

          // If something is ready to commit, it is no longer speculative --
          // a mis-speculated branch must have been detected in the execute
          // stage, which would have squashed any erroneous to-be-committed
          // instruction before reaching here.

          val (memReadReady, readResult, memtime) = checkMemoryLoad(state.mu)
          val (aluReady, aluResult, alutime) = checkALU(state.alu)
          if (memReadReady && aluReady) {
            if (memtime < alutime) {
              commit(state.regFile, state.mu.destReg, readResult)
              commit(state.regFile, state.alu.destReg, aluResult)
            }
            else {
              commit(state.regFile, state.alu.destReg, aluResult)
              commit(state.regFile, state.mu.destReg, readResult)
            }
          }
          else {
            if (memReadReady) {
              commit(state.regFile, state.mu.destReg, readResult)
            }
            if (aluReady) {
              commit(state.regFile, state.alu.destReg, aluResult)
            }
          }

          stepMU(state.mu, state.memory)
          stepALU(state.alu)

          // Execute

          val (schedulerReady, nextInsTag) = peek(state.scheduler)

          var needSquash = __newVar(false)

          var nextPC = __newVar(state.pc+1)
          if (schedulerReady) {
            if (isArithmetic(nextInsTag) && state.alu.done) {
              val ins = dequeue(state.scheduler)
              requestUOp(state.alu, ins.tag, ins.v1, ins.v2, ins.v3)
            }
            else if (isMemory(nextInsTag) && state.mu.done) {
              val ins = dequeue(state.scheduler)
              if (ins.tag == ldr) {
                // ldr rd, [rs, imm] => <ldr, rd, rs, imm>
                requestRead(state.mu, ins.v2+ins.v3, ins.v1)
              }
              else if (ins.tag == str) {
                // str rv, [rd, imm] => <str, rd, imm, rv>
                requestWrite(state.mu, ins.v1+ins.v2, ins.v3)
              }
            }
            else if (isBranch(nextInsTag)) {
              val ins = dequeue(state.scheduler)
              // cmp rs1, rs2; b__ pc+n => <b__, rs1, rs2, n>
              if (ins.tag == b) {
                nextPC = state.pc + ins.v3
              }
              else if (ins.tag == beq) {
                if (state.regFile(ins.v1) == state.regFile(ins.v2)) {
                  nextPC = state.pc + ins.v3
                }
              }
              else if (ins.tag == bne) {
                if (state.regFile(ins.v1) != state.regFile(ins.v2)) {
                  nextPC = state.pc + ins.v3
                }
              }
              else if (ins.tag == blt) {
                if (state.regFile(ins.v1) < state.regFile(ins.v2)) {
                  nextPC = state.pc + ins.v3
                }
              }
              else if (ins.tag == bge) {
                if (state.regFile(ins.v1) >= state.regFile(ins.v2)) {
                  nextPC = state.pc + ins.v3
                }
              }

              needSquash = notifyBranchTrueResult(state.frontend, state.pc, nextPC, state.ticks)
            }
          }

          // Even if we need to squash, we finish back-propagating the pipeline
          // update first before squashing instructions in-flight.
          stepScheduler(state.scheduler)

          // Fetch

          if (schedulerWaiting(state.scheduler) && fetchReady(state.frontend)) {
            val (fetchedInstr, fetchedPC) = fetch(state.frontend)
            state.pc = fetchedPC
            schedule(state.scheduler, fetchedInstr)
          }

          stepFetch(state.frontend)

          // Now, annul if necessary.

          if (needSquash) {
            squash(state.scheduler)
            squash(state.frontend, nextPC)
          }

        }

        state
      }
    }
  }

  abstract class DslDriverX[A:Manifest,B:Manifest] extends DslDriverC[A,B] { q =>
    val header: String = ""
    val mainBody: String

    def main(): String = s"""
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

$header

int main(int argc, char *argv[]) {
  if (argc != 2) {
    printf("usage: %s <arg>\\n", argv[0]);
    return 0;
  }

  $mainBody
}
"""

    override val codegen = new DslGenC with CCodeGenStruct {
      val IR: q.type = q

      override def emitAll(g: lms.core.Graph, name: String)(m1:Manifest[_],m2:Manifest[_]): Unit = {
        val ng = init(g)
        val efs = "" //quoteEff(g.block.ein)
        val stt = dce.statics.toList.map(quoteStatic).mkString(", ")
        prepareHeaders
        emitln("""
/*****************************************
Emitting C Generated Code
*******************************************/
        """.stripMargin)
        val src = run(name, ng)
        emitDefines(stream)
        emitHeaders(stream)
        emitDatastructures(stream)
        emitFunctionDecls(stream)
        emitFunctions(stream)
        emitInit(stream)
        emitln(s"\n/**************** $name ****************/")
        emit(src)
        emitln("""
/*****************************************
End of C Generated Code
*******************************************/
        """.stripMargin)
        emit(main())
      }
    }
  }

  test("1") {
    val runner = new BOOMRunner {
      val N = Reg(4)
      val Temp = Reg(3)
      val F_n = Reg(2)
      val F_n_1 = Reg(1)
      val ZERO = Reg(0)
      override val program = Array(
        Addi(F_n, ZERO, 1),
        Addi(F_n_1, ZERO, 0),
        Addi(N, ZERO, 15),
        Addi(Temp, ZERO, 0),
        Add(Temp, F_n, F_n_1),
        Add(F_n_1, F_n, ZERO),
        Add(F_n, Temp, ZERO),
        Addi(N, N, -1),
        B(Some(Ne), N, ZERO, -4),
      )
      override val btbSize: Int = 16
      override val numArchRegs: Int = 8
      override val numPhysRegs: Int = 8
      override val memSize: Int = 32
      override val limit: Int = 500
    }
    val snippet = new DslDriverX[State, State] with runner.Interp {
      override val header = """
int fact(int i) {
  __CPROVER_assert(0 <= i, "bad domain (fact)");
  if (i == 0) { return 1; }
  return i * fact(i-1);
}
"""

      // XXX - this looks terrible
      val mainBody = """
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
"""

      def snippet(inp: Rep[State]): Rep[State] = {
        run(inp)
      }
    }
    check("fact", snippet.code)
  }
}
