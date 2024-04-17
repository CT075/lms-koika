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

@virtualize
class BoomTests extends TutorialFunSuite {
  val under = "boom_naive_"

  // Naive approach, stage the entire cycle pipeline loop into the residue.
  object Naive {
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
      )

    trait BTBOps extends BTBEntryOps with ArrayOps

    @CStruct
    case class PendingBranch
      ( pc: Int
      , valid: Boolean
      , target: Int
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
      )

    // CR cam: OOO and renaming doesn't work yet
    @CStruct
    case class Scheduler
      ( enqueued: InstructionS
      , ready: Boolean
      )

    @CStruct
    case class ALU
      ( uop: Int
      , vl: Int
      , vr: Int
      , waitCtr: Int
      , done: Boolean
      , result: Int
      )

    @CStruct
    case class MemoryUnit
      ( requestedAddr: Int
      , writeRequested: Boolean
      , bus: Int
        // simulates taking multiple cycles to load or store
      , waitCtr: Int
      , done: Boolean
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
      , destReg: Int
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
    }

    class BOOMRunner
      ( program: Array[Instruction]
      , btbSize: Int
      , numArchRegs: Int
      , numPhysRegs: Int
      , memSize: Int
      , limit: Int
      )
    {
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

        // Given the entire frontend state, produce:
        //   - Whether we're done (encountered [ret] or ran off the program)
        //   - Whether there's an instruction ready
        //   - What that instruction is, exactly
        //   - The PC of the instruction that was fetched
        // and advance the entire pipeline.
        def fetch(state: Rep[Frontend]): (Rep[Boolean], Rep[Boolean], Rep[InstructionS], Rep[Int]) = {
          // First, gather all phase inputs

          // Output of F3
          val ready = state.instructionReady
          val result = state.instructionOut
          val pc = state.pcOut

          // Output of F1 and F2
          // CR cam: We actually need to delay a cycle here
          val (done, instrAvailable, instr) = loadInstr(state)
          val nextPC = queryBTB(state)

          // inputs to F4
          if (instrAvailable && member(instr.tag, b, beq, bne, bge, blt)) {
            for (i <- (0 until numSpeculativeBranches): Range) {
              if (!state.pendingBranchList(i).valid) {
                state.pendingBranchList(i).pc = pc
                state.pendingBranchList(i).valid = true
                state.pendingBranchList(i).target = nextPC
              }
            }
          }

          // inputs to F3
          state.instructionReady = instrAvailable
          state.instructionOut = instr
          state.pcOut = state.fetchPC

          // input to F1
          // Tentatively, set the fetch head to the predicted next step
          state.fetchPC = nextPC

          // F1 and F2 write to the

          (done, ready, result, pc)
        }
      }

      trait InterpScheduler extends Dsl with SchedulerOps {
        def nextScheduled(scheduler: Rep[Scheduler]): (Rep[Boolean], Rep[InstructionS]) = {
          val ready = scheduler.ready
          scheduler.ready = false
          (ready, scheduler.enqueued)
        }

        // Given an instruction, schedule it. Must be called after [dequeue]
        def schedule
          ( scheduler: Rep[Scheduler]
          , ins: Rep[InstructionS]
          ): Rep[Unit] =
        {
          scheduler.enqueued = ins
          scheduler.ready = true
        }
      }

      trait InterpExecute extends ExecuteDsl {
        def execute(ins: Rep[InstructionS]): Rep[Unit] = {
          if (ins.tag == add) {
          }
          else if (ins.tag == addi) {
          }
          else if (ins.tag == mul) {
          }
          else if (ins.tag == ldr) {
          }
          else if (ins.tag == str) {
          }
          else if (ins.tag == b) {
          }
          else if (ins.tag == beq) {
          }
          else if (ins.tag == bne) {
          }
          else if (ins.tag == blt) {
          }
          else if (ins.tag == bge) {
          }
        }
      }

      trait InterpALU extends Dsl with ALUOps {
        def checkALU(alu: Rep[ALU]): (Rep[Boolean], Rep[Int]) = {
          if (!alu.done && alu.waitCtr == 0) {
            alu.done = true
            return (unit(true), alu.result)
          }

          (unit(false), unit(0))
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
          , vl: Rep[Int]
          , vr: Rep[Int]
          ): Rep[Unit] =
        {
          alu.vl = vl
          alu.vr = vr
          alu.uop = uop

          if (alu.uop == mul) {
            alu.waitCtr = 7
          }
          else {
            alu.waitCtr = 0
          }
        }
      }

      trait MemoryUnitDsl extends Dsl
        with ArrayOps
        with MemoryUnitOps

      trait InterpMemoryUnit extends MemoryUnitDsl {
        def checkMemoryLoad(mu: Rep[MemoryUnit]): (Rep[Boolean], Rep[Int]) = {
          if (!mu.done && !mu.writeRequested && mu.waitCtr == 0) {
            mu.done = true
            return (unit(true), mu.bus)
          }

          (unit(false), unit(0))
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
        def requestRead(mu: Rep[MemoryUnit], addr: Rep[Int]): Rep[Unit] = {
          mu.done = false
          mu.waitCtr = 10
          mu.requestedAddr = addr
          mu.writeRequested = false
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

      trait InterpBoom extends StateOps
        // We assume decode is constant time and thus ignore it.
        with InterpFrontend
        with InterpScheduler
        with InterpMemoryUnit
        with InterpALU
        with InterpCommit
        with InstructionSOps
      {
        def run(state: Rep[State]): Rep[State] = {
          var ticks = __newVar(0)
          val exit = __newVar(false)

          while (state.ticks < limit && !exit) {
            state.ticks += 1

            // To ensure that pipeline bubbles are respected, we iterate over
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
            val (memReadReady, readResult) = checkMemoryLoad(state.mu)
            if (memReadReady) {
              commit(state.regFile, state.destReg, readResult)
            }
            else {
              val (aluReady, aluResult) = checkALU(state.alu)
              if (aluReady) {
                commit(state.regFile, state.destReg, aluResult)
              }
            }

            stepMU(state.mu, state.memory)
            stepALU(state.alu)
          }

          state
        }
      }
    }
  }
}
