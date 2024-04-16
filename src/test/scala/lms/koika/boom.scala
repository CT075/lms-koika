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
    case class Frontend
      ( btb: Array[BTBEntry]
      , stalled: Boolean
      , fetchPCReady: Boolean
      , fetchPC: Int
      , instructionOutReady: Boolean
      , instructionOut: InstructionS
      , btbResponseReady: Boolean
      , btbResponse: Int
      , nextPCReady: Boolean
      , nextPCOut: Int
        // the final output of the frontend
      , finalFetchReady: Boolean
      , finalFetchOut: InstructionS
      , finalFetchPC: Int
      , done: Boolean
      )

    @CStruct
    case class ROBEntry
      ( valid: Boolean
      , busy: Boolean
      , renames: Array[Int]
      , pc: Int
      )

    @CStruct
    case class ROB
      ( entries: Array[ROBEntry]
      , head: Int
      , tail: Int
      , free: Array[Int]
      , freeHead: Int
      , freeTail: Int
      )

    @CStruct
    case class State
      ( frontend: Frontend
      , regFile: Array[Int]
      , memory: Array[Int]
      )

    class BOOMRunner
      ( program: Array[Instruction]
      , btbSize: Int
      , numArchRegs: Int
      , numPhysRegs: Int
      , memSize: Int
      )
    {
      // Because [StructOps] uses mutation under the hood and we don't expose any
      // primitives for allocation, we need to be very careful about how the
      // stages compose to avoid accidentally leaking information into a further
      // stage. The ordering is very tricky, because the information flow graph
      // is inherently cyclic (fetch->decode->execute->fetch).

      trait ProcDsl extends Dsl
        with FrontendOps
        with BTBOps

      trait FrontendInterp extends ProcDsl {
        def stepF4(state: Rep[Frontend], out: Rep[Frontend]): Rep[Unit] = {
          if (state.nextPCReady) {
            out.fetchPC = state.nextPCOut
            out.fetchPCReady = true
          }
          else {
            out.fetchPC = state.fetchPC
            out.fetchPCReady = state.fetchPCReady
          }
        }

        // CR cam: write to the branch checker
        def stepF3(state: Rep[Frontend], out: Rep[Frontend]): Rep[Unit] = {
          if (state.btbResponseReady || state.instructionOutReady) {
            out.nextPCOut = state.btbResponse
            out.nextPCReady = true
            out.finalFetchOut = state.instructionOut
            out.finalFetchPC = state.fetchPC
            out.finalFetchReady = true
          }
          else {
            out.nextPCOut = state.nextPCOut
            out.nextPCReady = state.nextPCReady
            out.finalFetchOut = state.finalFetchOut
            out.finalFetchReady = state.finalFetchReady
          }
        }

        // If we were using proper queues for the output of icache and btb, this
        // would be the enqueue stage
        def stepF2(state: Rep[Frontend], out: Rep[Frontend]): Rep[Unit] = {
          unit(())
        }

        def checkBTB(pc: Rep[Int], btb: Rep[Array[BTBEntry]], out: Rep[Frontend]): Rep[Unit] = {
          for (i <- (0 until btbSize): Range) {
            val entry: Rep[BTBEntry] = btb(i)
            if (entry.valid && entry.tag == pc) {
              out.btbResponse = entry.target
              out.btbResponseReady = true
              return unit(())
            }
          }

          out.btbResponse = pc + 1
          out.btbResponseReady = true
        }

        def stepF1(state: Rep[Frontend], out: Rep[Frontend]): Rep[Unit] = {
          if (state.fetchPCReady) {
            if (state.fetchPC >= program.length || state.fetchPC < 0) {
              out.done = true
            }
            else {
              // program unroll.
              for (i <- (0 until program.length): Range) {
                if (i == state.fetchPC) {
                  out.instructionOut = InstructionOps.encode(program(i))
                  out.instructionOutReady = true
                  checkBTB(state.fetchPC, state.btb, out)
                }
              }
            }
          }
          else {
            out.instructionOut = state.instructionOut
            out.instructionOutReady = state.instructionOutReady
          }
        }

        def runF1(state: Rep[Frontend]): (Rep[Boolean], Rep[Boolean], Rep[InstructionS]) = {
          var done = __newVar(true)
          var ready = __newVar(false)
          var result = __newVar(noop)

          if (state.fetchPCReady) {
            if (state.fetchPC >= program.length || state.fetchPC < 0) {
              return (unit(true), unit(false), unit(noop))
            }
            else {
              for (i <- (0 until program.length): Range) {
                if (i == state.fetchPC) {
                  done = false
                  ready = true
                  result = InstructionOps.encode(program(i))
                  //checkBTB(state.fetchPC, state.btb, out)
                }
              }
            }
          }
          else {
            return (unit(false), unit(false), unit(noop))
          }

          (done, ready, result)
        }

        def step(state: Rep[Frontend], out: Rep[Frontend]): Rep[Unit] = {
          if (state.stalled) {
            out.btb = state.btb
            out.stalled = state.stalled
            out.fetchPCReady = state.fetchPCReady
            out.fetchPC = state.fetchPC
            out.instructionOutReady = state.instructionOutReady
            out.instructionOut = state.instructionOut
            out.btbResponseReady = state.btbResponseReady
            out.btbResponse = state.btbResponse
            out.nextPCReady = state.nextPCReady
            out.nextPCOut = state.nextPCOut
            out.finalFetchReady = state.finalFetchReady
            out.finalFetchOut = state.finalFetchOut
            out.finalFetchPC = state.finalFetchPC
            out.done = state.done
          }
          else {
            stepF1(state, out)
            stepF2(state, out)
            stepF3(state, out)
            stepF4(state, out)
          }
        }

        def fetch(state: Rep[Frontend]): (Rep[Boolean], Rep[Boolean], Rep[InstructionS], Rep[Int]) = {
          // Output of F4

          // Output of F3
          val ready = state.instructionOutReady
          val result = state.instructionOut
          val pc = state.fetchPC

          // Output of F2

          // Output of F1
          val (done, initialFetchReady, initialFetch) = runF1(state)
          //val (btbFound, btbResponse) =

          (done, ready, result, pc)
        }
      }

      trait ReorderOps extends Dsl with ROBEntryOps with ROBOps with ArrayOps {
      }

      trait ExecuteOps extends Dsl with InstructionSOps with ArrayOps {
        def execute(tag: Rep[Int], regFile: Rep[Array[Int]], memory: Rep[Array[Int]]): Rep[Unit] = {
          if (tag == add) {
          }
          else if (tag == addi) {
          }
          else if (tag == mul) {
          }
          else if (tag == ldr) {
          }
          else if (tag == str) {
          }
          else if (tag == b) {
          }
          else if (tag == beq) {
          }
          else if (tag == bne) {
          }
          else if (tag == blt) {
          }
          else if (tag == bge) {
          }
        }
      }

      trait InterpBoom extends Dsl with StateOps {
        def run
          ( initialMemory: Rep[Array[Int]]
          , allocState: Rep[State]
          ): Rep[Int] =
        {
          var ticks = unit(0)

          while (ticks < 500) {
            // val done, ready, ins, pc = fetch(state.frontend)
            // val next = nextScheduled(state.reorder)
            // val () = execute(next, state.registerFile, state.memory)

            ticks += 1
          }

          ticks
        }
      }
    }
  }
}
