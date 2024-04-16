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

  abstract sealed class Size
  case object Byte extends Size
  case object Short extends Size
  case object Word extends Size

  // CR cam: Do we actually need dedicated sign handling?
  abstract sealed class Signedness
  case object Signed extends Signedness
  case object Unsigned extends Signedness

  abstract sealed class Cmp
  case object Eq extends Cmp
  case object Ne extends Cmp
  case object Lt extends Cmp
  case object Ge extends Cmp
  case object UnsignedLt extends Cmp
  case object UnsignedGe extends Cmp

  abstract sealed class Instruction
  case class Add(rd: Reg, rs1: Reg, rs2: Reg) extends Instruction
  case class Addi(rd: Reg, rs1: Reg, imm: Int) extends Instruction
  case class Mul(rd: Reg, rs1: Reg, rs2: Reg) extends Instruction
  // ldr_ rd, [rs, offs]
  case class Load(size: Size, sign: Signedness, rd: Reg, rs: Reg, offs: Int) extends Instruction
  // str_ rv, [rd, offs]
  case class Store(size: Size, rd: Reg, offs: Int, rv: Reg) extends Instruction
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

      val ldrb = iota;
      val ldsb = iota;
      val ldrh = iota;
      val ldsh = iota;
      val ldr = iota;

      val strb = iota;
      val strh = iota;
      val str = iota;

      val b = iota;
      val beq = iota;
      val bne = iota;
      val blt = iota;
      val bge = iota;
      val bltu = iota;
      val bgeu = iota;
    }

    import InsTags._

    val noop = InstructionS(addi, 0, 0, 0)

    object InstructionOps {
      def encode(i: Instruction): InstructionS = i match {
        case Add(rd: Reg, rs1: Reg, rs2: Reg) => InstructionS(add, rd.i, rs1.i, rs2.i)
        case Addi(rd: Reg, rs1: Reg, imm: Int) => InstructionS(addi, rd.i, rs1.i, imm)
        case Mul(rd: Reg, rs1: Reg, rs2: Reg) => InstructionS(mul, rd.i, rs1.i, rs2.i)
        case Load(Byte, Unsigned, rd: Reg, rs: Reg, offs: Int) => InstructionS(ldrb, rd.i, rs.i, offs)
        case Load(Byte, Signed, rd: Reg, rs: Reg, offs: Int) => InstructionS(ldsb, rd.i, rs.i, offs)
        case Load(Short, Unsigned, rd: Reg, rs: Reg, offs: Int) => InstructionS(ldrh, rd.i, rs.i, offs)
        case Load(Short, Signed, rd: Reg, rs: Reg, offs: Int) => InstructionS(ldsh, rd.i, rs.i, offs)
        case Load(Word, _, rd: Reg, rs: Reg, offs: Int) => InstructionS(ldr, rd.i, rs.i, offs)
        case Store(Byte, rd: Reg, offs: Int, rv: Reg) => InstructionS(strb, rd.i, offs, rv.i)
        case Store(Short, rd: Reg, offs: Int, rv: Reg) => InstructionS(strh, rd.i, offs, rv.i)
        case Store(Word, rd: Reg, offs: Int, rv: Reg) => InstructionS(str, rd.i, offs, rv.i)
        case B(None, rs1: Reg, rs2: Reg, offs: Int) => InstructionS(b, rs1.i, rs2.i, offs)
        case B(Some(Eq), rs1: Reg, rs2: Reg, offs: Int) => InstructionS(beq, rs1.i, rs2.i, offs)
        case B(Some(Ne), rs1: Reg, rs2: Reg, offs: Int) => InstructionS(bne, rs1.i, rs2.i, offs)
        case B(Some(Lt), rs1: Reg, rs2: Reg, offs: Int) => InstructionS(blt, rs1.i, rs2.i, offs)
        case B(Some(Ge), rs1: Reg, rs2: Reg, offs: Int) => InstructionS(bge, rs1.i, rs2.i, offs)
        case B(Some(UnsignedLt), rs1: Reg, rs2: Reg, offs: Int) => InstructionS(bltu, rs1.i, rs2.i, offs)
        case B(Some(UnsignedGe), rs1: Reg, rs2: Reg, offs: Int) => InstructionS(bgeu, rs1.i, rs2.i, offs)
      }
    }

    // CR cam: reinstate caches
    // CR cam: superscalar fetching

    // For simplicity, we assume that instruction packets have size one (in
    // other words, that instruction fetch loads exactly one instruction at a
    // time).

    // CR-someday cam: The actual
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
        // CR cam: this is basically Option[Int]
      , fetchPCReady: Boolean
      , fetchPC: Int
        // The proper BOOM uses output queues. However, because we aren't doing
        // superscalar packet fetch, we can get away with a simple `out` intead.
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
      )

    @CStruct
    case class State
      ( ticks: Int
      , frontend: Frontend
      , regFile: Array[Int]
      , rat: Array[Int]
      , programMemory: Array[Int]
      )

    class BOOMRunner(program: Array[Instruction], btbSize: Int) {
      // Because [StructOps] uses mutation under the hood and we don't expose any
      // primitives for allocation, we need to be very careful about how the
      // stages compose to avoid accidentally leaking information into a further
      // stage. The ordering is very tricky, because the information flow graph
      // is inherently cyclic (fetch->decode->execute->fetch).
      //
      // To ensure that stages don't inadvertently interfere with each other, the
      // residue will keep two separate state structs, one "active" and one
      // "upcoming", with the active state being (spiritually) read-only and
      // the upcoming state being write-only. Then, at the end of each cycle,
      // we swap the active and upcoming states.

      trait FrontendInterp extends Dsl with FrontendOps with BTBOps {
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
              // CR cam: this makes the residue impossible to analyze
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
      }

      trait ReorderOps extends Dsl with ROBEntryOps with ROBOps with ArrayOps {
      }
    }
  }
}
