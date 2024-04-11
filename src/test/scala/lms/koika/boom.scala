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
  case object UnsignedByte extends Size
  case object UnsignedShort extends Size

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
  case class Load(rd: Reg, rs: Reg, offs: Int, size: Size) extends Instruction
  case class Store(rs1: Reg, offs: Int, rs2: Reg, size: Size) extends Instruction
}

@virtualize
class BoomTests extends TutorialFunSuite {
  val under = "boom_naive_"

  // Naive approach, stage the entire cycle pipeline loop into the residue.
  object Naive {
    import RiscVStripped._

    @CStruct
    case class Port
      ( initial: Int
      , read: Int
      , write: Int
      )

    // CR cam: Caches
    // CR cam: superscalar fetching

    @CStruct
    case class Frontend
      ( stage: Int
      )

    @CStruct
    case class State
      ( ticks: Int
      , regFile: Array[Int]
      , instructionMemory: Array[Instruction]
      , programMemory: Array[Int]
      , fetchBuffer: Array[Instruction]
      )

    trait PortDsl extends Dsl with PortOps {
      def flushPort(p: Rep[Port]): Rep[Unit] = {
        p.write = p.initial
      }

      def freezePort(p: Rep[Port]): Rep[Unit] = {
        p.write = p.read
      }

      def updatePort(p: Rep[Port]): Rep[Unit] = {
        p.read = p.write
      }
    }

    trait InterpDsl extends PortDsl with StateOps {
    }
  }
}
