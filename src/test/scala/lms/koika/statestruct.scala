package lms.koika

import lms.core._
import lms.core.stub._
import lms.macros.SourceContext
import lms.macros.RefinedManifest

import lms.collection.mutable._

@virtualize
class StateStructTest extends TutorialFunSuite {
  val under = "structdemo/"

  override def exec(label: String, code: String, suffix: String = "c") =
    super.exec(label, code, suffix)

  override def check(label: String, code: String, suffix: String = "c") =
    super.check(label, code, suffix)

  case class Reg(id: Int)

  // TODO cwong: The arrays should have fixed size
  @CStruct case class State(
    // XXX cwong: Maybe this should be a dedicated [Registers] struct?
    registers: Array[Int],
    cache_val: Int,
    time: Int,
    mem: Array[Int],
  )

  abstract sealed class Instruction
  case class Add(dst: Reg, lsrc: Reg, rsrc: Reg) extends Instruction
  case class Branch(tst: Reg, target: Reg) extends Instruction
  case class Load(dst: Reg, offs: Int, src: Reg) extends Instruction
  case class Store(dst: Int, offs: Int, src: Int) extends Instruction
}
