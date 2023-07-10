package lms.koika

import lms.core._
import lms.core.stub._
import lms.macros.SourceContext
import lms.macros.RefinedManifest

import lms.collection.mutable._

@virtualize
class StateStructTest extends TutorialFunSuite {
  val under = "structdemo_"

  val MAX_STEPS = 1000
  val NUM_REGS = 3

  override def exec(label: String, code: String, suffix: String = "c") =
    super.exec(label, code, suffix)

  override def check(label: String, code: String, suffix: String = "c") =
    super.check(label, code, suffix)

  case class Reg(id: Int)
  def r(id: Int): Reg = Reg(id)

  abstract sealed class Instruction
  case class Add(dst: Reg, lsrc: Reg, rsrc: Reg) extends Instruction
  case class JumpZ(tst: Reg, target: Int) extends Instruction
  case class Load(dst: Reg, offs: Int, src: Reg) extends Instruction
  case class Store(src: Reg, dst: Reg, offs: Int) extends Instruction

  type Prog = List[Instruction]

  trait Interp extends Dsl with StructOps {
    // TODO cwong: The arrays should have fixed size
    @CStruct case class State(
      // XXX cwong: Maybe this should be a dedicated [Registers] struct?
      registers: Array[Int],
      cache_val: Int,
      time: Int,
      mem: Array[Int],
    )

    def run(prog: Prog, pc: Int, s: Pointer[State]): Rep[Unit] =
      if (pc < prog.length) {
        prog(pc) match {
          case Add(dst, lsrc, rsrc) => {
            s.registers(dst.id) = s.registers(lsrc.id) + s.registers(rsrc.id)
            run(prog, pc+1, s)
          }
          case JumpZ(tst, target) => {
            if (s.registers(tst.id) == unit(0)) {
              run(prog, target, s)
            }
            else {
              run(prog, pc+1, s)
            }
          }
          case Load(dst, offs, src) => {
            s.registers(dst.id) = s.mem(offs + s.registers(src.id))
            run(prog, pc+1, s)
          }
          case Store(src, dst, offs) => {
            s.mem(offs + s.registers(src.id)) = s.registers(dst.id)
            run(prog, pc+1, s)
          }
        }
      }

    def go(prog: Prog, s: Pointer[State]) =
      run(prog, 0, s)
  }

  abstract class Driver[In: Manifest, Out: Manifest] extends DslDriverC[In,Out] {q =>
    val main: String = """
int main(int argc, char *argv[]) {
  if (argc != 2) {
    printf("usage: %s <arg>\n", argv[0]);
    return 0;
  }
  return 0;
}
"""

    override val codegen = new DslGenC with CCodeGenStruct {
      val IR: q.type = q

      override def emitAll(
          g: lms.core.Graph,
          name: String
      )(m1: Manifest[_], m2: Manifest[_]): Unit = {
        val ng = init(g)
        val efs = "" //quoteEff(g.block.ein)
        val stt = dce.statics.toList.map(quoteStatic).mkString(", ")
        prepareHeaders
        emitln("""
    |/*****************************************
    |Emitting C Generated Code
    |*******************************************/
    """.stripMargin)
        val src = run(name, ng)
        emitDefines(stream)
        emitHeaders(stream)
        emitFunctionDecls(stream)
        emitDatastructures(stream)
        emitFunctions(stream)
        emitInit(stream)
        emitln(s"\n/**************** $name ****************/")
        emit(src)
        emitln("""
    |/*****************************************
    |End of C Generated Code
    |*******************************************/
    |""".stripMargin)
        emit(main)
      }
    }
  }

  test("basic") {
    val snippet = new Driver[Array[Int], Array[Int]] with Interp {
      val prog = List(Add(r(0), r(0), r(0)), JumpZ(r(0),2))
      def snippet(initial_memory: Rep[Array[Int]]): Rep[Array[Int]] = {
        val s = Pointer.local[State]
        s.mem = initial_memory
        run(prog, 0, s)
        return s.mem
      }
    }
    check("basic", snippet.code, "c")
  }
}
