package lms.koika

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.core.Backend._

import java.io.{PrintStream}

// FIXME: Currently, we do not thread the state through any generated
// functions, which means the [state] variable is only in-scope for the main
// [snippet] function.

trait StateOps extends Base with ArrayOps {
  val typeName: String

  var fields_rev: List[(String, Manifest[_])] = List()

  final case class State()

  class Field[T: Manifest](val name: String) {
    fields_rev = (name, manifest[T]) :: fields_rev

    // TODO: It would be nicer if we could just read/write to a value of type
    // [Field] directly
    def v: Rep[T] =
      Wrap[T](
        Adapter.g.reflectRead
          ("state_get", Unwrap(name))
          // TODO: this doesn't seem correct
          (Adapter.CTRL))

    def v_=(vl: Rep[T]): Unit =
      Adapter.g.reflectWrite("state_set", Unwrap(name), Unwrap(vl))(Adapter.CTRL)
  }

  def init_state(): Rep[State] =
    Wrap[State](Adapter.g.reflectMutable("state_init", Unwrap(typeName)))
}

trait CCodeGenState extends ExtendedCCodeGen {
  val stateVarName: String

  override def traverse(n: Node): Unit = n match {
    case n @ Node(s, "state_set", List(Const(name: String), v), _) =>
      esln"$stateVarName->$name = $v;"
    case n @ Node(s, "state_init", List(Const(typeName: String)), _) =>
      // TODO: free
      esln"struct $typeName *$stateVarName = calloc(1, sizeof(struct $typeName))"
    case _ => super.traverse(n)
  }

  override def shallow(n: Node): Unit = n match {
    case n @ Node(s, "state_get", List(Const(name: String)), _) =>
      es"$stateVarName->$name"
    case _ => super.shallow(n)
  }
}

@virtualize
class ImplicitStateTest extends TutorialFunSuite {
  val under = "implicitstate_"

  override def exec(label: String, code: String, suffix: String = "c") =
    super.exec(label, code, suffix)

  override def check(label: String, code: String, suffix: String = "c") =
    super.check(label, code, suffix)

  abstract class DslDriverX[A: Manifest, B: Manifest] extends DslDriverC[A, B] { q =>
    val main: String = ""

    override val codegen = new DslGenC with CCodeGenState {
      val IR: q.type = q

      override val stateVarName = "state"

      override def emitAll(
          g: lms.core.Graph,
          name: String
      )(m1: Manifest[_], m2: Manifest[_]): Unit = {
        val ng = init(g)
        val efs = ""
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

  test("basic_state_is_OK") {
    val driver = new DslDriverX[Array[Int], Int] with StateOps { q =>
      override val typeName = "state_t"

      val scratch = new Field[Int]("scratch")

      @virtualize
      def snippet(arg: Rep[Array[Int]]) : Rep[Int] = {
        // FIXME: This isn't getting emitted, probably being optimized out
        val state = init_state()

        scratch.v = unit(0)

        /* gives error: [not found: value SourceContext], which doesn't make
         * any sense. Maybe a macro problem?
        for (i <- (0 until arg.length)) {
          scratch.v = scratch.v + arg(i)
        }
        */

        scratch.v
      }
    }

    check("basic_state", driver.code, "c")
  }
}
