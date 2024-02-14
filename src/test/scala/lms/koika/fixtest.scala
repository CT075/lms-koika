package lms.koika

import lms.core.stub._
import lms.core.virtualize
import lms.macros.SourceContext

@virtualize
class FixTest extends TutorialFunSuite {
  val under = "fixtest_"

  override def exec(label: String, code: String, suffix: String = "c") =
    super.exec(label, code, suffix)

  override def check(label: String, code: String, suffix: String = "c") =
    super.check(label, code, suffix)

  trait DslFix extends Dsl {
    def factX(f: Rep[Int => Int])(n: Rep[Int]): Rep[Int] = {
      if (n == 0) {
        unit(1)
      }
      else {
        f(n-1)
      }
    }

    def fix(f: (Rep[Int => Int] => Rep[Int] => Rep[Int])): Rep[Int => Int] = {
      topFun {(x: Rep[Int]) => {f(fix(f))(x)}}
    }
  }

  test("1") {
    val snippet = new DslDriverC[Int, Int] with DslFix {
      def snippet(x: Rep[Int]): Rep[Int] = fix(factX)(x)
    }
    check("fix1", snippet.code)
  }
}
