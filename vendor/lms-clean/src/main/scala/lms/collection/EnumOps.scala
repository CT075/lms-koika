package lms.collection.mutable

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.SourceContext
import lms.macros.RefinedManifest


import language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context
import scala.util.matching.Regex

object CEnumMatch_Impl {
  import scala.reflect.runtime.universe._

  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val List(a) = annottees
    a.tree match {
      case q"case class $name(..${fields: Seq[ValDef]})" =>
        val manifestName = internal.reificationSupport.freshTermName(name.toString+"Manifest")
        val fieldDecls = fields.map { f => q"""(${f.name.toString}, manifest[${f.tpt}])""" }
        val res = c.Expr(q"""
          case class $name(..$fields)
          implicit val $manifestName = new RefinedManifest[$name] {
            def fields: List[(String, Manifest[_])] = List(..$fieldDecls)
            def runtimeClass = classOf[$name]
            override def name = Some(${name.toString})
          }
        """)
        res
    }
  }
}
