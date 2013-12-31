package com.softwaremill.debug

import language.experimental.macros

import reflect.macros.Context

trait ObjMapper[TFrom, TTo] {
  def mapValue(obj: TFrom): TTo
}


private abstract class Helper[C <: Context, TFrom, TTo](val c: C) {

  import c.universe._

  protected def fromType: c.Type

  protected def toType: c.Type

  private def fromTypeName = fromType.typeSymbol.name

  private def toTypeName = toType.typeSymbol.name

  def mapValueBody: c.Expr[TTo] = ???

  def caseClassParamsOf(typ: c.Type) = {
    c.echo(c.enclosingPosition, s"typ: $typ")
    c.echo(c.enclosingPosition, s"${typ.declarations}")
    val cTor = typ.declaration(nme.CONSTRUCTOR).asMethod
    val params: List[Symbol] = cTor.paramss.flatten
    params
  }

  def checkSuperSet = {
    // Retrieve the params for the From Type
    val fromParams = caseClassParamsOf(fromType)

    // Find the parameters of the Published case class
    val toParams = caseClassParamsOf(toType)


    // Find the first parameter name that doesn't match
    val wrongProperties = toParams.filter {
      toParam =>
        val hasFromBrother = fromParams.exists {
          fromParam =>
            toParam.name.equals(fromParam.name) && toParam.typeSignature.equals(fromParam.typeSignature)
        }
        !hasFromBrother
    }

    if (!wrongProperties.isEmpty) {
      val offendingProperties = wrongProperties.map(_.name).mkString(",")
      c.abort(c.enclosingPosition, s"Could not create ObjMapper[$fromTypeName, $toTypeName], properties don't match: ${offendingProperties}")
    }
  }


}

object DebugMacros {

  def strictMapper[TFrom, TTo](): ObjMapper[TFrom, TTo] = macro strictMapperImpl[TFrom, TTo]

  private def mkHelper[TFrom: c.WeakTypeTag, TTo: c.WeakTypeTag](c: Context) = new Helper[c.type, TFrom, TTo](c) {
    val fromType = c.weakTypeOf[TFrom]
    val toType = c.weakTypeOf[TTo]
  }

  def strictMapperImpl[TFrom: c.WeakTypeTag, TTo: c.WeakTypeTag](c: Context)(): c.Expr[ObjMapper[TFrom, TTo]] = {
    import c.universe._
    val helper = mkHelper[TFrom, TTo](c)
    helper.checkSuperSet

    val body = helper.mapValueBody

    reify {
      new ObjMapper[TFrom, TTo] {
        def mapValue(obj: TFrom): TTo = body.splice
      }
    }
  }

  def hello(): Unit = macro hello_impl

  def hello_impl(c: Context)(): c.Expr[Unit] = {
    import c.universe._
    reify {
      println("Hello World!")
    }
  }

  def printparam(param: Any): Unit = macro printparam_impl

  def printparam_impl(c: Context)(param: c.Expr[Any]): c.Expr[Unit] = {
    import c.universe._
    reify {
      println(param.splice)
    }
  }

  def debug1(param: Any): Unit = macro debug1_impl

  def debug1_impl(c: Context)(param: c.Expr[Any]): c.Expr[Unit] = {
    import c.universe._
    val paramRep = show(param.tree)
    val paramRepTree = Literal(Constant(paramRep))
    val paramRepExpr = c.Expr[String](paramRepTree)
    reify {
      println(paramRepExpr.splice + " = " + param.splice)
    }
  }

  def debug(params: Any*): Unit = macro debug_impl

  def debug_impl(c: Context)(params: c.Expr[Any]*): c.Expr[Unit] = {
    import c.universe._

    val trees = params.map {
      param =>
        param.tree match {
          // Keeping constants as-is
          // The c.universe prefixes aren't necessary, but otherwise Idea keeps importing weird stuff ...
          case c.universe.Literal(c.universe.Constant(const)) => {
            val reified = reify {
              print(param.splice)
            }
            reified.tree
          }
          case _ => {
            val paramRep = show(param.tree)
            val paramRepTree = Literal(Constant(paramRep))
            val paramRepExpr = c.Expr[String](paramRepTree)
            val reified = reify {
              print(paramRepExpr.splice + " = " + param.splice)
            }
            reified.tree
          }
        }
    }

    // Inserting ", " between trees, and a println at the end.
    val separators = (1 to trees.size - 1).map(_ => (reify {
      print(", ")
    }).tree) :+ (reify {
      println()
    }).tree
    val treesWithSeparators = trees.zip(separators).flatMap(p => List(p._1, p._2))

    c.Expr[Unit](Block(treesWithSeparators.toList, Literal(Constant(()))))
  }
}
