package feh.tec.util

import scala.tools.nsc.interpreter._
import scala.tools.nsc.{Global, Settings}
import java.io.File
import scala.tools.reflect.ToolBox
import scala.util.Try

class RestrictedInterpreter(restrictions: Set[Restriction], additionalClasspath: Set[String])
{
//  System.setSecurityManager(new ScopedSecurityManager(restrictions))

  protected def iMainSettings = new Settings() $${
    s =>
      s.classpath.append(additionalClasspath.mkString(File.pathSeparator))
      s.usejavacp.value = true
  }

  /*protected */lazy val iMain = new IMain(iMainSettings)
  /*protected */lazy val iGlobal = iMain.global

  /*protected */lazy val sur = new InterpreterExprSurround(iGlobal)

  def interpret(line: String) = Try{
    val tree = sur.wrapSecure(line)
//    iGlobal.gen.evalOnce(tree, iMain("_root_"), iGlobal.currentUnit) todo
  }

// @see TreeGen.evalOnce
  // import i.sur.global.gen.CODE

}


class InterpreterExprSurround(/*protected */val global: Global) {
//  import global._
  import scala.reflect.runtime.universe
  import universe._

  protected val rMirror = runtimeMirror(getClass.getClassLoader)
  protected val rToolbox = rMirror.mkToolBox()
  protected val runtimeToGlobal = global.mkImporter(universe)

  def wrapSecure(line: String) = {

    val x = rToolbox.parse(line) match{
      case Block(stats, expr) => Block(surroundDefinitions(stats) :+ surroundResAndDef(expr): _*)
      case expr => surroundResAndDef(expr) // .asInstanceOf[Tree]
    }

    showRaw(x)
    runtimeToGlobal.importTree(x)
  }

  protected def surroundDefinitions(trees: List[Tree]): List[Tree] = trees match {
    case Nil => Nil
    case ValDef(mod, name, ttree, tree) :: tail =>
      ValDef(mod, name, ttree, surroundWithSecureScope(tree)) :: surroundDefinitions(tail)
    case DefDef(mod, name, tp, p, ttree, tree) :: tail =>
      DefDef(mod, name, tp, p, ttree, surroundWithSecureScope(tree)) :: surroundDefinitions(tail)
    case ClassDef(mod, name, tp, impl) :: tail =>
      ClassDef(mod, name, tp, surroundTemplateWithSecureScope(impl)) :: surroundDefinitions(tail)
    case ModuleDef(mod, name, impl) :: tail =>
      ModuleDef(mod, name, surroundTemplateWithSecureScope(impl)) :: surroundDefinitions(tail)
  }

  protected def surroundTemplateWithSecureScope(tree: Template): Template =
    Template(tree.parents, tree.self, tree.body map surroundWithSecureScope)
  protected def surroundWithSecureScope(tree: Tree): Tree =
    Apply(
      Select(
        TypeApply(
          Select(
            Apply(
              Select(
                Ident(newTermName("System")),
                newTermName("getSecurityManager")
              ), List()
            ), newTermName("asInstanceOf")
          ), List(Ident(newTermName("feh.tec.util.ScopedSecurityManager"))))
        , newTermName("secureScope")
      ), List(tree)) // Literal(Constant(()))

  protected def surroundResult(tree : Tree) = surroundWithSecureScope(tree)//todo ??

  protected def surroundResAndDef(tree: Tree) = surroundResult(surroundDefinitions(tree :: Nil).head)

    /*
showRaw(reify{ val x = 1 }.tree)
res11: String = Block(List(ValDef(Modifiers(), newTermName("x"), TypeTree(), Literal(Constant(1)))), Literal(Constant(())))

showRaw(reify{ def x(i: Int) = null }.tree)
res12: String = Block(List(DefDef(Modifiers(), newTermName("x"), List(), List(List(ValDef(Modifiers(PARAM), newTermName("i"), Ident(scala.Int), EmptyTree))), TypeTree(), Literal(Constant(null)))), Literal(Constant(())))

howRaw(reify{ implicit def x(i: Int) = null }.tree)
res13: String = Block(List(DefDef(Modifiers(IMPLICIT), newTermName("x"), List(), List(List(ValDef(Modifiers(PARAM), newTermName("i"), Ident(scala.Int), EmptyTree))), TypeTree(), Literal(Constant(null)))), Literal(Constant(())))

showRaw(reify{ 5 }.tree)
res15: String = Literal(Constant(5))

scala> showRaw(reify{ println("x") }.tree)
res16: String = Apply(Select(Ident(scala.Predef), newTermName("println")), List(Literal(Constant("x"))))

howRaw(reify{ var x=1  }.tree)
res18: String = Block(List(ValDef(Modifiers(MUTABLE), newTermName("x"), TypeTree(), Literal(Constant(1)))), Literal(Constant(())))

showRaw(reify{ class A  }.tree)
res17: String = Block(List(ClassDef(Modifiers(), newTypeName("A"), List(), Template(List(Ident(newTypeName("AnyRef"))), emptyValDef, List(DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))))))), Literal(Constant(())))

showRaw(reify{ trait A  }.tree)
res19: String = Block(List(ClassDef(Modifiers(ABSTRACT | INTERFACE | DEFAULTPARAM/TRAIT), newTypeName("A"), List(), Template(List(Ident(newTypeName("AnyRef"))), emptyValDef, List()))), Literal(Constant(())))

showRaw(reify{ type A = String }.tree)
res20: String = Block(List(TypeDef(Modifiers(), newTypeName("A"), List(), Select(Ident(scala.Predef), newTypeName("String")))), Literal(Constant(())))


showRaw(reify{ object X }.tree)
res21: String = Block(List(ModuleDef(Modifiers(), newTermName("X"), Template(List(Ident(newTypeName("AnyRef"))), emptyValDef, List(DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))))))), Literal(Constant(())))

     */

}

trait Restriction
trait BasicStringRestriction extends Restriction{
  def forbid: String => Boolean
}

trait FileRestriction extends Restriction
trait ReflectRestriction extends Restriction
trait SecurityRestriction extends Restriction
case class ExecRestriction(forbid: String => Boolean) extends BasicStringRestriction
case class PackageAccessRestriction(packages: String*) extends Restriction

//object SecureScope extends ScopedState[Boolean](false)

class ScopedSecurityManager(restrictions: Set[Restriction], enabledByDefault: Boolean = false) extends SecurityManager with ScopedInThreadState[Boolean]{
  def default = enabledByDefault

  def secureScope[R](f: => R): R = doWith(true)(f)

  private val execRestrictions = restrictions.collect{ case ExecRestriction(firbid) => firbid }
  override def checkExec(cmd: String){
    if(state)
      assert(!execRestrictions.exists(_(cmd)), s"exec has been forbidden in scope: $cmd")

    super.checkExec(cmd)
  }

//  override def checkMemberAccess(clazz: Class[_], which: Int): Unit = super.checkMemberAccess(clazz, which)
  private val packageAccessRestrictions = restrictions.collect{ case PackageAccessRestriction(pckgs) => pckgs }
  override def checkPackageAccess(pkg: String): Unit = {
    if(state)
      assert(!packageAccessRestrictions.exists(pkg.startsWith), s"access to package $pkg has been forbidden in scope")

    super.checkPackageAccess(pkg)
  }
  override def doWith[R](t: Boolean)(r: => R): R = {
    if(state) throw new SecurityException("cannot create secure scopes while within a restricted one")
    else super.doWith(t)(r)
  }
}
