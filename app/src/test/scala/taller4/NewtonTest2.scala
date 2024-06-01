package taller4

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatest.exceptions.TestFailedException
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NewtonTest2 extends AnyFunSuite{
  val ObjNew = new Newton()
  val expr1 = Suma( Atomo ('x') , Numero (2) )
  val expr2 = Prod( Atomo ('x') ,  Atomo ('x') )
  val expr3 = Suma( expr1 , Expo ( expr2 , Numero ( 5 ) ) )
  val expr4 = Logaritmo(Atomo('x'))
  val expr5 = Prod ( Div ( expr1 , expr2 ) , Resta ( expr3 , expr4 ) )
  val expr6 = Expo(Atomo('x'), Numero(3))
  val expr7 = ObjNew.derivar(expr6, Atomo ('x'))
  val expr8 = ObjNew.derivar( expr2 , Atomo ('x'))
  val expr9 = ObjNew.derivar( expr2 , Atomo ('y'))
  val expr10 = ObjNew.derivar (Suma( Atomo ('k') , Prod (Numero ( 3.0) , Atomo ('x'))), Atomo ('x'))

  test("Newton Derivar 1"){
    assert(ObjNew.mostrar(expr7) == "((x ^ 3.0) * (((1.0 * 3.0) / x) + (0.0 * (lg(x)))))")
  }

  test("Newton Derivar 2"){
    assert(ObjNew.mostrar(expr8) == "((1.0 * x) + (x * 1.0))")
  }

  test("Newton Derivar 3"){
    assert(ObjNew.mostrar(expr9) == "((0.0 * x) + (x * 0.0))")
  }

  test("Newton Derivar 4"){
    assert(ObjNew.mostrar(expr10) == "(0.0 + ((0.0 * x) + (3.0 * 1.0)))")
  }

  test("Newton Derivar 5"){
    assert(ObjNew.mostrar(ObjNew.derivar(Numero(5.0), Atomo('x'))) == "0.0")
  }

}
