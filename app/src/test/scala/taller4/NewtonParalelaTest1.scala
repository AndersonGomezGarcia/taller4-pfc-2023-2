package taller4

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatest.exceptions.TestFailedException
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NewtonParalelaTest1 extends AnyFunSuite{

  val ObjNew = new NewtonParalela()
  val expr1 = Suma( Atomo ('x') , Numero (2) )
  val expr2 = Prod( Atomo ('x') ,  Atomo ('x') )
  val expr3 = Suma( expr1 , Expo ( expr2 , Numero ( 5 ) ) )
  val expr4 = Logaritmo(Atomo('x'))
  val expr5 = Prod ( Div ( expr1 , expr2 ) , Resta ( expr3 , expr4 ) )
  val expr6 = Expo(Atomo('x'), Numero(3))

  test("Newton Mostrar 1"){
    assert(ObjNew.mostrar(expr1) == "(x + 2.0)")
  }

  test("Newton Mostrar 2"){
    assert(ObjNew.mostrar(expr2) == "(x * x)")
  }

  test("Newton Mostrar 3"){
    assert(ObjNew.mostrar(expr3) == "((x + 2.0) + ((x * x) ^ 5.0))")
  }

  test("Newton Mostrar 4"){
    assert(ObjNew.mostrar(expr4) == "(lg(x))")
  }

  test("Newton Mostrar 5"){
    assert(ObjNew.mostrar(expr5) == "(((x + 2.0) / (x * x)) * (((x + 2.0) + ((x * x) ^ 5.0)) - (lg(x))))")
  }

  test("Newton Mostrar 6"){
    assert(ObjNew.mostrar(expr6) == "(x ^ 3.0)")
  }

}
