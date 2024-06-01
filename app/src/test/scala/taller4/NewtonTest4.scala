package taller4


import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatest.exceptions.TestFailedException
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NewtonTest4 extends AnyFunSuite{
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
  val expr11 = Suma(Resta(Prod(Prod(Atomo('x'), Atomo('x')), Prod(Atomo('x'), Atomo('x'))), Prod(Prod(Numero(2.0), Atomo('x')), Atomo('x'))), Numero(1.0)) // f(x) = (x^4 - 2x^2) + 1
  val expr12 = ObjNew.derivar(expr11, Atomo ('x')) // f'(x) = 4x^3 - 4x
  test("Newton Limpiar 1"){
    assert(ObjNew.limpiar(ObjNew.derivar(Suma( Atomo('k') , Prod (Numero(3.0), Atomo('x'))), Atomo('x'))) == Numero(3.0))
  }

  test("Newton Limpiar 2"){
    assert(ObjNew.limpiar(expr12) == Resta(Suma(Prod(Suma(Atomo('x'), Atomo('x')), Prod(Atomo('x'), Atomo('x'))), Prod(Prod(Atomo('x'), Atomo('x')), Suma(Atomo('x'), Atomo('x')))), Suma(Prod(Numero(2.0), Atomo('x')), Prod(Numero(2.0), Atomo('x')))))
  }

  test("Newton Limpiar 3")
  {
    assert(ObjNew.limpiar(expr9) == Numero(0.0))
  }

  test("Newton Limpiar 4"){
    assert(ObjNew.limpiar(expr8) == Suma(Atomo('x'), Atomo('x')))
  }

  test("Newton Limpiar 5"){
    assert(ObjNew.limpiar(expr7) == Prod(Expo(Atomo('x'), Numero(3.0)), Div(Numero(3.0), Atomo('x'))))
  }
}
