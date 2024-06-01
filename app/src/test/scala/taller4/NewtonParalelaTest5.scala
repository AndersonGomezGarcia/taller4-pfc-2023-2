package taller4

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatest.exceptions.TestFailedException
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NewtonParalelaTest5 extends AnyFunSuite {

  val e1 = Resta(Prod(Atomo('x'), Atomo('x')), Numero(2.0)) // f(x) = x^2 - 2
  val e2 = Resta(Prod(Atomo('x'), Atomo('x')), Numero(4.0)) // f(x) = x^2 - 4
  val e3 = Suma(Resta(Prod(Atomo('x'), Atomo('x')), Numero(4.0)), Prod(Numero(3.0), Atomo('x'))) // f(x) = x^2 - 4 + 3x
  val e4 = Resta(Prod(Atomo('x'), Atomo('x')), Numero(9.0)) // f(x) = x^2 - 9
  val e5 = Suma(Resta(Prod(Atomo('x'), Atomo('x')), Numero(1.0)), Prod(Numero(2.0), Atomo('x')))
  val objRaiz = new NewtonParalela()

  val raiz1 = objRaiz.raizNewton(e1, Atomo('x'), 2.0, objRaiz.buenaAprox)
  val raiz2 = objRaiz.raizNewton(e2, Atomo('x'), 2.0, objRaiz.buenaAprox)
  val raiz3 = objRaiz.raizNewton(e3, Atomo('x'), 2.0, objRaiz.buenaAprox)
  val raiz4 = objRaiz.raizNewton(e4, Atomo('x'), 2.0, objRaiz.buenaAprox)
  val raiz5 = objRaiz.raizNewton(e5, Atomo('x'), 2.0, objRaiz.buenaAprox)

  test("Newton Raiz 1"){
    assert(raiz1 == 1.4142156862745099)
  }

  test("Newton Raiz 2"){
    assert(raiz2 == 2.0)
  }

  test("Newton Raiz 3"){
    assert(raiz3 == 1.0000029768726761)
  }

  test("Newton Raiz 4"){
    assert(raiz4 == 3.000015360039322)
  }

  test("Newton Raiz 5"){
    assert(raiz5 == 0.41421378004719756)
  }

}
