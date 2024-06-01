package taller4

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatest.exceptions.TestFailedException
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NewtonParalelaTest3 extends AnyFunSuite {

  val ObjNew = new NewtonParalela()
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
  ObjNew.mostrar ( Numero ( 5.0 ) )
  ObjNew.evaluar ( Numero ( 5.0 ) , Atomo ('x') , 1.0 )
  ObjNew.mostrar ( Atomo ( 'x' ) )
  ObjNew.evaluar ( Atomo ( 'x' ) , Atomo ( 'x' ) , 5.0 )
  ObjNew.mostrar (Suma( expr1 , expr2 ) )
  ObjNew.evaluar (Suma( expr1 , expr2 ) , Atomo ( 'x' ) , 5.0 )
  ObjNew.mostrar ( Prod ( expr1 , expr2 ) )
  ObjNew.evaluar ( Prod ( expr1 , expr2 ) , Atomo ( 'x' ) , 5.0 )
  ObjNew.mostrar ( Resta ( expr1 , expr2 ) )
  ObjNew.evaluar ( Resta ( expr1 , expr2 ) , Atomo ('x') , 5.0 )
  ObjNew.mostrar ( Div ( expr1 , expr2 ) )
  ObjNew.evaluar ( Div ( expr1 , expr2 ) , Atomo ('x') , 5.0)
  ObjNew.mostrar ( Expo ( expr1 , expr2 ) )
  ObjNew.evaluar ( Expo ( expr1 , expr2 ) , Atomo ( 'x' ) , 5.0 )
  ObjNew.mostrar ( Logaritmo ( expr1 ) )
  ObjNew.evaluar ( Logaritmo ( expr1 ) , Atomo ( 'x' ) , 5.0 )


  test("Newton Evaluar 1"){
    assert(ObjNew.evaluar(Numero(5.0), Atomo('x'), 1.0) == 5.0)
  }

  test("Newton Evaluar 2"){
    assert(ObjNew.evaluar(Atomo('x'), Atomo('x'), 5.0) == 5.0)
  }

  test("Newton Evaluar 3"){
    assert(ObjNew.evaluar(Prod( expr1 , expr2), Atomo('x'), 5.0) == 175.0)
  }

  test("Newton Evaluar 4"){assert(ObjNew.evaluar(Div(expr1,expr2),Atomo('x') ,5.0) == 0.28)
  }

  test("Newton Evaluar 5"){assert(ObjNew.evaluar(Logaritmo(expr1),Atomo('x') , 5.0) == 1.9459101490553132)
  }

}
