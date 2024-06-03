package benchmarking

import org.scalameter._
import taller4.{Atomo, Div, Expo, Logaritmo, Newton, NewtonParalela, Numero, Prod, Resta, Suma}

class Benchmarking {
  def NewtonBenchmarking(): Unit = {


    val objSecuencial= new Newton()
    val objParalelizado = new NewtonParalela()
    val e5 = Suma(Resta(Prod(Atomo('x'), Atomo('x')), Numero(1.0)), Prod(Numero(2.0), Atomo('x')))
    val timeSecuencial = config(

      Key.exec.minWarmupRuns := 10,
      Key.exec.maxWarmupRuns := 10,
      Key.exec.benchRuns := 20,
      Key.verbose := true,
    ) withWarmer new Warmer.Default measure {
      objSecuencial.raizNewton(e5, Atomo('x'), 50000000.0, objSecuencial.buenaAprox)
    }
    val timeParalela = config(

      Key.exec.minWarmupRuns := 10,
      Key.exec.maxWarmupRuns := 10,
      Key.exec.benchRuns := 20,
      Key.verbose := true,
    ) withWarmer new Warmer.Default measure {
      objParalelizado.raizNewtonParalelo(e5, Atomo('x'), 50000000.0, objParalelizado.buenaAprox)
    }

    val timeParalelaDer = config(

      Key.exec.minWarmupRuns := 10,
      Key.exec.maxWarmupRuns := 10,
      Key.exec.benchRuns := 20,
      Key.verbose := true,
    ) withWarmer new Warmer.Default measure {
      objParalelizado.derivar(Suma(Prod(Expo(Suma(Atomo('x'), Prod(Numero(3.0), Atomo('y'))), Numero(2.0)), Logaritmo(Resta(Atomo('z'), Div(Numero(4.0), Atomo('w'))))), Div(Resta(Expo(Atomo('v'), Numero(2.0)), Prod(Numero(5.0), Atomo('u'))), Suma(Logaritmo(Atomo('t')), Prod(Numero(6.0), Atomo('s'))))), Atomo('x'))
    }
    val timeSecuencialDer = config(
      Key.exec.minWarmupRuns := 10,
      Key.exec.maxWarmupRuns := 10,
      Key.exec.benchRuns := 20,
      Key.verbose := true,
    ) withWarmer new Warmer.Default measure {
      objSecuencial.derivar(Suma(Prod(Expo(Suma(Atomo('x'), Prod(Numero(3.0), Atomo('y'))), Numero(2.0)), Logaritmo(Resta(Atomo('z'), Div(Numero(4.0), Atomo('w'))))), Div(Resta(Expo(Atomo('v'), Numero(2.0)), Prod(Numero(5.0), Atomo('u'))), Suma(Logaritmo(Atomo('t')), Prod(Numero(6.0), Atomo('s'))))), Atomo('x'))
    }
    println(s"Tiempo de ejecución S: $timeSecuencial")
    println(s"Tiempo de ejecución P: $timeParalela")
    println(s"Tiempo de ejecución P der: $timeParalelaDer")
    println(s"Tiempo de ejecución S  der: $timeSecuencialDer")

  }
}