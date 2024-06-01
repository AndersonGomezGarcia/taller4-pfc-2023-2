package taller4

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration._

class NewtonParalela {

  def mostrar(e: Expr): String = e match {

    // Los primeros no se paralelizan dado que no tienen recursividad

    case Numero(d) => d.toString
    case Atomo(x) => x.toString

    // En los siguientes casos se paraleliza la evaluación de las subexpresiones
    // Se emplea Future para poder ejecutar estas tareas en paralelo, dado que ambos calculos son realizados
    // al mismo tiempo en diferentes hilos de ejecucion

    case Suma(e1, e2) =>
      val f1 = Future { mostrar(e1) }
      val f2 = Future { mostrar(e2) }
      val resultado = for { // Se espera a que ambos resultados esten listos para concatenarlos
        r1 <- f1
        r2 <- f2
      } yield s"($r1 + $r2)"
      Await.result(resultado, Duration.Inf)

    case Prod(e1, e2) =>
      val f1 = Future { mostrar(e1) }
      val f2 = Future { mostrar(e2) }
      val resultado = for {
        r1 <- f1
        r2 <- f2
      } yield s"($r1 * $r2)"
      Await.result(resultado, Duration.Inf)

    case Resta(e1, e2) =>
      val f1 = Future { mostrar(e1) }
      val f2 = Future { mostrar(e2) }
      val resultado = for {
        r1 <- f1
        r2 <- f2
      } yield s"($r1 - $r2)"
      Await.result(resultado, Duration.Inf)

    case Div(e1, e2) =>
      val f1 = Future { mostrar(e1) }
      val f2 = Future { mostrar(e2) }
      val resultado = for {
        r1 <- f1
        r2 <- f2
      } yield s"($r1 / $r2)"
      Await.result(resultado, Duration.Inf)


    case Expo(e1, e2) =>
      val f1 = Future { mostrar(e1) }
      val f2 = Future { mostrar(e2) }
      val resultado = for {
        r1 <- f1
        r2 <- f2
      } yield s"($r1 ^ $r2)"
      Await.result(resultado, Duration.Inf)

    //Se paraleliza por el posible costo computacional que conlleva evaluar la subexpresion
    case Logaritmo(e1:Expr) =>
      val f1: Future[String] = Future {
        mostrar(e1)
      }
      val resultado = for {
        r1 <- f1
      } yield s"(lg($r1))"
      Await.result(resultado, Duration.Inf)
  }


  def derivar(f: Expr, a: Atomo): Expr = f match {

    // Los primeros no se paralelizan dado que no tienen recursividad
    case Numero(_) => Numero(0)
    case Atomo(x) if x == a.x => Numero(1)
    case Atomo(_) => Numero(0)

    // En los siguientes casos se paraleliza la derivación de las subexpresiones
    // Se emplea Future para poder ejecutar estas tareas en paralelo, dado que ambos calculos son realizados
    // al mismo tiempo en diferentes hilos de ejecucion
    case Suma(e1, e2) =>
      val f1 = Future { derivar(e1, a) }
      val f2 = Future { derivar(e2, a) }
      val resultado = for { // Se espera a que ambos resultados esten listos para concatenarlos
        r1 <- f1
        r2 <- f2
      } yield Suma(r1, r2) // Se aplica la regla de la derivada de una suma
      Await.result(resultado, Duration.Inf)

    case Prod(e1, e2) =>
      val f1 = Future { derivar(e1, a) }
      val f2 = Future { derivar(e2, a) }
      val resultado = for {
        r1 <- f1
        r2 <- f2
      } yield Suma(Prod(r1, e2), Prod(e1, r2)) // Se aplica la regla de la derivada de un producto
      Await.result(resultado, Duration.Inf)

    case Resta(e1, e2) =>
      val f1 = Future { derivar(e1, a) }
      val f2 = Future { derivar(e2, a) }
      val resultado = for {
        r1 <- f1
        r2 <- f2
      } yield Resta(r1, r2) // Se aplica la regla de la derivada de una resta
      Await.result(resultado, Duration.Inf)

    case Div(e1, e2) =>
      val f1 = Future { derivar(e1, a) }
      val f2 = Future { derivar(e2, a) }
      val resultado = for {
        r1 <- f1
        r2 <- f2
      } yield Div(Resta(Prod(r1, e2), Prod(e1, r2)), Prod(e2, e2)) // Se aplica la regla de la derivada de una división
      Await.result(resultado, Duration.Inf)

    case Expo(e1, e2) =>
      val f1 = Future { derivar(e1, a) }
      val f2 = Future { derivar(e2, a) }
      val resultado = for {
        r1 <- f1
        r2 <- f2
      } yield Prod(Expo(e1, e2), Suma(Div(Prod(r1, e2), e1), Prod(derivar(e2, a), Logaritmo(e1)))) // Se aplica la regla de la derivada de una exponenciación
      Await.result(resultado, Duration.Inf)

    // Se paraleliza por el posible costo computacional que conlleva derivar la subexpresion
    case Logaritmo(e1) =>
      val f1 = Future { derivar(e1, a) }
      val resultado = for {
        r1 <- f1
      } yield Div(r1, e1)
      Await.result(resultado, Duration.Inf)
  }


  def evaluar(f: Expr, a: Atomo, v: Double): Double = f match {

    // Los primeros no se paralelizan dado que no tienen recursividad
    case Numero(d) => d
    case Atomo(x) if x == a.x => v
    case Atomo(_) => 0.0

    // En los siguientes casos se paraleliza la evaluación de las subexpresiones
    // Se emplea Future para poder ejecutar estas tareas en paralelo, dado que ambos calculos son realizados
    // al mismo tiempo en diferentes hilos de ejecucion
    case Suma(e1, e2) =>
      val f1 = Future { evaluar(e1, a, v) }
      val f2 = Future { evaluar(e2, a, v) }
      val resultado = for { // Se espera a que ambos resultados esten listos para sumarlos
        r1 <- f1
        r2 <- f2
      } yield r1 + r2 // Se suman los resultados
      Await.result(resultado, Duration.Inf)

    case Prod(e1, e2) =>
      val f1 = Future { evaluar(e1, a, v) }
      val f2 = Future { evaluar(e2, a, v) }
      val resultado = for { // Se espera a que ambos resultados esten listos para multiplicarlos
        r1 <- f1
        r2 <- f2
      } yield r1 * r2 // Se multiplican los resultados
      Await.result(resultado, Duration.Inf)

    case Resta(e1, e2) =>
      val f1 = Future { evaluar(e1, a, v) }
      val f2 = Future { evaluar(e2, a, v) }
      val resultado = for { // Se espera a que ambos resultados esten listos para restarlos
        r1 <- f1
        r2 <- f2
      } yield r1 - r2 // Se restan los resultados
      Await.result(resultado, Duration.Inf)

    case Div(e1, e2) =>
      val f1 = Future { evaluar(e1, a, v) }
      val f2 = Future { evaluar(e2, a, v) }
      val resultado = for { // Se espera a que ambos resultados esten listos para dividirlos
        r1 <- f1
        r2 <- f2
      } yield r1 / r2 // Se dividen los resultados
      Await.result(resultado, Duration.Inf)

    case Expo(e1, e2) =>
      val f1 = Future { evaluar(e1, a, v) }
      val f2 = Future { evaluar(e2, a, v) }
      val resultado = for { // Se espera a que ambos resultados esten listos para elevarlos
        r1 <- f1
        r2 <- f2
      } yield math.pow(r1, r2) // Se elevan los resultados
      Await.result(resultado, Duration.Inf)

    // Se paraleliza por el posible costo computacional que conlleva evaluar la subexpresion
    case Logaritmo(e1) =>
      val f1 = Future { evaluar(e1, a, v) }
      val resultado = for { // Se espera a que el resultado este listo para calcular el logaritmo
        r1 <- f1
      } yield math.log(r1) // Se calcula el logaritmo
      Await.result(resultado, Duration.Inf)
  }

  def limpiar(f: Expr): Expr = f match {

    // En los siguientes casos se paraleliza la evaluación de las subexpresiones
    // Se emplea Future para poder ejecutar estas tareas en paralelo, dado que ambos calculos son realizados
    // al mismo tiempo en diferentes hilos de ejecucion

    case Suma(Numero(0), e2) => limpiar(e2)
    case Suma(e1, Numero(0)) => limpiar(e1)
    case Suma(e1, e2) =>
      val f1 = Future { limpiar(e1) } // Se limpia la primera subexpresión
      val f2 = Future { limpiar(e2) } // Se limpia la segunda subexpresión
      val resultado = for { // Se espera a que ambos resultados esten listos para simplificar la operacion
        r1 <- f1
        r2 <- f2
      } yield {
        (r1, r2) match { // Se aplica la regla de la simplificación de la suma
          case (Numero(0), e2) => e2
          case (e1, Numero(0)) => e1
          case _ => Suma(r1, r2)
        }
      }
      Await.result(resultado, Duration.Inf) // Se espera a que el resultado este listo


    case Prod(Numero(1), e2) => limpiar(e2)
    case Prod(e1, Numero(1)) => limpiar(e1)
    case Prod(Numero(0), _) => Numero(0)
    case Prod(_, Numero(0)) => Numero(0)
    case Prod(e1, e2) =>
      val f1 = Future { limpiar(e1) }
      val f2 = Future { limpiar(e2) }
      val resultado = for {
        r1 <- f1
        r2 <- f2
      } yield {
        (r1, r2) match { // Se aplica la regla de la simplificación del producto
          case (Numero(1), e2) => e2
          case (e1, Numero(1)) => e1
          case (Numero(0), _) => Numero(0)
          case (_, Numero(0)) => Numero(0)
          case _ => Prod(r1, r2)
        }
      }
      Await.result(resultado, Duration.Inf)

    case Resta(e1, Numero(0)) => limpiar(e1)
    case Resta(e1, e2) =>
      val f1 = Future { limpiar(e1) }
      val f2 = Future { limpiar(e2) }
      val resultado = for {
        r1 <- f1
        r2 <- f2
      } yield {
        if (r1 == r2) Numero(0) // Se aplica la regla de la simplificación de la resta
        else Resta(r1, r2)
      }
      Await.result(resultado, Duration.Inf)

    case Div(Numero(0), _) => Numero(0)
    case Div(e1, Numero(1)) => limpiar(e1)
    case Div(e1, e2) =>
      val f1 = Future { limpiar(e1) }
      val f2 = Future { limpiar(e2) }
      val resultado = for {
        r1 <- f1
        r2 <- f2
      } yield {
        if (r2 == Numero(1)) r1 // Se aplica la regla de la simplificación de la división
        else Div(r1, r2)
      }
      Await.result(resultado, Duration.Inf)


    case Expo(e1, Numero(1)) => limpiar(e1)
    case Expo(Numero(1), _) => Numero(1)
    case Expo(e1, e2) =>
      val f1 = Future { limpiar(e1) }
      val f2 = Future { limpiar(e2) }
      val resultado = for {
        r1 <- f1
        r2 <- f2
      } yield Expo(r1, r2) // Se aplica la regla de la simplificación de la exponenciación
      Await.result(resultado, Duration.Inf) // Se espera a que el resultado este listo

    case Logaritmo(e1) =>
      val f1 = Future { limpiar(e1) }
      val resultado = for {
        r1 <- f1
      } yield Logaritmo(r1) // Se aplica la regla de la simplificación del logaritmo
      Await.result(resultado, Duration.Inf)

    case _ => f // Para Numero y Atomo, retornamos la expresión tal como está
  }


  def raizNewton(f: Expr, a: Atomo, x0: Double, ba: (Expr, Atomo, Double) => Boolean): Double = {
    @annotation.tailrec
    def iter(x: Double): Double = {
      // Evaluamos la función y su derivada en el punto actual en paralelo
      // Se emplea Future para poder ejecutar estas tareas en paralelo, dado que ambos calculos son realizados
      // al mismo tiempo en diferentes hilos de ejecucion
      val fxFuture = Future { evaluar(f, a, x) }
      val dfxFuture = Future { evaluar(derivar(f, a), a, x) }

      // Obtenemos los resultados de las evaluaciones
      val fx = Await.result(fxFuture, Duration.Inf) // Se espera a que el resultado este listo
      val dfx = Await.result(dfxFuture, Duration.Inf)

      // Si la aproximación es suficientemente buena, retornamos el punto actual
      if (ba(f, a, x)) x
      else {
        // Calculamos el siguiente punto candidato utilizando el método de Newton
        val x1 = x - fx / dfx
        iter(x1) // Repetimos el proceso con el nuevo punto candidato
      }
    }

    iter(x0) // Iniciamos la iteración desde el punto inicial
  }

  def buenaAprox(f: Expr, a: Atomo, d: Double): Boolean = {
    math.abs(evaluar(f, a, d)) < 0.001
  }

}
