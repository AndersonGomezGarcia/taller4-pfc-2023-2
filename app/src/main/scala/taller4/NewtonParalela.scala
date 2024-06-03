package taller4

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration._

class NewtonParalela {

  //Esta función no fue paralelizada. La razón es que simplemente está haciendo un patrón
  //de coincidencia en el tipo de expresión (Expr) y luego construyendo una cadena de texto a
  //partir de ella. No hay cálculos ni operaciones que se puedan dividir y ejecutar en paralelo
  //La función es bastante simple y se ejecuta rápidamente sin necesidad de paralelismo.
  def mostrar (e : Expr ) : String = {
    e match {
      case Numero (d) => d.toString
      case Atomo (x) => x.toString
      case Suma (e1 , e2 ) => "(" + mostrar (e1 ) + " + " + mostrar (e2 ) + ")"
      case Prod (e1 , e2 ) => "(" + mostrar (e1 ) + " * " + mostrar (e2 ) + ")"
      case Resta (e1 , e2 ) => "(" + mostrar (e1 ) + " - " + mostrar (e2 ) + ")"
      case Div (e1 , e2 ) => "(" + mostrar (e1 ) + " / " + mostrar (e2 ) + ")"
      case Expo (e1 , e2 ) => "(" + mostrar (e1 ) + " ^ " + mostrar (e2 ) + ")"
      case Logaritmo (e1 ) => "(lg(" + mostrar (e1 ) + "))"
    }
  }

  // Esta función tampoco fue paralelizada. Aunque realiza cálculos para derivar una
  // expresión, la naturaleza de la derivación es tal que cada paso depende del resultado del paso
  // anterior. Esto significa que no es fácil dividir el cálculo en partes independientes que puedan
  // ejecutarse en paralelo sin coordinación adicional. Por lo tanto, la derivación se realiza de forma secuencial.
  def derivar(f: Expr, a: Atomo): Expr = f match {
    case Numero(_) => Numero(0)
    case Atomo(x) if x == a.x => Numero(1)
    case Atomo(_) => Numero(0)
    case Suma(e1, e2) => Suma(derivar(e1, a), derivar(e2, a))
    case Prod(e1, e2) => Suma(Prod(derivar(e1, a), e2), Prod(e1, derivar(e2, a)))
    case Resta(e1, e2) => Resta(derivar(e1, a), derivar(e2, a))
    case Div(e1, e2) => Div(Resta(Prod(derivar(e1, a), e2), Prod(e1, derivar(e2, a))), Prod(e2, e2))
    case Expo(e1,e2)=> Prod(Expo(e1,e2),Suma(Div(Prod(derivar(e1,a),e2),e1),Prod(derivar(e2,a),Logaritmo(e1))))
    case Logaritmo(e1) => Div(derivar(e1, a), e1)
  }

  // La función limpiar tampoco se paralelizó. Al igual que la función de derivación
  // opera en la estructura de la expresión de manera secuencial y no hay operaciones que se puedan
  // dividir fácilmente en partes independientes que puedan ejecutarse en paralelo.
  def limpiar(f: Expr): Expr = f match {
    case Suma(Numero(0), e2) => limpiar(e2)
    case Suma(e1, Numero(0)) => limpiar(e1)
    case Suma(e1, e2) =>
      val l1 = limpiar(e1)
      val l2 = limpiar(e2)
      (l1, l2) match {
        case (Numero(0), e2) => e2
        case (e1, Numero(0)) => e1
        case _ => Suma(l1, l2)
      }
    case Prod(Numero(1), e2) => limpiar(e2)
    case Prod(e1, Numero(1)) => limpiar(e1)
    case Prod(Numero(0),_) => Numero(0)
    case Prod(_, Numero(0)) => Numero(0)
    case Prod(e1, e2) =>
      val l1 = limpiar(e1)
      val l2 = limpiar(e2)
      (l1, l2) match {
        case (Numero(1), e2) => e2
        case (e1, Numero(1)) => e1
        case (Numero(0),_) => Numero(0)
        case (_, Numero(0)) => Numero(0)
        case _   => Prod(l1, l2)
      }
    case Resta(e1, Numero(0)) => limpiar(e1)
    case Resta(e1, e2) =>
      val l1 = limpiar(e1)
      val l2 = limpiar(e2)
      if (l1 == l2) Numero(0)
      else Resta(l1, l2)
    case Div(Numero(0),_) => Numero(0)
    case Div(e1, Numero(1)) => limpiar(e1)
    case Div(e1, e2) =>
      val l1 = limpiar(e1)
      val l2 = limpiar(e2)
      if (l2 == Numero(1)) l1
      else Div(l1, l2)
    case Expo(e1, Numero(1)) => limpiar(e1)
    case Expo(Numero(1),_) => Numero(1)
    case Expo(e1, e2) => Expo(limpiar(e1), limpiar(e2))
    case Logaritmo(e1) => Logaritmo(limpiar(e1))
    case _ => f // Para Numero y Atomo, retornamos la expresión tal como está
  }

  // Esta función sí fue paralelizada. La razón es que, al evaluar una expresión
  // los cálculos en las ramas izquierda y derecha de la expresión son independientes entre sí.
  // Por lo tanto, es posible calcularlos en paralelo utilizando Future y map.
  def evaluarParalelo(f: Expr, a: Atomo, v: Double): Future[Double] = f match {
    case Numero(d) => Future(d)
    case Atomo(x) if x == a.x => Future(v)
    case Atomo(_) => Future(0.0)
    case Suma(e1, e2) =>
      val eval1 = evaluarParalelo(e1, a, v)
      val eval2 = evaluarParalelo(e2, a, v)
      for {
        result1 <- eval1
        result2 <- eval2
      } yield result1 + result2
    case Prod(e1, e2) =>
      val eval1 = evaluarParalelo(e1, a, v)
      val eval2 = evaluarParalelo(e2, a, v)
      for {
        result1 <- eval1
        result2 <- eval2
      } yield result1 * result2
    case Resta(e1, e2) =>
      val eval1 = evaluarParalelo(e1, a, v)
      val eval2 = evaluarParalelo(e2, a, v)
      for {
        result1 <- eval1
        result2 <- eval2
      } yield result1 - result2
    case Div(e1, e2) =>
      val eval1 = evaluarParalelo(e1, a, v)
      val eval2 = evaluarParalelo(e2, a, v)
      for {
        result1 <- eval1
        result2 <- eval2
      } yield result1 / result2
    case Expo(e1, e2) =>
      val eval1 = evaluarParalelo(e1, a, v)
      val eval2 = evaluarParalelo(e2, a, v)
      for {
        result1 <- eval1
        result2 <- eval2
      } yield math.pow(result1, result2)
    case Logaritmo(e1) =>
      evaluarParalelo(e1, a, v).map(math.log)
  }

  // También fue paralelizada. Aunque el cálculo del método de Newton en sí mismo es secuencial
  // (cada iteración depende del resultado de la iteración anterior)
  // la evaluación de la función y su derivada se paraleliza utilizando evaluarParalelo,
  // lo que permite que los cálculos de la función y su derivada en un punto dado se realicen en paralelo.
  def raizNewtonParalelo(f: Expr, a: Atomo, x0: Double, ba: (Expr, Atomo, Double) => Boolean): Future[Double] = {
    @annotation.tailrec
    def iter(x: Double): Double = {
      // Evaluamos la función y su derivada en el punto actual
      val fxFuture = evaluarParalelo(f, a, x)
      val dfxFuture = evaluarParalelo(derivar(f, a), a, x)

      // Esperamos a que las evaluaciones se completen
      val fx = Await.result(fxFuture, Duration.Inf)
      val dfx = Await.result(dfxFuture, Duration.Inf)

      // Si la aproximación es suficientemente buena, retornamos el punto actual
      if (ba(f, a, x)) x
      else {
        // Calculamos el siguiente punto candidato utilizando el método de Newton
        val x1 = x - fx / dfx
        iter(x1) // Repetimos el proceso con el nuevo punto candidato
      }
    }
    Future(iter(x0)) // Iniciamos la iteración desde el punto inicial
  }

  // Esta función no se paralelizó. La razón es similar a las anteriores funciones que no se paralelizaron:
  // no realiza cálculos que se puedan dividir en partes independientes para ejecutar en paralelo.
  // Simplemente evalúa la función en un punto dado y compara el resultado con un umbral.
  def buenaAprox(f: Expr, a: Atomo, d: Double): Boolean = {
    val resultado = Await.result(evaluarParalelo(f, a, d), Duration.Inf)
    math.abs(resultado) < 0.001
  }


}



