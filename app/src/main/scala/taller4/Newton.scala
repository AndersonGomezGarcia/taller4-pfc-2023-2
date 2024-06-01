package taller4
trait Expr
case class Numero ( d : Double ) extends Expr
case class Atomo ( x : Char ) extends Expr
case class Suma( e1 : Expr , e2 : Expr ) extends Expr
case class Prod ( e1 : Expr , e2 : Expr ) extends Expr
case class Resta ( e1 : Expr , e2 : Expr ) extends Expr
case class Div ( e1 : Expr , e2 : Expr ) extends Expr
case class Expo ( e1 : Expr , e2 : Expr ) extends Expr
case class Logaritmo ( e1 : Expr ) extends Expr


class Newton {
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

  def evaluar(f: Expr, a: Atomo, v: Double): Double = f match {
    case Numero(d) => d
    case Atomo(x) if x == a.x => v
    case Atomo(_) => 0.0 // Si el átomo no es el átomo de interés, se evalúa como cero.
    case Suma(e1, e2) => evaluar(e1, a, v) + evaluar(e2, a, v)
    case Prod(e1, e2) => evaluar(e1, a, v) * evaluar(e2, a, v)
    case Resta(e1, e2) => evaluar(e1, a, v) - evaluar(e2, a, v)
    case Div(e1, e2) => evaluar(e1, a, v) / evaluar(e2, a, v)
    case Expo(e1, e2) => math.pow(evaluar(e1, a, v), evaluar(e2, a, v))
    case Logaritmo(e1) => math.log(evaluar(e1, a, v))
  }

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

  def raizNewton(f: Expr, a: Atomo, x0: Double, ba: (Expr, Atomo, Double) => Boolean): Double = {
    @annotation.tailrec
    def iter(x: Double): Double = {
      // Evaluamos la función y su derivada en el punto actual
      val fx = evaluar(f, a, x)
      val dfx = evaluar(derivar(f, a), a, x)

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
