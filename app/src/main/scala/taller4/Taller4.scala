/**
  * Taller 3 - Programación Funcional
  * Autores: <Estudiantes>
  * Profesor: Carlos A Delgado
  */
/**
 * Taller 3 - Programación Funcional
 * Autores: <Estudiantes>
 * Profesor: Carlos A Delgado
 */
package taller4

import benchmarking.Benchmarking
import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Taller4{
  def main(args: Array[String]): Unit = {
    val benchmarking = new Benchmarking()

    val objnew = new Newton()
    val expr = Suma(Prod(Numero(2), Atomo('x')), Numero(3))
    benchmarking.NewtonBenchmarking()


  }
}