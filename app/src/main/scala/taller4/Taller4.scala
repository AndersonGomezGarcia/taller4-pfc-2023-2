/**
  * Taller 3 - Programación Funcional
  * Autores: <Estudiantes>
  * Profesor: Carlos A Delgado
  */
package taller4

import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer

object Taller4{

  def saludo() = "Taller 4"

  def main(args: Array[String]): Unit = {
    val objnew = new Newton()
    val expr = Suma(Prod(Numero(2), Atomo('x')), Numero(3))
    println(objnew.mostrar(expr))

    val expr1 = Suma(Atomo('x'), Numero(2))
    println(objnew.mostrar(expr1))
  }
 }
