import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

package taller {

class multMatrizRecPar{

    type Matriz = Vector[Vector[Int]]

    def subMatriz(m: Matriz, i: Int, j: Int, l: Int): Matriz = {
        m.slice(i, i + l).map(row => row.slice(j, j + l))
    }

    def sumMatriz(m1: Matriz, m2: Matriz): Matriz = {
        val n = m1.length
        require(m1.length == m2.length && m1(0).length == m2(0).length)
        Vector.tabulate(n, n)((i, j) => m1(i)(j) + m2(i)(j))
    }

    def multMatrizRecPar(matriz1: Matriz, matriz2: Matriz): Matriz = {
        val n = matriz1.length
        require(matriz1.length == matriz2.length, "Matrices must be square and of the same size")

        if (n == 1) {
            Vector(Vector(matriz1(0)(0) * matriz2(0)(0)))
        } else {
            val medio = n / 2

            val matriz1A = subMatriz(matriz1, 0, 0, medio)
            val matriz1B = subMatriz(matriz1, 0, medio, medio)
            val matriz1C = subMatriz(matriz1, medio, 0, medio)
            val matriz1D = subMatriz(matriz1, medio, medio, medio)

            val matriz2A = subMatriz(matriz2, 0, 0, medio)
            val matriz2B = subMatriz(matriz2, 0, medio, medio)
            val matriz2C = subMatriz(matriz2, medio, 0, medio)
            val matriz2D = subMatriz(matriz2, medio, medio, medio)

            val futures = Seq(
                Future(multMatrizRecPar(matriz1A, matriz2A)),
                Future(multMatrizRecPar(matriz1B, matriz2C)),
                Future(multMatrizRecPar(matriz1A, matriz2B)),
                Future(multMatrizRecPar(matriz1B, matriz2D)),
                Future(multMatrizRecPar(matriz1C, matriz2A)),
                Future(multMatrizRecPar(matriz1D, matriz2C)),
                Future(multMatrizRecPar(matriz1C, matriz2B)),
                Future(multMatrizRecPar(matriz1D, matriz2D))
            )

            val results = Await.result(Future.sequence(futures), Duration.Inf)

            val top_left = sumMatriz(results(0), results(1))
            val top_right = sumMatriz(results(2), results(3))
            val bottom_left = sumMatriz(results(4), results(5))
            val bottom_right = sumMatriz(results(6), results(7))

            Vector.tabulate(n, n) { (i, j) =>
                if (i < medio && j < medio) top_left(i)(j)
                else if (i < medio && j >= medio) top_right(i)(j - medio)
                else if (i >= medio && j < medio) bottom_left(i - medio)(j)
                else bottom_right(i - medio)(j - medio)
            }
        }
    }
}}
