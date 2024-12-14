package taller

class MultMatrizRec() {

    type Matriz = Vector[Vector[Int]]

    def subMatriz(matriz: Matriz, filaInicio: Int, colInicio: Int, tamaño: Int): Matriz = {
        matriz.slice(filaInicio, filaInicio + tamaño).map(_.slice(colInicio, colInicio + tamaño))
    }

    def SumMatriz(m1: Matriz, m2: Matriz): Matriz = {
        m1.zip(m2).map { case (fila1, fila2) =>
            fila1.zip(fila2).map { case (elem1, elem2) => elem1 + elem2 }
        }
    }

    def multMatrizRec(m1: Matriz, m2: Matriz): Matriz = {
        val n = m1.length

        if (n == 1) {
            Vector(Vector(m1(0)(0) * m2(0)(0)))
        } else {
            val mitad = n / 2

            val a11 = subMatriz(m1, 0, 0, mitad)
            val a12 = subMatriz(m1, 0, mitad, mitad)
            val a21 = subMatriz(m1, mitad, 0, mitad)
            val a22 = subMatriz(m1, mitad, mitad, mitad)

            val b11 = subMatriz(m2, 0, 0, mitad)
            val b12 = subMatriz(m2, 0, mitad, mitad)
            val b21 = subMatriz(m2, mitad, 0, mitad)
            val b22 = subMatriz(m2, mitad, mitad, mitad)

            val c11 = SumMatriz(multMatrizRec(a11, b11), multMatrizRec(a12, b21))
            val c12 = SumMatriz(multMatrizRec(a11, b12), multMatrizRec(a12, b22))
            val c21 = SumMatriz(multMatrizRec(a21, b11), multMatrizRec(a22, b21))
            val c22 = SumMatriz(multMatrizRec(a21, b12), multMatrizRec(a22, b22))
            
            Vector.tabulate(n, n) { (i, j) =>
                if (i < mitad && j < mitad) c11(i)(j)
                else if (i < mitad && j >= mitad) c12(i)(j - mitad)
                else if (i >= mitad && j < mitad) c21(i - mitad)(j)
                else c22(i - mitad)(j - mitad)
            }
        }
    }
}
