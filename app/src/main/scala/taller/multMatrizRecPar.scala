package taller {

class MultiMatRecPar {

    type Matriz = Vector[Vector[Int]]

    // Extrae una submatriz
    def subMatriz(m: Matriz, i: Int, j: Int, l: Int): Matriz = {
        m.slice(i, i + l).map(row => row.slice(j, j + l))
    }

    // Suma dos matrices
    def sumMatriz(m1: Matriz, m2: Matriz): Matriz = {
        m1.zip(m2).map { case (fila1, fila2) =>
            fila1.zip(fila2).map { case (elem1, elem2) => elem1 + elem2 }
        }
    }

    // Multiplica matrices de manera recursiva con paralelización
    def multMatrizRecPar(matriz1: Matriz, matriz2: Matriz): Matriz = {
        val n = matriz1.length
        require(matriz1.length == matriz2.length, "Las matrices deben ser cuadradas y del mismo tamaño")

        if (n == 1) {
            // Caso base: matrices 1x1
            Vector(Vector(matriz1(0)(0) * matriz2(0)(0)))
        } else {
            // Dividir las matrices en submatrices
            val medio = n / 2

            val (m1A, m1B, m1C, m1D) = (
                subMatriz(matriz1, 0, 0, medio),
                subMatriz(matriz1, 0, medio, medio),
                subMatriz(matriz1, medio, 0, medio),
                subMatriz(matriz1, medio, medio, medio)
            )

            val (m2A, m2B, m2C, m2D) = (
                subMatriz(matriz2, 0, 0, medio),
                subMatriz(matriz2, 0, medio, medio),
                subMatriz(matriz2, medio, 0, medio),
                subMatriz(matriz2, medio, medio, medio)
            )

            // Calcular las submatrices en paralelo usando `.par`
            val resultados = Vector(
                () => sumMatriz(multMatrizRecPar(m1A, m2A), multMatrizRecPar(m1B, m2C)),
                () => sumMatriz(multMatrizRecPar(m1A, m2B), multMatrizRecPar(m1B, m2D)),
                () => sumMatriz(multMatrizRecPar(m1C, m2A), multMatrizRecPar(m1D, m2C)),
                () => sumMatriz(multMatrizRecPar(m1C, m2B), multMatrizRecPar(m1D, m2D))
            ).par.map(task => task()).seq

            val (c11, c12, c21, c22) = (resultados(0), resultados(1), resultados(2), resultados(3))

            // Combinar las submatrices
            Vector.tabulate(n, n) { (i, j) =>
                if (i < medio && j < medio) c11(i)(j)
                else if (i < medio && j >= medio) c12(i)(j - medio)
                else if (i >= medio && j < medio) c21(i - medio)(j)
                else c22(i - medio)(j - medio)
            }
        }
    }
}
}
