package taller

class SumMatriz {

    type Matriz = Vector[Vector[Int]]

    def sumMatriz(m1: Matriz, m2: Matriz): Matriz = {
        require(
            m1.length == m2.length && m1.forall(_.length == m1.length),
            "Ambas matrices deben ser cuadradas y del mismo tamaño."
        )
        Vector.tabulate(m1.length, m1.length)((i, j) => m1(i)(j) + m2(i)(j))
    }
}
