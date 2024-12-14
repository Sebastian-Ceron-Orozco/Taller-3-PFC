package taller

class SubMatriz_Resta {
  type Matriz = Vector[Vector[Int]]

  def subMatriz(m: Matriz, i: Int, j: Int, l: Int): Matriz = {
    require(
      i >= 0 && j >= 0 && l > 0 && i + l <= m.length && j + l <= m.head.length,
      "Los índices o dimensiones están fuera de los límites de la matriz"
    )
    m.slice(i, i + l).map(row => row.slice(j, j + l))
  }

  def restaMatriz(m1: Matriz, m2: Matriz): Matriz = {
    require(m1.length == m2.length && m1.head.length == m2.head.length, "Las matrices deben ser del mismo tamaño")
    m1.zip(m2).map {
      case (row1, row2) => row1.zip(row2).map { case (a, b) => a - b }
    }
  }
}
