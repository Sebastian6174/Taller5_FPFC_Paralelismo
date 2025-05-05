import common._
import scala.util.Random
import scala.collection.parallel.immutable.ParVector

package object Matrices {

  type Matriz = Vector[Vector[Int]]
  val random = new Random()

  def matrizAlAzar(long: Int, vals: Int): Matriz = {
    val v = Vector.fill(long, long) {
      random.nextInt(vals)
    }
    v
  }

  def vectorAlAzar(long: Int, vals: Int): Vector[Int] = {
    val v = Vector.fill(long){random.nextInt(vals)}
    v
  }

  def prodPunto(v1: Vector[Int], v2: Vector[Int]): Int = {
    (v1 zip v2).map({case (i,j) => (i*j)}).sum
  }

  def prodPuntoParD(v1: ParVector[Int], v2: ParVector[Int]): Int = {
    (v1 zip v2).map({ case (i, j) => (i * j) }).sum
  }

  def transpuesta(m: Matriz): Matriz = {
    val l = m.length
    Vector.tabulate(l,l)((i,j) => m(j)(i))
  }

  // Ejercicio 1.1.1
  def multMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val l = m1.length
    val Tm2 = transpuesta(m2)
    Vector.tabulate(l,l)((i,j) => prodPunto(m1(i), Tm2(j)))
  }

  // Ejercicio 1.1.2
  def multMatrizPar(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    val m2T = transpuesta(m2)
    val mitad = n / 2

    val (parte1, parte2) = parallel(
      Vector.tabulate(mitad, n)((i, j) => prodPunto(m1(i), m2T(j))),
      Vector.tabulate(n - mitad, n)((i, j) => prodPunto(m1(i + mitad), m2T(j))))

    parte1 ++ parte2
  }

  // Ejercicio 1.2.1
  def subMatriz(m: Matriz, i: Int, j: Int, l: Int): Matriz = {
    Vector.tabulate(l,l) { (row, col) =>
      m(row+i)(col+j)
    }
  }

  // Ejercicio 1.2.2
  def sumMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val l = m1.length
    Vector.tabulate(l,l) { (i,j) =>
      m1(i)(j) + m2(i)(j)
    }
  }

  // Se define esta función auxiliar para combinar submatrices que componen una matriz.
  def combMatrices(C11: Matriz, C12: Matriz, C21: Matriz, C22: Matriz): Matriz = {
    val mitad = C11.length
    val n = mitad * 2
    Vector.tabulate(n, n) { (i, j) =>
      if (i < mitad) {
        if (j < mitad) C11(i)(j)
        else C12(i)(j - mitad)
      } else {
        if (j < mitad) C21(i - mitad)(j)
        else C22(i - mitad)(j - mitad)
      }
    }
  }

  // Ejercicio 1.2.3
  def multMatrizRec(m1: Matriz, m2: Matriz): Matriz = {
    // Se obtiene la dimensión de la matriz m1
    val n = m1.length

    // Caso base: matrices 1x1
    if (n == 1) {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    } else {
      // Se dividen las matrices en submatrices n/2 x n/2
      val mitad = n / 2

      // Extraemos las submatrices de m1
      val m1_11 = subMatriz(m1, 0, 0, mitad)
      val m1_12 = subMatriz(m1, 0, mitad, mitad)
      val m1_21 = subMatriz(m1, mitad, 0, mitad)
      val m1_22 = subMatriz(m1, mitad, mitad, mitad)

      // Extraemos las submatrices de B
      val m2_11 = subMatriz(m2, 0, 0, mitad)
      val m2_12 = subMatriz(m2, 0, mitad, mitad)
      val m2_21 = subMatriz(m2, mitad, 0, mitad)
      val m2_22 = subMatriz(m2, mitad, mitad, mitad)

      // Calculamos las submatrices de C recursivamente
      val c11 = sumMatriz(
        multMatrizRec(m1_11, m2_11),
        multMatrizRec(m1_12, m2_12))

      val c12 = sumMatriz(
        multMatrizRec(m1_11, m2_12),
        multMatrizRec(m1_12, m2_22))

      val c21 = sumMatriz(
        multMatrizRec(m1_21, m2_11),
        multMatrizRec(m1_22, m2_21))

      val c22 = sumMatriz(
        multMatrizRec(m1_21, m2_12),
        multMatrizRec(m1_22, m2_22))

      // Se combinan las submatrices obtenidas recursivamente
      combMatrices(c11, c12, c21, c22)
    }
  }

  // Ejercicio 1.2.4
  def multMatrizRecPar(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length

    if (n == 1) {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    } else {
      val mitad = n / 2

      // Extracción de submatrices
      val (m1_11, m1_12, m1_21, m1_22) = (
        subMatriz(m1, 0, 0, mitad),
        subMatriz(m1, 0, mitad, mitad),
        subMatriz(m1, mitad, 0, mitad),
        subMatriz(m1, mitad, mitad, mitad)
      )

      val (m2_11, m2_12, m2_21, m2_22) = (
        subMatriz(m2, 0, 0, mitad),
        subMatriz(m2, 0, mitad, mitad),
        subMatriz(m2, mitad, 0, mitad),
        subMatriz(m2, mitad, mitad, mitad)
      )

      // Multiplicaciones en paralelo (8 operaciones simultáneas)
      val ((c11_1, c11_2), (c12_1, c12_2), (c21_1, c21_2), (c22_1, c22_2)) =
        parallel(
          parallel(multMatrizRecPar(m1_11, m2_11), multMatrizRecPar(m1_12, m2_21)),
          parallel(multMatrizRecPar(m1_11, m2_12), multMatrizRecPar(m1_12, m2_22)),
          parallel(multMatrizRecPar(m1_21, m2_11), multMatrizRecPar(m1_22, m2_21)),
          parallel(multMatrizRecPar(m1_21, m2_12), multMatrizRecPar(m1_22, m2_22))
        )

      // Sumas en paralelo (4 operaciones simultáneas)
      val (c11, c12, c21, c22) = parallel(
        sumMatriz(c11_1, c11_2),
        sumMatriz(c12_1, c12_2),
        sumMatriz(c21_1, c21_2),
        sumMatriz(c22_1, c22_2)
      )

      combMatrices(c11, c12, c21, c22)
    }
  }

  // Ejercicio 1.3.1
  def restaMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val l = m1.length
    Vector.tabulate(l,l) { (i,j) =>
      m1(i)(j) - m2(i)(j)
    }
  }

  // Ejercicio 1.3.2
  def strassenSecuencial(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    if (n == 1) return Vector(Vector(m1(0)(0) * m2(0)(0)))

    val mitad = n / 2

    // Dividir matrices en submatrices
    val A11 = subMatriz(m1, 0, 0, mitad)
    val A12 = subMatriz(m1, 0, mitad, mitad)
    val A21 = subMatriz(m1, mitad, 0, mitad)
    val A22 = subMatriz(m1, mitad, mitad, mitad)

    val B11 = subMatriz(m2, 0, 0, mitad)
    val B12 = subMatriz(m2, 0, mitad, mitad)
    val B21 = subMatriz(m2, mitad, 0, mitad)
    val B22 = subMatriz(m2, mitad, mitad, mitad)

    // Calcular las 7 multiplicaciones de Strassen
    val M1 = strassenSecuencial(sumMatriz(A11, A22), sumMatriz(B11, B22))
    val M2 = strassenSecuencial(sumMatriz(A21, A22), B11)
    val M3 = strassenSecuencial(A11, restaMatriz(B12, B22))
    val M4 = strassenSecuencial(A22, restaMatriz(B21, B11))
    val M5 = strassenSecuencial(sumMatriz(A11, A12), B22)
    val M6 = strassenSecuencial(restaMatriz(A21, A11), sumMatriz(B11, B12))
    val M7 = strassenSecuencial(restaMatriz(A12, A22), sumMatriz(B21, B22))

    // Calcular las submatrices resultantes
    val C11 = sumMatriz(restaMatriz(sumMatriz(M1, M4), M7), M5)
    val C12 = sumMatriz(M3, M5)
    val C21 = sumMatriz(M2, M4)
    val C22 = sumMatriz(restaMatriz(sumMatriz(M1, M3), M2), M6)

    combMatrices(C11, C12, C21, C22)
  }
  // Ejercicio 1.3.3
  def strassenParalelo(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    if (n == 1) return Vector(Vector(m1(0)(0) * m2(0)(0)))

    val mitad = n / 2

    // Dividir matrices (secuencial)
    val A11 = subMatriz(m1, 0, 0, mitad)
    val A12 = subMatriz(m1, 0, mitad, mitad)
    val A21 = subMatriz(m1, mitad, 0, mitad)
    val A22 = subMatriz(m1, mitad, mitad, mitad)

    val B11 = subMatriz(m2, 0, 0, mitad)
    val B12 = subMatriz(m2, 0, mitad, mitad)
    val B21 = subMatriz(m2, mitad, 0, mitad)
    val B22 = subMatriz(m2, mitad, mitad, mitad)

    // Precalcular sumas/restas en paralelo (2 grupos)
    val (sumA11A22, (sumB11B22, sumA21A22), restaB12B22, (restaB21B11, sumA11A12)) = parallel(
      sumMatriz(A11, A22),
      parallel(
        sumMatriz(B11, B22),
        sumMatriz(A21, A22)
      ),
      restaMatriz(B12, B22),
      parallel(
        restaMatriz(B21, B11),
        sumMatriz(A11, A12)
      )
    )

    // Otras sumas/restas (secuencial o con task)
    val restaA21A11 = restaMatriz(A21, A11)
    val sumB11B12 = sumMatriz(B11, B12)
    val restaA12A22 = restaMatriz(A12, A22)
    val sumB21B22 = sumMatriz(B21, B22)

    // Multiplicaciones en paralelo (usando task)
    val t1 = task { strassenParalelo(sumA11A22, sumB11B22) }
    val t2 = task { strassenParalelo(sumA21A22, B11) }
    val t3 = task { strassenParalelo(A11, restaB12B22) }
    val t4 = task { strassenParalelo(A22, restaB21B11) }
    val t5 = task { strassenParalelo(sumA11A12, B22) }
    val t6 = task { strassenParalelo(restaA21A11, sumB11B12) }
    val M7 = strassenParalelo(restaA12A22, sumB21B22) // Ejecución en hilo actual

    val M1 = t1.join()
    val M2 = t2.join()
    val M3 = t3.join()
    val M4 = t4.join()
    val M5 = t5.join()
    val M6 = t6.join()

    // Calcular submatrices resultantes en paralelo
    val ((c11, c12), (c21, c22)) = parallel(
      parallel(
        sumMatriz(restaMatriz(sumMatriz(M1, M4), M7), M5),
        sumMatriz(M3, M5)
      ),
      parallel(
        sumMatriz(M2, M4),
        sumMatriz(restaMatriz(sumMatriz(M1, M3), M2), M6)
      )
    )
    combMatrices(c11, c12, c21, c22)
  }
}
