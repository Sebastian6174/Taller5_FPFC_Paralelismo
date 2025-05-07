import Matrices._
import scala.collection.parallel.immutable.ParVector
import common._
import Benchmark._

// Configuración de pruebas
val maxExponentMatrices = 9  // Máxima potencia de 2 para matrices
val maxExponentVectores = 6   // Máxima potencia de 10 para vectores
val numRepeticiones = 5       // Número de repeticiones de cada prueba

// Lista de comparaciones para matrices
val comparacionesMatrices = List(
  ("Estándar vs Paralelo",
    (m1: Matriz, m2: Matriz) => multMatriz(m1, m2),
    (m1: Matriz, m2: Matriz) => multMatrizPar(m1, m2)),
  ("Recursivo vs RecursivoPar",
    (m1: Matriz, m2: Matriz) => multMatrizRec(m1, m2),
    (m1: Matriz, m2: Matriz) => multMatrizRecPar(m1, m2)),
  ("Strassen vs StrassenPar",
    (m1: Matriz, m2: Matriz) => multStrassen(m1, m2),
    (m1: Matriz, m2: Matriz) => multStrassenPar(m1, m2))
)

// Función para ejecutar una prueba completa de matrices
def ejecutarPruebaMatrices() = {
  println("===== PRUEBAS DE MULTIPLICACIÓN DE MATRICES =====")
  val resultadosMatrices = for {
    exponente <- 1 to maxExponentMatrices
    dim = math.pow(2, exponente).toInt
    m1 = matrizAlAzar(dim, 2)
    m2 = matrizAlAzar(dim, 2)
  } yield {
    println(s"\nProbando matrices ${dim}x${dim}...")
    val comparaciones = comparacionesMatrices.map { case (nombre, a1, a2) =>
      val (t1, t2, speedup) = compararAlgoritmos(a1, a2)(m1, m2)
      (nombre, t1, t2, speedup)
    }
    (dim, comparaciones)
  }

  // Imprimir resultados
  println("\n===== RESULTADOS MATRICES =====")
  resultadosMatrices.foreach { case (dim, comps) =>
    println(s"\nDimensión ${dim}x${dim}:")
    comps.foreach { case (nombre, t1, t2, speedup) =>
      println(f"$nombre%25s: Secuencial ${t1}%8.4f ms | Paralelo ${t2}%8.4f ms | Speedup ${speedup}%4.2fx")
    }
  }

  resultadosMatrices
}

// Función para ejecutar una prueba completa de producto punto
def ejecutarPruebaPunto() = {
  println("\n===== PRUEBAS DE PRODUCTO PUNTO =====")
  val resultadosPunto = for {
    exponente <- 1 to maxExponentVectores
    dim = math.pow(10, exponente).toInt
    v1 = vectorAlAzar(dim, 2)
    v2 = vectorAlAzar(dim, 2)
  } yield {
    println(s"\nProbando vectores de tamaño $dim...")
    val (t1, t2, speedup) = compararProdPunto(dim)
    (dim, t1, t2, speedup)
  }

  // Imprimir resultados
  println("\n===== RESULTADOS PRODUCTO PUNTO =====")
  resultadosPunto.foreach { case (dim, t1, t2, speedup) =>
    println(f"Tamaño $dim%8d: Secuencial ${t1}%8.4f ms | Paralelo ${t2}%8.4f ms | Speedup ${speedup}%4.2fx")
  }

  resultadosPunto
}

// Ejecutar las repeticiones
println("Iniciando pruebas con " + numRepeticiones + " repeticiones para cada algoritmo\n")

// Repeticiones para matrices
println("EJECUTANDO PRUEBAS DE MATRICES")
for (i <- 1 to numRepeticiones) {
  println(s"\n=== REPETICIÓN $i DE $numRepeticiones PARA MATRICES ===")
  val resultados = ejecutarPruebaMatrices()
}

// Repeticiones para producto punto
println("\nEJECUTANDO PRUEBAS DE PRODUCTO PUNTO")
for (i <- 1 to numRepeticiones) {
  println(s"\n=== REPETICIÓN $i DE $numRepeticiones PARA PRODUCTO PUNTO ===")
  val resultados = ejecutarPruebaPunto()
}

println("\nTodas las pruebas han finalizado.")