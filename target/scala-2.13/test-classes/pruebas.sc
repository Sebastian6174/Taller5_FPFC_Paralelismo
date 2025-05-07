import Matrices._
import scala.collection.parallel.immutable.ParVector
import common._
import Benchmark._

// Función para verificar si dos matrices son iguales
def matricesIguales(m1: Matriz, m2: Matriz): Boolean = {
  if (m1.length != m2.length) return false
  for (i <- 0 until m1.length) {
    if (m1(i).length != m2(i).length) return false
    for (j <- 0 until m1(i).length) {
      if (m1(i)(j) != m2(i)(j)) return false
    }
  }
  true
}

println("===== PRUEBAS DE CORRECCIÓN =====")
println("\n1. VERIFICACIÓN CON MATRICES PEQUEÑAS")

// Caso base - matriz 1x1
val m1x1A = Vector(Vector(1))
val m1x1B = Vector(Vector(2))
println(s"Matriz 1x1 A: $m1x1A")
println(s"Matriz 1x1 B: $m1x1B")

val m1x1_est = multMatriz(m1x1A, m1x1B)
val m1x1_estPar = multMatrizPar(m1x1A, m1x1B)
val m1x1_rec = multMatrizRec(m1x1A, m1x1B)
val m1x1_recPar = multMatrizRecPar(m1x1A, m1x1B)
val m1x1_str = multStrassen(m1x1A, m1x1B)
val m1x1_strPar = multStrassenPar(m1x1A, m1x1B)

println(s"Resultados matrices 1x1:")
println(s"  Estándar: $m1x1_est")
println(s"  Estándar Paralelo: $m1x1_estPar")
println(s"  Recursivo: $m1x1_rec")
println(s"  Recursivo Paralelo: $m1x1_recPar")
println(s"  Strassen: $m1x1_str")
println(s"  Strassen Paralelo: $m1x1_strPar")

val correcto1x1 = matricesIguales(m1x1_est, m1x1_estPar) &&
  matricesIguales(m1x1_est, m1x1_rec) &&
  matricesIguales(m1x1_est, m1x1_recPar) &&
  matricesIguales(m1x1_est, m1x1_str) &&
  matricesIguales(m1x1_est, m1x1_strPar)
println(s"Todos los algoritmos dan el mismo resultado para 1x1: $correcto1x1")

// Matriz 2x2
val m2x2A = Vector(Vector(1, 2), Vector(3, 4))
val m2x2B = Vector(Vector(5, 6), Vector(7, 8))
println(s"\nMatriz 2x2 A: $m2x2A")
println(s"Matriz 2x2 B: $m2x2B")

val m2x2_est = multMatriz(m2x2A, m2x2B)
val m2x2_estPar = multMatrizPar(m2x2A, m2x2B)
val m2x2_rec = multMatrizRec(m2x2A, m2x2B)
val m2x2_recPar = multMatrizRecPar(m2x2A, m2x2B)
val m2x2_str = multStrassen(m2x2A, m2x2B)
val m2x2_strPar = multStrassenPar(m2x2A, m2x2B)

println(s"Resultados matrices 2x2:")
println(s"  Estándar: $m2x2_est")
println(s"  Estándar Paralelo: $m2x2_estPar")
println(s"  Recursivo: $m2x2_rec")
println(s"  Recursivo Paralelo: $m2x2_recPar")
println(s"  Strassen: $m2x2_str")
println(s"  Strassen Paralelo: $m2x2_strPar")

val correcto2x2 = matricesIguales(m2x2_est, m2x2_estPar) &&
  matricesIguales(m2x2_est, m2x2_rec) &&
  matricesIguales(m2x2_est, m2x2_recPar) &&
  matricesIguales(m2x2_est, m2x2_str) &&
  matricesIguales(m2x2_est, m2x2_strPar)
println(s"Todos los algoritmos dan el mismo resultado para 2x2: $correcto2x2")

// Matriz 4x4
val m4x4A = matrizAlAzar(4, 2)
val m4x4B = matrizAlAzar(4, 2)
println(s"\nMatriz 4x4 A generada aleatoriamente")
println(s"Matriz 4x4 B generada aleatoriamente")

val m4x4_est = multMatriz(m4x4A, m4x4B)
val m4x4_estPar = multMatrizPar(m4x4A, m4x4B)
val m4x4_rec = multMatrizRec(m4x4A, m4x4B)
val m4x4_recPar = multMatrizRecPar(m4x4A, m4x4B)
val m4x4_str = multStrassen(m4x4A, m4x4B)
val m4x4_strPar = multStrassenPar(m4x4A, m4x4B)

val correcto4x4 = matricesIguales(m4x4_est, m4x4_estPar) &&
  matricesIguales(m4x4_est, m4x4_rec) &&
  matricesIguales(m4x4_est, m4x4_recPar) &&
  matricesIguales(m4x4_est, m4x4_str) &&
  matricesIguales(m4x4_est, m4x4_strPar)
println(s"Todos los algoritmos dan el mismo resultado para 4x4: $correcto4x4")

// Matriz 8x8
val m8x8A = matrizAlAzar(8, 2)
val m8x8B = matrizAlAzar(8, 2)
println(s"\nMatriz 8x8 A generada aleatoriamente")
println(s"Matriz 8x8 B generada aleatoriamente")

val m8x8_est = multMatriz(m8x8A, m8x8B)
val m8x8_estPar = multMatrizPar(m8x8A, m8x8B)
val m8x8_rec = multMatrizRec(m8x8A, m8x8B)
val m8x8_recPar = multMatrizRecPar(m8x8A, m8x8B)
val m8x8_str = multStrassen(m8x8A, m8x8B)
val m8x8_strPar = multStrassenPar(m8x8A, m8x8B)

val correcto8x8 = matricesIguales(m8x8_est, m8x8_estPar) &&
  matricesIguales(m8x8_est, m8x8_rec) &&
  matricesIguales(m8x8_est, m8x8_recPar) &&
  matricesIguales(m8x8_est, m8x8_str) &&
  matricesIguales(m8x8_est, m8x8_strPar)
println(s"Todos los algoritmos dan el mismo resultado para 8x8: $correcto8x8")

// Caso especial - matriz identidad
println("\n2. VERIFICACIÓN CON CASOS ESPECIALES")
val identidad4 = Vector(
  Vector(1, 0, 0, 0),
  Vector(0, 1, 0, 0),
  Vector(0, 0, 1, 0),
  Vector(0, 0, 0, 1)
)
val matrizPrueba4 = matrizAlAzar(4, 2)

println("Prueba con matriz identidad 4x4:")
val identidad4_est = multMatriz(identidad4, matrizPrueba4)
val correctoIdentidad = matricesIguales(identidad4_est, matrizPrueba4)
println(s"La multiplicación de la identidad por una matriz da la misma matriz: $correctoIdentidad")

println("\n===== PRUEBAS DE RENDIMIENTO =====")
println("\n1. COMPARACIÓN ALGORITMO ESTÁNDAR VS PARALELO")

// Función para imprimir resultados de rendimiento formateados
def imprimirResultados(dim: Int, estVsEstPar: (Double, Double, Double),
                       recVsRecPar: (Double, Double, Double),
                       strVsStrPar: (Double, Double, Double)): Unit = {
  println(s"Dimensión $dim x $dim:")
  println(f"  Estándar: ${estVsEstPar._1}%.4f ms, Estándar Paralelo: ${estVsEstPar._2}%.4f ms, Aceleración: ${estVsEstPar._3}%.4f")
  println(f"  Recursivo: ${recVsRecPar._1}%.4f ms, Recursivo Paralelo: ${recVsRecPar._2}%.4f ms, Aceleración: ${recVsRecPar._3}%.4f")
  println(f"  Strassen: ${strVsStrPar._1}%.4f ms, Strassen Paralelo: ${strVsStrPar._2}%.4f ms, Aceleración: ${strVsStrPar._3}%.4f")
}

// Crear una tabla para almacenar resultados (para análisis posterior)
println("Comparando versiones secuenciales vs paralelas:")
var resultadosSeqVsPar = List[(Int, (Double, Double, Double), (Double, Double, Double), (Double, Double, Double))]()

// Comparar los algoritmos para matrices de diferentes tamaños
for (i <- 2 to 8) {
  val dim = math.pow(2, i).toInt
  val m1 = matrizAlAzar(dim, 2)
  val m2 = matrizAlAzar(dim, 2)

  // Comparar estándar vs estándar paralelo
  val estVsEstPar = compararAlgoritmos(multMatriz, multMatrizPar)(m1, m2)

  // Comparar recursivo vs recursivo paralelo
  val recVsRecPar = compararAlgoritmos(multMatrizRec, multMatrizRecPar)(m1, m2)

  // Comparar Strassen vs Strassen paralelo
  val strVsStrPar = compararAlgoritmos(multStrassen, multStrassenPar)(m1, m2)

  imprimirResultados(dim, estVsEstPar, recVsRecPar, strVsStrPar)

  // Almacenar resultados
  resultadosSeqVsPar = resultadosSeqVsPar :+ (dim, estVsEstPar, recVsRecPar, strVsStrPar)
}

println("\n2. COMPARACIÓN ENTRE ALGORITMOS SECUENCIALES")

// Comparar los tres algoritmos secuenciales
println("Comparando algoritmos secuenciales entre sí:")
var resultadosAlgoritmosSeq = List[(Int, (Double, Double, Double), (Double, Double, Double), (Double, Double, Double))]()

for (i <- 4 to 8) {
  val dim = math.pow(2, i).toInt
  val m1 = matrizAlAzar(dim, 2)
  val m2 = matrizAlAzar(dim, 2)

  val estVsRec = compararAlgoritmos(multMatriz, multMatrizRec)(m1, m2)
  val estVsStr = compararAlgoritmos(multMatriz, multStrassen)(m1, m2)
  val recVsStr = compararAlgoritmos(multMatrizRec, multStrassen)(m1, m2)

  println(s"Dimensión $dim x $dim:")
  println(f"  Estándar vs Recursivo: Estándar ${estVsRec._1}%.4f ms, Recursivo ${estVsRec._2}%.4f ms, Ratio: ${estVsRec._3}%.4f")
  println(f"  Estándar vs Strassen: Estándar ${estVsStr._1}%.4f ms, Strassen ${estVsStr._2}%.4f ms, Ratio: ${estVsStr._3}%.4f")
  println(f"  Recursivo vs Strassen: Recursivo ${recVsStr._1}%.4f ms, Strassen ${recVsStr._2}%.4f ms, Ratio: ${recVsStr._3}%.4f")

  // Almacenar resultados
  resultadosAlgoritmosSeq = resultadosAlgoritmosSeq :+ (dim, estVsRec, estVsStr, recVsStr)
}

println("\n3. COMPARACIÓN ENTRE ALGORITMOS PARALELOS")

// Comparar los tres algoritmos paralelos
println("Comparando algoritmos paralelos entre sí:")
var resultadosAlgoritmosPar = List[(Int, (Double, Double, Double), (Double, Double, Double), (Double, Double, Double))]()

for (i <- 4 to 8) {
  val dim = math.pow(2, i).toInt
  val m1 = matrizAlAzar(dim, 2)
  val m2 = matrizAlAzar(dim, 2)

  val estParVsRecPar = compararAlgoritmos(multMatrizPar, multMatrizRecPar)(m1, m2)
  val estParVsStrPar = compararAlgoritmos(multMatrizPar, multStrassenPar)(m1, m2)
  val recParVsStrPar = compararAlgoritmos(multMatrizRecPar, multStrassenPar)(m1, m2)

  println(s"Dimensión $dim x $dim (paralelo):")
  println(f"  Estándar Par vs Recursivo Par: Estándar ${estParVsRecPar._1}%.4f ms, Recursivo ${estParVsRecPar._2}%.4f ms, Ratio: ${estParVsRecPar._3}%.4f")
  println(f"  Estándar Par vs Strassen Par: Estándar ${estParVsStrPar._1}%.4f ms, Strassen ${estParVsStrPar._2}%.4f ms, Ratio: ${estParVsStrPar._3}%.4f")
  println(f"  Recursivo Par vs Strassen Par: Recursivo ${recParVsStrPar._1}%.4f ms, Strassen ${recParVsStrPar._2}%.4f ms, Ratio: ${recParVsStrPar._3}%.4f")

  // Almacenar resultados
  resultadosAlgoritmosPar = resultadosAlgoritmosPar :+ (dim, estParVsRecPar, estParVsStrPar, recParVsStrPar)
}

println("\n===== PRUEBAS DE PRODUCTO PUNTO (PARALELISMO DE DATOS) =====")

// Comparar prodPunto vs prodPuntoParD para diferentes tamaños
println("Comparando producto punto secuencial vs paralelo:")
var resultadosProdPunto = List[(Int, Double, Double, Double)]()

// Probar con potencias de 10 para mejor análisis del umbral
for (exp <- 1 to 7) {
  val size = math.pow(10, exp).toInt
  val result = compararProdPunto(size)
  println(f"Tamaño vector $size: Secuencial ${result._1}%.4f ms, Paralelo ${result._2}%.4f ms, Aceleración: ${result._3}%.4f")

  // Almacenar resultados
  resultadosProdPunto = resultadosProdPunto :+ (size, result._1, result._2, result._3)
}

// También probar con algunos tamaños intermedios para encontrar el punto de inflexión
val tamañosIntermedios = List(50000, 100000, 500000)
for (size <- tamañosIntermedios) {
  val result = compararProdPunto(size)
  println(f"Tamaño vector $size: Secuencial ${result._1}%.4f ms, Paralelo ${result._2}%.4f ms, Aceleración: ${result._3}%.4f")

  // Almacenar resultados
  resultadosProdPunto = resultadosProdPunto :+ (size, result._1, result._2, result._3)
}

println("\n===== PRUEBAS DE FUNCIONES AUXILIARES =====")

// Prueba de subMatriz
val matrizOriginal = Vector(
  Vector(1, 2, 3, 4),
  Vector(5, 6, 7, 8),
  Vector(9, 10, 11, 12),
  Vector(13, 14, 15, 16)
)

println("1. PRUEBA DE SUBMATRIZ")
println("Matriz original:")
matrizOriginal.foreach(fila => println(s"  $fila"))

val subM11 = subMatriz(matrizOriginal, 0, 0, 2)
val subM12 = subMatriz(matrizOriginal, 0, 2, 2)
val subM21 = subMatriz(matrizOriginal, 2, 0, 2)
val subM22 = subMatriz(matrizOriginal, 2, 2, 2)

println("\nSubmatrices:")
println(s"  A11 (esquina superior izquierda): $subM11")
println(s"  A12 (esquina superior derecha): $subM12")
println(s"  A21 (esquina inferior izquierda): $subM21")
println(s"  A22 (esquina inferior derecha): $subM22")

// Verificar que las submatrices son correctas
val subM11Correcto = subM11 == Vector(Vector(1, 2), Vector(5, 6))
val subM12Correcto = subM12 == Vector(Vector(3, 4), Vector(7, 8))
val subM21Correcto = subM21 == Vector(Vector(9, 10), Vector(13, 14))
val subM22Correcto = subM22 == Vector(Vector(11, 12), Vector(15, 16))

println("\nVerificación de submatrices:")
println(s"  A11 correcta: $subM11Correcto")
println(s"  A12 correcta: $subM12Correcto")
println(s"  A21 correcta: $subM21Correcto")
println(s"  A22 correcta: $subM22Correcto")

// Prueba de sumMatriz y restaMatriz
println("\n2. PRUEBA DE SUMA Y RESTA DE MATRICES")
val matrizA = Vector(Vector(1, 2), Vector(3, 4))
val matrizB = Vector(Vector(5, 6), Vector(7, 8))

val suma = sumMatriz(matrizA, matrizB)
val resta = restaMatriz(matrizA, matrizB)

println("Matrices de entrada:")
println(s"  matrizA: $matrizA")
println(s"  matrizB: $matrizB")
println(s"  A + B: $suma")
println(s"  A - B: $resta")

// Verificar que la suma y resta son correctas
val sumaCorrecta = suma == Vector(Vector(6, 8), Vector(10, 12))
val restaCorrecta = resta == Vector(Vector(-4, -4), Vector(-4, -4))

println("\nVerificación de operaciones:")
println(s"  Suma correcta: $sumaCorrecta")
println(s"  Resta correcta: $restaCorrecta")

// Resumen de resultados
println("\n===== RESUMEN DE RESULTADOS =====")
println("\n1. VERIFICACIÓN DE CORRECCIÓN")
println(s"  Todas las matrices 1x1 coinciden: $correcto1x1")
println(s"  Todas las matrices 2x2 coinciden: $correcto2x2")
println(s"  Todas las matrices 4x4 coinciden: $correcto4x4")
println(s"  Todas las matrices 8x8 coinciden: $correcto8x8")
println(s"  Prueba de identidad correcta: $correctoIdentidad")

println("\n2. FUNCIONES AUXILIARES")
println(s"  Todas las submatrices correctas: ${subM11Correcto && subM12Correcto && subM21Correcto && subM22Correcto}")
println(s"  Operaciones de suma y resta correctas: ${sumaCorrecta && restaCorrecta}")

println("\n3. RENDIMIENTO")
println("  Ver tablas arriba para análisis detallado")

// Identificar el tamaño donde el paralelismo comienza a ser beneficioso
val umbralProdPunto = resultadosProdPunto.find(_._3 > 1.0).map(_._1).getOrElse("No encontrado")
println(s"\n4. UMBRALES DE PARALELISMO")
println(s"  Producto punto: beneficioso a partir de tamaño aproximado: $umbralProdPunto")

// Identificar el mejor algoritmo por tamaño
println("\n5. MEJOR ALGORITMO POR TAMAÑO (BASADO EN TIEMPOS)")
resultadosSeqVsPar.foreach { case (dim, est, rec, str) =>
  val tiemposSec = List(("Estándar", est._1), ("Recursivo", rec._1), ("Strassen", str._1))
  val tiemposPar = List(("Estándar Par", est._2), ("Recursivo Par", rec._2), ("Strassen Par", str._2))

  val mejorSec = tiemposSec.minBy(_._2)
  val mejorPar = tiemposPar.minBy(_._2)
  val mejorGeneral = (tiemposSec ++ tiemposPar).minBy(_._2)

  println(s"  Dimensión ${dim}x${dim}:")
  println(f"    Mejor algoritmo secuencial: ${mejorSec._1} (${mejorSec._2}%.4f ms)")
  println(f"    Mejor algoritmo paralelo: ${mejorPar._1} (${mejorPar._2}%.4f ms)")
  println(f"    Mejor algoritmo general: ${mejorGeneral._1} (${mejorGeneral._2}%.4f ms)")
}