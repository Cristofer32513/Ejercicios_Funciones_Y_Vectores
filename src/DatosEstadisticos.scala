import scala.io.StdIn.readLine
import scala.util.Sorting.quickSort

object DatosEstadisticos {

  def LlenarVector(vector:Array[Int]) : Array[Int] = {
    for (i <- vector.indices) {
      println("Ingrese el dato numero " + (i + 1) + "...")
      vector(i) = readLine().toInt
    }
    vector
  }

  def ImprimirVector(vector:Array[Int]) : Unit = {
    for (i <- vector.indices) print(vector(i) + ", ")
  }

  def OrdenarVector(vector:Array[Int]) : Array[Int] = {
    quickSort(vector)
    vector
  }

  def Media(vector:Array[Int]) : Double = {
    var sumatoria = 0
    for (i <- vector.indices) sumatoria += vector(i)
    sumatoria.toDouble / vector.length.toDouble
  }

  def Moda(vector:Array[Int]) : Int = {
    var masAlto = 0
    var contActual = 0
    var contAnterior = 0
    for (i <- vector.indices) {
      contActual = 0
      for (j <- vector.indices) if(vector(j) == vector(i)) contActual += 1
      if (contActual > contAnterior) {
        contAnterior = contActual
        masAlto = vector(i)
      }
    }
    if(contAnterior == 1) {
      print("-------- NO HAY VALORES REPETIDOS EN EL VECTOR --------")
      0
    } else masAlto
  }

  def Mediana(vector:Array[Int]) : Double = {
    val mitad = vector.length / 2
    if(vector.length % 2 == 0) (vector(mitad-1).toDouble + vector(mitad).toDouble) / 2
    else vector(mitad)
  }



  def main(args: Array[String]): Unit = {
    //println("Ingrese la cantidad de datos a almacenar en el vector...")
    //val vectorDatos = new Array[Int](readLine().toInt)
    //LlenarVector(vectorDatos)

    val vectorDatos = Array(10, 8, 9, 7, 4, 6, 3, 5, 1, 2)
    println("\n\n\n===== VECTOR ORIGINAL =====")
    ImprimirVector(vectorDatos)
    println("\n\n\n===== VECTOR ORDENADO =====")
    OrdenarVector(vectorDatos)
    ImprimirVector(vectorDatos)
    println("\n\n\n===== MEDIA DE LOS DATOS =====")
    println(Media(vectorDatos))
    println("\n\n===== MODA DE LOS DATOS =====")
    println(Moda(vectorDatos))
    println("\n\n===== MEDIANA DE LOS DATOS =====")
    println(Mediana(vectorDatos))
    println("\n\n===== DESVIACION RESPECTO A LA MEDIA DE LOS DATOS =====")
    println("\n\n===== DESVIACION MEDIA DE LOS DATOS =====")
    println("\n\n===== VARIANZA DE LOS DATOS =====")
    println("\n\n===== DESVICION ESTANDAR DE LOS DATOS =====")




  }
}