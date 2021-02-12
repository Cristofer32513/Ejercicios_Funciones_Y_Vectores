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
    for (i <- vector.indices) {
      print(vector(i) + ", ")
    }
  }

  def OrdenarVector(vector:Array[Int]) : Array[Int] = {
    quickSort(vector)
    vector
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
    println("\n\n\n")




  }
}