import scala.io.StdIn.readLine

object DatosEstadisticos {

  def LlenarVector(vector:Array[Int]) : Array[Int] = {
    for (i <- vector.indices) {
      println("Ingrese el dato numero " + (i + 1) + "...")
      vector(i) = readLine().toInt
    }
    vector
  }

  def ImprimirVector(vector:Array[Int]) : Unit = {
    vector.foreach(println)
  }


  def main(args: Array[String]): Unit = {
    println("Ingrese la cantidad de datos a almacenar en el vector...")
    var vectorDatos = new Array[Int](readLine().toInt)

    vectorDatos = LlenarVector(vectorDatos)
    ImprimirVector(vectorDatos)




  }
}