package cartesiano

trait Recta {
  type T <: Punto
  
  var origen: T = _
  
  var fin: T = _
  
  var angulo:Angulo
  
  def calcularAngulo:Unit={
    //calcula el angulo entre el vector fin-origen y el vector (1,0)
    val vector = new Punto(fin.x-origen.x,fin.y-origen.y)
    if(vector.y>0){ 
     angulo=new Angulo((scala.math.acos(vector.x/Punto.distancia(vector,Punto(0,0))))*180/math.Pi)
    }
    //se le multiplica por -1 en caso de que el vector esté en los cuadrantes 3 o 4
    else{
      angulo=new Angulo(-((scala.math.acos(vector.x/Punto.distancia(vector,Punto(0,0))))*180/math.Pi))
    }
  }
}