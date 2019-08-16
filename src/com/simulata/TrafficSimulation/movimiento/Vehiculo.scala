package movimiento

import json._
import cartesiano._
import vias._
import simulacion.Simulacion

abstract case class Vehiculo (placa:String, origen:Interseccion, destino:Interseccion, private var _velocidad:Velocidad) 
extends Movil(origen, _velocidad) with MovimientoUniforme {
 
 private var _punto:Punto = origen
 
 def punto=_punto
 
 private var _distanciaRecorrida:Double=0
  
  def distanciaRecorrida=_distanciaRecorrida
  
  def distanciaRecorrida_=(d:Double): Unit = _distanciaRecorrida = d
 
 //para obtener la ruta
  def n(outer:Interseccion):Simulacion.grafoVia.NodeT=Simulacion.grafoVia get outer
  //ruta es una lista de intersecciones
  var ruta =(n(origen) shortestPathTo n(destino)).get.nodes.toList
  
  def actualizarAngulo(a:Punto,b:Punto,v:Velocidad):Unit={
   //se haya el ángulo entre el vector (b-a) y el vector (1,0) siendo a el punto actual y b al que se quiere ir
    val vector = new Punto(b.x-a.x,b.y-a.y)
    if(vector.y>0){ 
    v.angulo=new Angulo((scala.math.acos(vector.x/Punto.distancia(vector,Punto(0,0))))*180/math.Pi)
    }
    //se le multiplica por -1 en caso de que el vector esté en los cuadrantes 3 o 4
    else{
      v.angulo=new Angulo(-((scala.math.acos(vector.x/Punto.distancia(vector,Punto(0,0))))*180/math.Pi))
    }
 }
 
 ////////////////////////////////////////
 
 def mover(dt:Double):Unit={
    //se verifica que aún no se haya llegado a la via final
    if (ruta.length>1){
      //se verifica que la distancia entre el vehículo y la intersección objetivo actual sea mayor que la distancia que se
      //mueve el vehículo en dt, si es menor se corrige poniendo el vehículo en la posición de la intersección objetivo actual
      //y como se alcanzó se actualiza el ángulo y se remueve esta intersección de la ruta
      if(Punto.distancia(p, ruta(1))<=Punto.distancia(p,movimiento(dt,Velocidad.kphTomps(v)))){
        actualizarAngulo(ruta(0),ruta(1),Velocidad.kphTomps(v))
        p=Punto(ruta(0).x,ruta(0).y)
        ruta=ruta.drop(1)
      }
    }
    //Verifica que aún no haya llegado a la intersección final
    if(ruta.length>0){
      val dp = movimiento(dt,Velocidad.kphTomps(v))
      val nuevox = dp.x+this.p.x
      val nuevoy = dp.y+this.p.y
      p_=(Punto(nuevox,nuevoy))
      distanciaRecorrida_=(distanciaRecorrida+Punto.distancia(dp,Punto(0,0)))
    }
  }
 
 ////////////////////////////////////////
 
 Simulacion.vehiculos :+=this
 
 origen.origenes :+=this
 
 destino.fin:+=this

}

object Vehiculo{
  
  val r = scala.util.Random
  
  val letras = ('A' to 'Z')
  
  val digitos = ('0' to '9')  
  
  def crearVehiculo(vMin:Int, vMax:Int, proporciones:Array[String], intersecciones:Array[Interseccion]):Vehiculo={
    //Para verificar que el origen y el destino no sean iguales
    val a:Int = r.nextInt(intersecciones.length)
    var b:Int = r.nextInt(intersecciones.length)
    while(b == a) b=r.nextInt
    def definirTipo(n:String)= n match{
      //se usa el constructor que no recibe placa, en cada clase estará definido como se crean
      //y se envía una interseccion origen y una destino
      //el angulo de la velocidad es 0 por defecto
      case "carro" => new Carro(Carro.generarPlaca,
            intersecciones(a),
            intersecciones(b),
            new Velocidad(vMin+r.nextInt((vMax-vMin))))
      case "moto" => new Moto(Moto.generarPlaca,
            intersecciones(a),
            intersecciones(b),
            new Velocidad(vMin+r.nextInt((vMax-vMin))))
      case "mototaxi" => new Moto(Moto.generarPlaca,
            intersecciones(a),
            intersecciones(b),
            new Velocidad(vMin+r.nextInt((vMax-vMin))))
      case "camion" => new Moto(Moto.generarPlaca,
            intersecciones(a),
            intersecciones(b),
            new Velocidad(vMin+r.nextInt((vMax-vMin))))
      case "bus" => new Moto(Moto.generarPlaca,
            intersecciones(a),
            intersecciones(b),
            new Velocidad(vMin+r.nextInt((vMax-vMin))))
    }
  //se escoge un indice al azar y dependiendo del tipo de este se crea un vehículo
  definirTipo(proporciones(r.nextInt(1000)))
  }
}
