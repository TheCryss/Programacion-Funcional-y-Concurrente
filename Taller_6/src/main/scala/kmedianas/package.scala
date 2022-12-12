
import   scala.util.Random
import   scala.annotation.tailrec
import   scala.collection.{ Map ,  Seq ,  mutable }
import   scala.collection.parallel.CollectionConverters._
import   scala.collection.parallel.{ ParMap ,  ParSeq }
import   scala.util.Random
import   scala.math._
package object kmedianas {
  //class Punto ( val x : Double , val y : Double , val z : Double ) {
  class Punto(val x: Double, val y: Double) {
    private def cuadrado(v: Double): Double = v * v

    def distanciaAlCuadrado(that: Punto): Double =
    //    cuadrado ( that.x-x ) + cuadrado ( that.y-y ) + cuadrado (that.z-z)
      cuadrado(that.x - x) + cuadrado(that.y - y)

    private def round(v: Double): Double = (v * 100).toInt / 100

    //override def toString = s"( ${round (x)},${round (y)},${round(z)})"
    override def toString = s"( ${(x)},${(y)})"
  }

  def generarPuntosSeq(k: Int, num: Int): Seq[Punto] = {
    val randx = new Random(1)
    val randy = new Random(3)
    val randz = new Random(5)
    (0 until num)
      .map({ i =>
        val x = ((i + 1) % k) * 1.0 / k + randx.nextDouble() * 0.5
        val y = ((i + 5) % k) * 1.0 / k + randy.nextDouble() * 0.5
        val z = ((i + 7) % k) * 1.0 / k + randz.nextDouble() * 0.5
        new Punto(x, y)
      }).to(mutable.ArrayBuffer)
  }


  def inicializarMedianasPar(k: Int, puntos: ParSeq[Punto]): ParSeq[Punto] = {
    val rand = new Random(7)
    (0 until k).map(_ => puntos(rand.nextInt(puntos.length))).to(mutable.ArrayBuffer).par
  }

  //k clusters ?
  def inicializarMedianasSeq(k: Int, puntos: Seq[Punto]): Seq[Punto] = {
    val rand = new Random(7)
    (0 until k).map(_ => puntos(rand.nextInt(puntos.length))).to(mutable.ArrayBuffer)
  }


  def hallarPuntoMasCercano(p: Punto, medianas: IterableOnce[Punto]): Punto = {
    val it = medianas.iterator
    assert(it.nonEmpty)
    var puntoMasCercano = it.next()
    var minDistancia = p.distanciaAlCuadrado(puntoMasCercano)
    while (it.hasNext) {
      val point = it.next()
      val distancia = p.distanciaAlCuadrado(point)
      if (distancia < minDistancia) {
        minDistancia = distancia
        puntoMasCercano = point
      }
    }
    puntoMasCercano
  }

  def clasificarSeq(puntos: Seq[Punto], medianas: Seq[Punto]): Map[Punto, Seq[Punto]] = {
    puntos.groupBy(x => hallarPuntoMasCercano(x, medianas))
  }

  def clasificarPar(puntos: ParSeq[Punto], medianas: ParSeq[Punto]): ParMap[Punto, ParSeq[Punto]] = {
    puntos.groupBy(x => hallarPuntoMasCercano(x, medianas))
  }

  def calculePromedioSeq(medianaVieja: Punto, puntos: Seq[Punto]): Punto
  = {
    if (puntos.isEmpty) medianaVieja
    else {
      var x = 0.0
      var y = 0.0
      var z = 0.0
      puntos.foreach { p =>
        x += p.x
        y += p.y
        //z += p.z
      }
      //new Punto(x / puntos.length, y / puntos.length, z/puntos.length)
      new Punto(x / puntos.length, y / puntos.length)
    }
  }

  def calculePromedioPar(medianaVieja: Punto, puntos: ParSeq[Punto]): Punto = {
    if (puntos.isEmpty) medianaVieja
    else {
      var x = 0.0
      var y = 0.0
      var z = 0.0
      puntos.seq.foreach { p =>
        x += p.x
        y += p.y
        // z += p.z
      }
      new Punto(x / puntos.length, y / puntos.length /*, z / puntos.length*/)
    }
  }

  def actualizarSeq(clasif: Map[Punto, Seq[Punto]], medianasViejas: Seq[Punto]): Seq[Punto] = {
    medianasViejas.map(mean => calculePromedioSeq(mean, clasif(mean)))
  }

  def actualizarPar(clasif: ParMap[Punto, ParSeq[Punto]], medianasViejas: ParSeq[Punto]): ParSeq[Punto] = {
    medianasViejas.map(mean => calculePromedioPar(mean, clasif(mean)))
  }

  def hayConvergenciaSeq(eta: Double, medianasViejas: Seq[Punto], medianasNuevas: Seq[Punto]): Boolean = {
    (medianasViejas zip medianasNuevas).forall { case (o, n) => o.distanciaAlCuadrado(n) < eta }
  }

  def hayConvergenciaPar(eta: Double, medianasViejas: ParSeq[Punto], medianasNuevas: ParSeq[Punto]): Boolean = {
    (medianasViejas zip medianasNuevas).par.forall { case (o, n) => o.distanciaAlCuadrado(n) < eta }
  }

  @tailrec
  final def kMedianasSeq(puntos: Seq[Punto], medianas: Seq[Punto], eta: Double): Seq[Punto] = {
    val classf = clasificarSeq(puntos, medianas)
    val nMeans = actualizarSeq(classf, medianas)
    if (hayConvergenciaSeq(eta, medianas, nMeans)) nMeans else kMedianasSeq(puntos, nMeans, eta)
  }

  @tailrec
  final def kMedianasPar(puntos: ParSeq[Punto], medianas: ParSeq[Punto], eta: Double): ParSeq[Punto] = {
    val classf = clasificarPar(puntos, medianas)
    val means = medianas
    val nMeans = actualizarPar(classf, means)
    if (hayConvergenciaPar(eta, means, nMeans)) nMeans else kMedianasPar(puntos, nMeans, eta)
  }

}