import scala.math._
import scala.util.Random
import common.*

package object Matrices {
  type Matriz = Vector [ Vector [Int] ]

  def matrizAlAzar(long: Int, vals: Int):Matriz = {
    val v = Vector.fill (long, long) {Random.nextInt(vals)}
    v
  }

  def transpuesta(m: Matriz): Matriz = {
    val l = m.length
      Vector.tabulate (l, l) ((i, j) => m(j)(i))
  }
  def prodEscalar ( v1 : Vector [ Int ] , v2 : Vector [ Int ] ) : Int ={
    ( v1 zip v2 ) .map({ (i,j )=> (i*j) } ).sum
  }

  def multMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val l = m1.length
    val m2trans = transpuesta(m2)
    Vector.tabulate(l, l)((i, j) => prodEscalar(m1(i), m2trans(j)))
  }

  def multMatrizPar(m1: Matriz, m2: Matriz): Matriz = {
    val l = m1.length
    //NUmero de hilos
    val h = if (l>=64) 32 else if(l==2) 2 else l/2
    //paso
    val pas = l/h
    val trans = transpuesta(m2)
      //cantidad de llamados recursivos
      def fCounter():Int={
        l match
        case 2 => 1 //parallel de 2
        case 4 => 1 //paralell de 2
        case 8 => 1 //parallel de 4
        case 16 => 2 //2 parallel de 4
        case 32 => 4 //parallel de 4
        case 64 => 16 //parallel de 4
        case _ => (l / 2) / 4 //2^n/2/4
      }

    def aux(paso:Int,counter:Int):Matriz = {
      //println(s"paso:$paso l:$l pas:$pas hilos:$h")
      if(l<=4){

        val (a,b) =parallel(
          Vector.tabulate(pas, l)((i, j) => prodEscalar(m1(i), trans(j))),
          Vector.tabulate(pas, l)((i, j) => prodEscalar(m1(i+pas), trans(j)))
        )

        a++b
      }else{
        if (counter == 0) Vector()
        else {
          val (a, b, c, d) = parallel(
            Vector.tabulate(pas, l)((i, j) => prodEscalar(m1(i + paso), trans(j))),
            Vector.tabulate(pas, l)((i, j) => prodEscalar(m1(i + paso + pas), trans(j))),
            Vector.tabulate(pas, l)((i, j) => prodEscalar(m1(i + paso + pas + pas), trans(j))),
            Vector.tabulate(pas, l)((i, j) => prodEscalar(m1(i + paso + pas + pas + pas), trans(j))))
          a ++ b ++ c ++ d ++ aux(pas * 4, counter - 1)
        }
      }
    }
    aux(0,fCounter())

  }



}

/*
val (a,b,c,d)= parallel(Vector.tabulate(r, l)((i, j) => prodEscalar(m1(i),
trans(j))),Vector.tabulate(r, l)((i, j) => prodEscalar(m1(i +2), trans(j))),
Vector.tabulate(r, l)((i, j) => prodEscalar(m1(i+4),
trans(j))),Vector.tabulate(r, l)((i, j) => prodEscalar(m1(i+6), trans(j))))
a++b++c++d



    Para 4*4
    val t1 = task(Vector.tabulate(2, l)((i, j) => prodEscalar(m1(i), trans(j))))
    val t2 =task(Vector.tabulate(2, l)((i, j) => prodEscalar(m1(i+2), trans(j))))
*/
/*
val t1 = task(Vector.tabulate(r, l)((i, j) => prodEscalar(m1(i), trans(j)))) //0-1
val t2 = task(Vector.tabulate(r, l)((i, j) => prodEscalar(m1(i +2), trans(j))))//2-3
val t3 = task(Vector.tabulate(r, l)((i, j) => prodEscalar(m1(i+4), trans(j))))//4-5
val t4 = task(Vector.tabulate(r, l)((i, j) => prodEscalar(m1(i+6), trans(j))))//4-5
val newM = t1.join()++t2.join()++t3.join()++t4.join()*/