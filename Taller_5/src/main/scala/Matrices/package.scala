import scala.math._
import scala.util.Random
import common.*

package object Matrices {
  type Matriz = Vector [ Vector [Int] ]
  type VectorMatriz = Vector[Matriz]

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

  def subMatriz (m: Matriz , i : Int , j : Int , l : Int ) : Matriz={

    Vector.tabulate(l,l)((a,b)=>m(i+a)(j+b))
  }




  def sumMatriz(m1 : Matriz , m2 : Matriz ) : Matriz={
    val l = m1.length
    Vector.tabulate(l, l)((a, b) => m1(a)(b)+m2(a)(b))
  }

  def multMatrizRec (m1 : Matriz , m2 : Matriz ) : Matriz= {
    val l = m1.length
    //Extraigo las 4 submatrices de m1
    val mA = (for (i <- 0 to 1; j <- 0 to 1) yield subMatriz(m1, i * l / 2, j * l / 2, l / 2)).toVector
    val mB = (for (i <- 0 to 1; j <- 0 to 1) yield subMatriz(m2, i * l / 2, j * l / 2, l / 2)).toVector
    val trans = for (a <- 0 until mB.length) yield transpuesta(mB(a))
    //cambiar left y right
    def left(): VectorMatriz = {
      (for (i <- 0 to 1; j <- 0 to 1) yield multMatriz(mA(i * 2), mB(j))).toVector
    }
    def right(): VectorMatriz = {
      (for (i <- 1 to 2; j <- 2 to 3) yield multMatriz(mA((i * 2) - 1), mB(j))).toVector
    }
    val nR = right()
    val nL = left()
    def reAgroup(): VectorMatriz = {
      (nR zip nL).map({ (i, j) => (sumMatriz(i, j)) })
    }
    val q = reAgroup()
    def nMatriz(): Matriz = {
      ((Vector((q(0)(0) ++ q(1)(0))):+(q(0)(1) ++ q(1)(1))):+(q(2)(0) ++ q(3)(0))):+(q(2)(1) ++ q(3)(1))
    }
   val f = nMatriz()
    f
  }


}
