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

  def restaMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val l = m1.length
    Vector.tabulate(l, l)((a, b) => m1(a)(b)-m2(a)(b))
  }


  def multMatrizRec(m1: Matriz, m2: Matriz): Matriz = {
    val l = m1.length
    if (l == 1) {
      Vector.tabulate(l, l)((i, j) => prodEscalar(m1(i), m2(j)))
    } else {
      val mA = (for (i <- 0 to 1; j <- 0 to 1) yield subMatriz(m1, i * l / 2, j * l / 2, l / 2)).toVector
      val mB = (for (i <- 0 to 1; j <- 0 to 1) yield subMatriz(m2, i * l / 2, j * l / 2, l / 2)).toVector
      val C11 = sumMatriz(multMatrizRec(mA(0), mB(0)), multMatrizRec(mA(1), mB(2)))
      val C12 = sumMatriz(multMatrizRec(mA(0), mB(1)), multMatrizRec(mA(1), mB(3)))
      val C21 = sumMatriz(multMatrizRec(mA(2), mB(0)), multMatrizRec(mA(3), mB(2)))
      val C22 = sumMatriz(multMatrizRec(mA(2), mB(1)), multMatrizRec(mA(3), mB(3)))

      val upper = Vector.tabulate(C11.length)(i => C11(i) ++ C12(i))
      val downer = Vector.tabulate(C21.length)(i => C21(i) ++ C22(i))
      val nMatriz = (upper++downer)
      return nMatriz
    }
  }
  def multMatrizRecPar(m1: Matriz, m2: Matriz): Matriz = {
    val l = m1.length
    if (l == 1) {
      Vector.tabulate(l, l)((i, j) => prodEscalar(m1(i), m2(j)))
    } else {
      val mA = (for (i <- 0 to 1; j <- 0 to 1) yield subMatriz(m1, i * l / 2, j * l / 2, l / 2)).toVector
      val mB = (for (i <- 0 to 1; j <- 0 to 1) yield subMatriz(m2, i * l / 2, j * l / 2, l / 2)).toVector
      val (a,b,c,d) = parallel(
        sumMatriz(multMatrizRec(mA(0), mB(0)), multMatrizRec(mA(1), mB(2))),
        sumMatriz(multMatrizRec(mA(0), mB(1)), multMatrizRec(mA(1), mB(3))),
        sumMatriz(multMatrizRec(mA(2), mB(0)), multMatrizRec(mA(3), mB(2))),
        sumMatriz(multMatrizRec(mA(2), mB(1)), multMatrizRec(mA(3), mB(3)))
      )
      val upper = Vector.tabulate(a.length)(i => a(i) ++ b(i))
      val downer = Vector.tabulate(c.length)(i => c(i) ++ d(i))
      val nMatriz = (upper ++ downer)
      return nMatriz
    }
  }

  def multStrassen(m1: Matriz, m2: Matriz): Matriz = {
    val l = m1.length
    if(l==1){
      Vector.tabulate(l, l)((i, j) => prodEscalar(m1(i), m2(j)))
    }else{
      val A11 = subMatriz(m1, 0, 0, l / 2)
      val A12 = subMatriz(m1, 0, l / 2, l / 2)
      val A21 = subMatriz(m1, l / 2, 0, l / 2)
      val A22 = subMatriz(m1, l / 2, l / 2, l / 2)

      val B11 = subMatriz(m2, 0, 0, l / 2)
      val B12 = subMatriz(m2, 0, l / 2, l / 2)
      val B21 = subMatriz(m2, l / 2, 0, l / 2)
      val B22 = subMatriz(m2, l / 2, l / 2, l / 2)

      val s1 = restaMatriz(B12,B22)
      val s2 = sumMatriz(A11,A12)
      val s3 = sumMatriz(A21,A22)
      val s4 = restaMatriz(B21,B11)
      val s5 = sumMatriz(A11,A22)
      val s6 = sumMatriz(B11,B22)
      val s7 = restaMatriz(A12,A22)
      val s8 = sumMatriz(B21,B22)
      val s9 = restaMatriz(A11,A21)
      val s10 = sumMatriz(B11,B12)

      val p1= multStrassen(A11,s1)
      val p2= multStrassen(s2,B22)
      val p3= multStrassen(s3,B11)
      val p4= multStrassen(A22,s4)
      val p5= multStrassen(s5,s6)
      val p6= multStrassen(s7,s8)
      val p7= multStrassen(s9,s10)

      val C11 = sumMatriz(sumMatriz(p5, p4), restaMatriz(p6, p2))
      val C12 = sumMatriz(p1, p2)
      val C21 = sumMatriz(p3, p4)
      val C22 = restaMatriz(sumMatriz(p5, p1),(sumMatriz(p3, p7)))

      val upper = Vector.tabulate(C11.length)(i => C11(i) ++ C12(i))
      val downer = Vector.tabulate(C21.length)(i => C21(i) ++ C22(i))
      val nMatriz = (upper ++ downer)
      return nMatriz
    }
  }

  def multStrassenPar(m1: Matriz, m2: Matriz): Matriz = {
    val l = m1.length
    if (l == 1) {
      Vector.tabulate(l, l)((i, j) => prodEscalar(m1(i), m2(j)))
    } else {
      val A11 = subMatriz(m1, 0, 0, l / 2)
      val A12 = subMatriz(m1, 0, l / 2, l / 2)
      val A21 = subMatriz(m1, l / 2, 0, l / 2)
      val A22 = subMatriz(m1, l / 2, l / 2, l / 2)

      val B11 = subMatriz(m2, 0, 0, l / 2)
      val B12 = subMatriz(m2, 0, l / 2, l / 2)
      val B21 = subMatriz(m2, l / 2, 0, l / 2)
      val B22 = subMatriz(m2, l / 2, l / 2, l / 2)

      val s1 = restaMatriz(B12, B22)
      val s2 = sumMatriz(A11, A12)
      val s3 = sumMatriz(A21, A22)
      val s4 = restaMatriz(B21, B11)
      val s5 = sumMatriz(A11, A22)
      val s6 = sumMatriz(B11, B22)
      val s7 = restaMatriz(A12, A22)
      val s8 = sumMatriz(B21, B22)
      val s9 = restaMatriz(A11, A21)
      val s10 = sumMatriz(B11, B12)

      val (p1,p2,p3,p4) = parallel(
        multStrassen(A11, s1),multStrassen(s2, B22),
        multStrassen(s3, B11),multStrassen(A22, s4)
      )

      val (p5, p6) = parallel(
        multStrassen(s5, s6),
        multStrassen(s7, s8)
      )

      val p7= task(multStrassen(s9, s10))

      val C11 = sumMatriz(sumMatriz(p5, p4), restaMatriz(p6, p2))
      val C12 = sumMatriz(p1, p2)
      val C21 = sumMatriz(p3, p4)
      val C22 = restaMatriz(sumMatriz(p5, p1), (sumMatriz(p3, p7.join())))

      val upper = Vector.tabulate(C11.length)(i => C11(i) ++ C12(i))
      val downer = Vector.tabulate(C21.length)(i => C21(i) ++ C22(i))
      val nMatriz = (upper ++ downer)
      return nMatriz
    }
  }



}
