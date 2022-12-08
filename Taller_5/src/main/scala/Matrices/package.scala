import common._
import org.apache.commons.math3.linear.MatrixUtils

import scala.util.Random
package object Matrices {
  val random = new Random()
  type Matriz = Vector[Vector[Int]]

  def matrizAlAzar(long: Int, vals: Int): Matriz = {
    //Crea una matriz de enteros cuadrada de long x long ,
    // con valores aleatorios entre 0 y vals
    val v = Vector.fill(long, long) {
      random.nextInt(vals)
    }
    v
  }

  def transpuesta(m: Matriz): Matriz = {
    val l = m.length
    Vector.tabulate(l,l)((i,j) => m(j)(i))
  }

  def prodPunto(v1: Vector[Int], v2: Vector[Int]): Int = {
    (v1 zip v2).map({ case (i, j) => (i * j) }).sum
  }

  // Ejercicio 1 .1 .1
  def multMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val l =m1.length
    val nuevaM = for (i <- m1; j <- transpuesta(m2)) yield prodPunto(i, j)
    Vector.tabulate(l,l)((i,j) => nuevaM(j+(i*l)))
  }
  // Ejercicio 1.2.2
  def sumMatriz(m1: Matriz, m2: Matriz): Matriz = {
    // recibe m1 y m2 matrices cuadradas del mismo tamanio,potencia de 2
    // y devuelve la suma de las 2 matrices
    val l=m1.length
    Vector.tabulate(l,l)((i,j)=>m1(i)(j)+m2(i)(j))
  }
  // Ejercicio 1.3.1
  def restaMatriz(m1: Matriz, m2: Matriz): Matriz = {
    // recibe m1 y m2 matrices cuadradas del mismo tamanio,potencia de 2
    // y devuelve la resta de las 2 matrices
    val l = m1.length
    Vector.tabulate(l, l)((i, j) => m1(i)(j) - m2(i)(j))
  }
  // Ejercicio 1 .2.1
  def subMatriz(m: Matriz, i: Int, j: Int, l: Int): Matriz = {
    // Dada m, matriz cuadrada de NxN , 1<=i , j<=N, i+n<=N, j+n<=N,
    // devuelve la submatriz de nxn correspondiente a m[ i . . i +(n−1) , j . . j +(n−1)]
    Vector.tabulate(l,l)((a,b) => m(i+a)(j+b))
  }
  // Ejercicio 1.2.3
  def multMatrizRec(m1: Matriz, m2: Matriz): Matriz = {
    // recibe m1 y m2 matrices cuadradas del mismo tamanio,potencia de 2
    // y devuelve la multiplicacion de las 2 matrices
    val n=m1.length
    if(n==1) Vector.tabulate(n,n)((i,j)=>m1(0)(0)*m2(0)(0)) else{
      val mAa = subMatriz(m1, 0, 0, m1.length / 2)
      val mAb = subMatriz(m1, 0, m1.length / 2, m1.length / 2)
      val mAc = subMatriz(m1, m1.length / 2, 0, m1.length / 2)
      val mAd = subMatriz(m1, m1.length / 2, m1.length / 2, m1.length / 2)

      val mBa = subMatriz(m2, 0, 0, m2.length / 2)
      val mBb = subMatriz(m2, 0, m2.length / 2, m2.length / 2)
      val mBc = subMatriz(m2, m2.length / 2, 0, m2.length / 2)
      val mBd = subMatriz(m2, m2.length / 2, m2.length / 2, m2.length / 2)

      val C1 =sumMatriz(multMatrizRec(mAa, mBa), multMatrizRec(mAb, mBc))
      val C2 = sumMatriz(multMatrizRec(mAa, mBb), multMatrizRec(mAb, mBd))
      val C3 = sumMatriz(multMatrizRec(mAc, mBa), multMatrizRec(mAd, mBc))
      val C4 = sumMatriz(multMatrizRec(mAc, mBb), multMatrizRec(mAd, mBd))

      Vector.tabulate(n,n)((i, j)=> if(i>=m1.length/2 && j>=m1.length/2) C4(i-m1.length/2)(j-m1.length/2)
      else if(i>=m1.length/2) C3(i-m1.length/2)(j)
      else if (j>=m1.length/2) C2(i)(j-m1.length/2)
      else C1(i)(j))
    }
  }
  // Ejercicio 1.3.2
  def multStrassen(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    if(n==1) Vector.tabulate(n,n)((i,j)=>m1(0)(0)*m2(0)(0)) else{
      //Sub matrices de A
      val mAa = subMatriz(m1, 0, 0, m1.length / 2)
      val mAb = subMatriz(m1, 0, m1.length / 2, m1.length / 2)
      val mAc = subMatriz(m1, m1.length / 2, 0, m1.length / 2)
      val mAd = subMatriz(m1, m1.length / 2, m1.length / 2, m1.length / 2)
      //Sub matrices de B
      val mBa = subMatriz(m2, 0, 0, m2.length / 2)
      val mBb = subMatriz(m2, 0, m2.length / 2, m2.length / 2)
      val mBc = subMatriz(m2, m2.length / 2, 0, m2.length / 2)
      val mBd = subMatriz(m2, m2.length / 2, m2.length / 2, m2.length / 2)
      //multiplicacione de matrices
      val p1 = multStrassen(mAa, restaMatriz(mBb, mBd))
      val p2 = multStrassen(sumMatriz(mAa, mAb), mBd)
      val p3 = multStrassen(sumMatriz(mAc, mAd), mBa)
      val p4 = multStrassen(mAd, restaMatriz(mBc, mBa))
      val p5 = multStrassen(sumMatriz(mAa, mAd), sumMatriz(mBa, mBd))
      val p6 = multStrassen(restaMatriz(mAb, mAd), sumMatriz(mBc, mBd))
      val p7 = multStrassen(restaMatriz(mAa, mAc), sumMatriz(mBa, mBb))
      //Calculo de las sumas de matrices
      val C1 = sumMatriz(restaMatriz(sumMatriz(p5, p4), p2), p6)
      val C2 = sumMatriz(p1, p2)
      val C3 = sumMatriz(p3, p4)
      val C4 = restaMatriz(restaMatriz(sumMatriz(p1, p5), p3), p7)
      Vector.tabulate(n, n)((i, j) => if (i >= m1.length / 2 && j >= m1.length / 2) C4(i - m1.length / 2)(j - m1.length / 2)
      else if (i >= m1.length / 2) C3(i - m1.length / 2)(j)
      else if (j >= m1.length / 2) C2(i)(j - m1.length / 2)
      else C1(i)(j))
    }
  }
  // Ejercicio 1.1.2
  def multMatrizPar(m1: Matriz, m2: Matriz): Matriz = {
    val l = m1.length
    val mTranspues=transpuesta(m2)
    val nuevaM = for (i <- m1; j <- mTranspues) yield prodPunto(i, j)
    parallel(mTranspues,nuevaM)
    Vector.tabulate(l, l)((i, j) => nuevaM(j + (i * l)))
  }
  // Ejercicio 1.2.4
  def multMatrizRecPar(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    if (n == 1) Vector.tabulate(n, n)((i, j) => m1(0)(0) * m2(0)(0)) else {
      val mAa = subMatriz(m1, 0, 0, m1.length / 2)
      val mAb = subMatriz(m1, 0, m1.length / 2, m1.length / 2)
      val mAc = subMatriz(m1, m1.length / 2, 0, m1.length / 2)
      val mAd = subMatriz(m1, m1.length / 2, m1.length / 2, m1.length / 2)
      val mBa = subMatriz(m2, 0, 0, m2.length / 2)
      val mBb = subMatriz(m2, 0, m2.length / 2, m2.length / 2)
      val mBc = subMatriz(m2, m2.length / 2, 0, m2.length / 2)
      val mBd = subMatriz(m2, m2.length / 2, m2.length / 2, m2.length / 2)

      val t1 = task(sumMatriz(multMatrizRec(mAa, mBa), multMatrizRec(mAb, mBc)))
      val t2 = task(sumMatriz(multMatrizRec(mAa, mBb), multMatrizRec(mAb, mBd)))
      val t3 = task(sumMatriz(multMatrizRec(mAc, mBa), multMatrizRec(mAd, mBc)))
      val t4 = task(sumMatriz(multMatrizRec(mAc, mBb), multMatrizRec(mAd, mBd)))

      val C1 = t1.join()
      val C2 = t2.join()
      val C3 = t3.join()
      val C4 = t4.join()
      Vector.tabulate(n, n)((i, j) => if (i >= m1.length / 2 && j >= m1.length / 2) C4(i - m1.length / 2)(j - m1.length / 2)
      else if (i >= m1.length / 2) C3(i - m1.length / 2)(j)
      else if (j >= m1.length / 2) C2(i)(j - m1.length / 2)
      else C1(i)(j))
    }
  }
  // Ejercicio 1.3.3
  def multStrassenPar(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    if (n == 1) Vector.tabulate(n, n)((i, j) => m1(0)(0) * m2(0)(0)) else {
      //Sub matrices de A
      val mAa = subMatriz(m1, 0, 0, m1.length / 2)
      val mAb = subMatriz(m1, 0, m1.length / 2, m1.length / 2)
      val mAc = subMatriz(m1, m1.length / 2, 0, m1.length / 2)
      val mAd = subMatriz(m1, m1.length / 2, m1.length / 2, m1.length / 2)
      //Sub matrices de B
      val mBa = subMatriz(m2, 0, 0, m2.length / 2)
      val mBb = subMatriz(m2, 0, m2.length / 2, m2.length / 2)
      val mBc = subMatriz(m2, m2.length / 2, 0, m2.length / 2)
      val mBd = subMatriz(m2, m2.length / 2, m2.length / 2, m2.length / 2)
      //multiplicacione de matrices
      val t1 = task(multStrassen(mAa, restaMatriz(mBb, mBd)))
      val t2 = task(multStrassen(sumMatriz(mAa, mAb), mBd))
      val t3 = task(multStrassen(sumMatriz(mAc, mAd), mBa))
      val t4 = task(multStrassen(mAd, restaMatriz(mBc, mBa)))
      val t5 = task(multStrassen(sumMatriz(mAa, mAd), sumMatriz(mBa, mBd)))
      val t6 = task(multStrassen(restaMatriz(mAb, mAd), sumMatriz(mBc, mBd)))
      val t7 = task(multStrassen(restaMatriz(mAa, mAc), sumMatriz(mBa, mBb)))
      val p1=t1.join()
      val p2=t2.join()
      val p3=t3.join()
      val p4=t4.join()
      val p5=t5.join()
      val p6=t6.join()
      val p7=t7.join()
      //Calculo de las sumas de matrices
      val T1 = task(sumMatriz(restaMatriz(sumMatriz(p5, p4), p2), p6))
      val T2 = task(sumMatriz(p1, p2))
      val T3 = task(sumMatriz(p3, p4))
      val T4 = task(restaMatriz(restaMatriz(sumMatriz(p1, p5), p3), p7))
      val C1=T1.join()
      val C2=T2.join()
      val C3=T3.join()
      val C4=T4.join()
      Vector.tabulate(n, n)((i, j) => if (i >= m1.length / 2 && j >= m1.length / 2) C4(i - m1.length / 2)(j - m1.length / 2)
      else if (i >= m1.length / 2) C3(i - m1.length / 2)(j)
      else if (j >= m1.length / 2) C2(i)(j - m1.length / 2)
      else C1(i)(j))
    }
  }


}