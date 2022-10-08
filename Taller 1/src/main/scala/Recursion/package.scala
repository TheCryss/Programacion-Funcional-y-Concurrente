package object Recursion {
  /*
  Ejercicio 1.1.1
  Maximo comun divisor a partir del teorema fundamental de la aritmetica.
   */
  def mcdTFA(ln: List[Int], lm: List[Int], primos: List[Int]): Int = {
    def cmpEXPO(ln: List[Int], lm: List[Int]): Int = if (ln.head > lm.head) lm.head else ln.head

    def pwrPrime(primes: List[Int], ln: List[Int], lm: List[Int]): Double = scala.math.pow(primes.head, cmpEXPO(ln, lm))

    def pwrs(primes: List[Int], ln: List[Int], lm: List[Int], result: Double): Double = {
      if (primes.isEmpty) result
      else pwrs(primes.tail, ln.tail, lm.tail, result * pwrPrime(primes, ln, lm))
    }

    pwrs(primos, ln, lm, 1).toInt
  }

  /*
    Ejercicio 1.1.2
    Maximo comun divisor a partir algoritmo extendido de Euclides
  */
  def mcdEB(n: Int, m: Int): (Int, Int, Int) = {
    def q(n: Int, m: Int, result: Int): Int = if (n > m) q(n - m, m, result + 1) else return result
    def divs(n: Int, m: Int, r: Int, x: Int, y: Int, fx: Int, fy: Int): (Int, Int, Int) = {
      if (r == 0) (m, fx, fy)
      else {
        divs(m, n - (m * q(n, m, 0)), (n - m) * q(n, m, 0), fx, fy, x - (fx * q(n, m, 0)), y - (fy * q(n, m, 0)))
      }
    }
    divs(n, m, 1, 1, 0, 0, 1)
  }

  /*
      Ejercicio 1.2.1
      Fibonacci recursion de arbol
    */
  def fibonacciA(n:Int):Int ={
    if (n<=1) 1
    else fibonacciA(n-1)+fibonacciA(n-2)
  }
  /*
      Ejercicio 1.2.2
      Fibonacci recurcion iterativa
  */
  def fibonacciI(n:Int):Int ={
    def iterable(counter: Int,result: Int,pred:Int,pos:Int):Int={
      if(counter>=n) result
      else {
        iterable(counter+1,pred+pos,pos,pred+pos)
      }
    }
    iterable(0,1,0,1)
  }

}

