package object Recursion {
  //Calcular el minimo comun divisor mediante el TFA
  def mcdTFA(ln: List[Int], lm: List[Int], primos: List[Int]): Int = {
    def cmpEXPO(ln: List[Int],lm: List[Int]):Int = if (ln.head>lm.head) lm.head else ln.head
    def pwrPrime(primes: List[Int], ln: List[Int], lm: List[Int]): Double = scala.math.pow(primes.head,cmpEXPO(ln, lm))
    def pwrs(primes: List[Int], ln: List[Int], lm: List[Int], result: Double):Double= {
      if (primes.isEmpty) result
      else pwrs(primes.tail,ln.tail,lm.tail,result*pwrPrime(primes, ln, lm))
    }
    pwrs(primos, ln, lm,1).toInt
  }
  //Calcular el minimo comun divisor y coneficientes de berzuit mediante el algoritmo de Euclides
  def mcdEB(n: Int, m: Int): Int = {
    def q(n: Int, m: Int, result: Int): Int = if (n > m) q(n - m, m, result + 1) else return result
    def divs(n: Int, m: Int, r: Int): Int = {
      if (r == 0) m
      else
        divs(m, n - (m* q(n, m, 0)), (n-m)*q(n, m, 0))
    }

    divs(n, m, 1)
  }
 // def div(n:Int,m:Int,q:Int):Int= if (n>m) div(n-m,m, q+1) else return q
}

