package object Recursion {
  def mcdTFA(ln: List[Int], lm: List[Int], primos: List[Int]): Double = {
    def cmpEXPO(ln: List[Int],lm: List[Int]):Int = if (ln.head>lm.head) lm.head else ln.head
    def pwrPrime(primes: List[Int], ln: List[Int], lm: List[Int]): Double = scala.math.pow(primes.head,cmpEXPO(ln, lm))
    def pwrs(primes: List[Int], ln: List[Int], lm: List[Int], result: Double):Double= {
      if (primes.isEmpty) result
      else pwrs(primes.tail,ln.tail,lm.tail,result*pwrPrime(primes, ln, lm))
    }
    pwrs(primos, ln, lm,1)
  }
}

