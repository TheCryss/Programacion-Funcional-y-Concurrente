package object Recursion {

    // Ejercicio 1.1.1. Encontrar mcd usando el teorema fundamental del algebra

    def mcdTFA(ln: List[Int], lm: List[Int], primos: List[Int]): Int = {
      def menorExpo(n: List[Int], m: List[Int]): Int = if (n.head < m.head) n.head else m.head

      if ((primos.tail).isEmpty)
        math.pow(primos.head, menorExpo(ln, lm)).toInt
      else
        (math.pow(primos.head, menorExpo(ln, lm)) * mcdTFA(ln.tail, lm.tail, primos.tail)).toInt
    }


    // Ejercicio 1.1.2. Encontrar mcd usando la definicion recursiva de euclides y devolviendo tambien los coeficientes de Bezout

    def mcdEBez(n: Int, m: Int): List[Int] = {
      def qGeneral(n: Int, m: Int, q: Int): Int = if (n < m * q) q - 1 else qGeneral(n, m, q + 1)

      def mcdAux(n: Int, m: Int, q: Int, x_0: Int, y_0: Int, x_1: Int, y_1: Int): List[Int] = {
        if (m == 0)
          List(n, x_1, y_1)
        else
          mcdAux(m, n % m, qGeneral(n, m, 0), x_1, y_1, x_0 - q * x_1, y_0 - q * y_1)
      }

      mcdAux(n, m, 0, 0, 1, 1, 0)
    }
print(mcdEBez(25,5))

    // Ejercicio 1.1.3. devolver fibonacci n usando recursion de arbol

    def fibonacciA(n: Int): Int = {
      if (n <= 1)
        1
      else
        fibonacciA(n - 1) + fibonacciA(n - 2)
    }

    // Ejercicio 1.1.4. devolver fibonacci n iteracion

    def fibonacciI(n: Int): Int = {
      def fibonacciAux(anterior: Int, actual: Int, contador: Int): Int = {
        if (contador == 0)
          actual
        else
          fibonacciAux(actual, anterior + actual, contador - 1)
      }
      fibonacciAux(0, 1, n)
    }


}
