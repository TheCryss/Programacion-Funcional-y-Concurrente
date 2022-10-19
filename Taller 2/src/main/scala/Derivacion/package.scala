package object Derivacion {
  def derivada( f:Double=>Double):Double=>Double ={
    (x: Double) => {
      val h = 0.1
      BigDecimal((f(x - 2 * h) - 8 * f(x - h) + 8 * f(x + h) - f(x + 2 * h))/(12*h)).setScale(4, BigDecimal.RoundingMode.HALF_UP).toDouble
    }

  }

  def derivadaSuma ( f : Double=>Double , g : Double=>Double ) : Double => Double ={
    (x:Double)=>{
      derivada (f)(x)+derivada(g)(x)
    }
}

  def derivadaResta(f : Double=>Double , g : Double=>Double ) : Double => Double ={
    (x:Double)=>{
      derivada (f)(x)-derivada(g)(x)
    }
  }
  def derivadaMult(f : Double=>Double , g : Double=>Double ) : Double => Double = {
    (x: Double) => {
      derivada(f)(x) * g(x) + f(x) * derivada(g)(x)
    }
  }
  def derivadaDiv(f : Double=>Double , g : Double=>Double ): Double => Double ={
    (x:Double)=> {
      (derivada(f)(x) * g(x) - f(x) * derivada(g)(x))/(Math.pow(g(x),2))
    }
  }

}

