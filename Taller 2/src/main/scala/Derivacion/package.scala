package object Derivacion {
  def derivada( f : Double => Double ) : Double => Double ={
    (x0:Double)=> (f(x0-2*0.1)-8*f(x0-0.1)+8*f(x0+0.1)-f(x0+2*0.1))/1.2
  }

  def derivadaSuma ( f : Double=>Double , g : Double=>Double ) : Double => Double = {
    (x:Double)=>derivada(f)(x)+derivada(g)(x)
  }
  def derivadaResta ( f : Double=>Double , g : Double=>Double ) : Double => Double = {
    (x:Double)=>derivada(f)(x)-derivada(g)(x)
  }
  def derivadaMult ( f : Double=>Double , g : Double=>Double ) : Double => Double = {
    (x:Double)=>(derivada(f)(x)*g(x))+(f(x)*derivada(g)(x))
  }
  def derivadaDiv ( f : Double=>Double , g : Double=>Double ) : Double => Double = {
    (x:Double)=>((derivada(f)(x)*g(x))-(f(x)*derivada(g)(x)))/(g(x)*g(x))
  }
}