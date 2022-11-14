
package object Newton{
trait Expr
case class Numero(d: Double) extends Expr
case class Atomo(x: Char) extends Expr
case class Suma(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr
case class Resta(e1: Expr, e2: Expr) extends Expr
case class Div(e1: Expr, e2: Expr) extends Expr
case class Expo(e1: Expr, e2: Expr) extends Expr
case class Logaritmo(e1: Expr) extends Expr

  def mostrar (e:Expr):String= e match {
    case Numero(d)=> d.toString
    case Atomo(x)=> x.toString
    case Suma(e1,e2)=>"(" + mostrar(e1) + "+" + mostrar(e2) + ")"
    case Prod(e1,e2)=>"(" + mostrar(e1) + "*" + mostrar(e2) + ")"
    case Resta(e1,e2)=>"(" + mostrar(e1) + "-" + mostrar(e2) + ")"
    case Div(e1,e2)=>"(" + mostrar(e1) + "/" + mostrar(e2) + ")"
    case Expo(e1,e2)=>"(" + mostrar(e1) + "^" + mostrar(e2) + ")"
    case Logaritmo(e1)=>"(log(" + mostrar(e1) + "))"
  }

  def derivar(f:Expr, a:Atomo):Expr= f match {
    case Numero(d)=> Numero(0)
    case Atomo(x)=> if (Atomo(x)==a) Numero(1) else Numero(0)
    case Suma(e1,e2) => Suma(derivar(e1,a),derivar(e2,a))
    case Resta(e1,e2)=> Resta(derivar(e1,a),derivar(e2,a))
    case Prod(e1,e2)=> Suma(Prod(derivar(e1,a),e2),Prod(derivar(e2,a),e1))
    case Div(e1,e2)=> Div(Resta(Prod(derivar(e1,a),e2),Prod(derivar(e2,a),e1)), Expo(e2,Numero(2)))
    case Logaritmo(e1)=>Div(derivar(e1,a),e1)
    case Expo(e1,e2)=> Prod(Expo(e1,e2),Suma(Div(Prod(derivar(e1,a),e2),e1),Prod(derivar(e2,a),Logaritmo(e1))))
  }
  def evaluar (f:Expr, a:Atomo, v:Double):Double= f match {
    case Numero(d)=> d
    case Atomo(x)=> v
    case Suma(e1,e2)=> evaluar(e1,a,v) + evaluar(e2,a,v)
    case Resta(e1,e2)=> evaluar(e1,a,v) - evaluar(e2,a,v)
    case Prod(e1,e2)=> evaluar(e1,a,v) * evaluar(e2,a,v)
    case Div(e1,e2)=> evaluar(e1,a,v) / evaluar(e2,a,v)
    case Logaritmo(e1)=> math.log(evaluar(e1,a,v))
    case Expo(e1,e2)=> math.pow(evaluar(e1,a,v),evaluar(e2,a,v))

  }
def limpiar (f:Expr):Expr= f match {
  case Numero(d)=> Numero(d)
  case Atomo(x)=>Atomo(x)
  case Suma(e1, e2)=> if(e1==Numero(0))limpiar(e2) else if (e2== Numero(0)) limpiar(e1) else limpiar(Suma(limpiar(e1),limpiar(e2)))
  case Resta(e1, e2)=> if(e1==Numero(0))limpiar(e2) else if (e2== Numero(0)) limpiar(e1) else limpiar(Resta(limpiar(e1),limpiar(e2)))
  case Prod(e1,e2)=> if (e1==Numero(0)|| e2==Numero(0))limpiar(Numero(0)) else if (e1==Numero(1)) limpiar(e2) else if (e2==Numero(1)) limpiar(e1) else limpiar((Prod(limpiar(e1),limpiar(e2))))
  case Div(e1, e2)=> if (e1== Numero(0))limpiar(Numero(0)) else limpiar(Div(limpiar((e1)),limpiar(e2)))
  case Logaritmo(e1)=> if (e1== Numero(1)) limpiar(Numero(0)) else limpiar(Logaritmo(limpiar(e1)))
  case Expo(e1,e2)=> if (e2== Numero(0)) limpiar(Numero(1)) else limpiar(Expo(limpiar(e1),limpiar(e2)))

}

  def raizNewton(f:Expr,a:Atomo,x0:Double,ba:(Expr,Atomo,Double)=>Boolean):Double={
    if(ba(f,a,x0)==true)x0
    else raizNewton(f,a,x0 - evaluar(f,a,x0)/evaluar(derivar(f,a),a,x0), ba)
  }
}
