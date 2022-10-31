package object Newton {
  trait Expr
    case class Numero (d : Double ) extends Expr
    case class Atomo ( x : Char ) extends Expr
    case class Suma( e1 : Expr , e2 : Expr ) extends Expr
    case class Prod ( e1 : Expr , e2 : Expr ) extends Expr
    case class Resta ( e1 : Expr , e2 : Expr ) extends Expr
    case class Div ( e1 : Expr , e2 : Expr ) extends Expr
    case class Expo ( e1 : Expr , e2 : Expr ) extends Expr
    case class Logaritmo ( e1 : Expr ) extends Expr
    //case class Nega (e1: Expr) extends  Expr

    def mostrar ( e : Expr ) : String = {
      e match
        case Numero(n) => n.toString
        case Atomo(n) => n.toString
        case Suma(e1, e2)=> "( "+mostrar(e1) +" + "+ mostrar(e2)+" )"
        case Resta(e1, e2) => "( "+mostrar(e1) +" - "+ mostrar(e2)+" )"
        case Prod(e1, e2) => "( "+mostrar(e1) +" * "+ mostrar(e2)+" )"
        case Div(e1, e2) => "( "+mostrar(e1) +" / "+ mostrar(e2)+" )"
        case Expo(e1, e2) => "( "+mostrar(e1) +" ^ "+ mostrar(e2)+" )"
        case Logaritmo(e1) => "( "+"lg( "+mostrar(e1)+" ) )"
    }

  def derivar(f: Expr, a: Atomo): Expr = {
    f match
      case Numero(a) => Numero(0)
      case Atomo(value) => {
        if (value == a.x) Numero(1) else Numero(0)
      }
      case Suma(e1, e2) => Suma(derivar(e1, a), derivar(e2, a))
      case Resta(e1, e2) => Resta(derivar(e1, a), derivar(e2, a))
      case Prod(e1, e2) => Suma(Prod(derivar(e1, a), e2), Prod(e1, derivar(e2, a)))
      case Div(e1, e2) => Div(Resta(Prod(derivar(e1, a), e2), Prod(e1, derivar(e2, a))), Expo(e2, Numero(2)))
      case Expo(e1, e2) => Prod(Expo(e1, e2), Suma(Div(Prod(derivar(e1, a), e2), e1), Prod(derivar(e2, a), Logaritmo(e1))))
      case Logaritmo(e1) => if (derivar(e1, a) == a) Div(Numero(1), e1) else Numero(0)
  }

  def evaluar ( f : Expr , a : Atomo , v : Double ) : Double = {
    f match
      case Numero(v) => v
      case Atomo(a) => v
      case Suma(e1, e2) =>  evaluar(e1,a, v) + evaluar(e2,a, v)
      case Resta(e1, e2) => evaluar(e1,a, v) - evaluar(e2,a, v)
      case Prod(e1, e2) =>  evaluar(e1,a, v) * evaluar(e2,a, v)
      case Div(e1, e2) =>  evaluar(e1,a, v)/evaluar(e2,a, v)
      case Expo(e1, e2) =>  math.pow(evaluar(e1,a, v),evaluar(e2,a, v))
      case Logaritmo(e1) =>math.log(evaluar(e1,a, v))
  }

  def limpiar ( f : Expr ) : Expr = {
    f match
      case Numero(n) => Numero(n)
      case Atomo(n) => Atomo(n)

      case Suma(e1, e2) =>{
        limpiar(e1) match
          case Numero(0) => limpiar(e2)
          case _ => {
            limpiar(e2) match
              case Numero(0) => limpiar(e1)
              case _ => (Suma(limpiar(e1), limpiar(e2)))
          }
      }
      case Resta(e1, e2)=> {
        limpiar(e1) match
          case Numero(0)=>{
            limpiar(e2) match
              case Numero(0) => Numero(0)
              case _ => Prod(Atomo('-'),limpiar(e2))
          }
          case _ =>{
            limpiar(e2) match
              case Numero(0) => limpiar(e1)
              case _ => Resta(limpiar(e1),limpiar(e2))
          }

      }
      case Prod(e1, e2) =>{
        limpiar(e1) match
          case Numero(0) => limpiar(Numero(0))
          case Numero(1) => limpiar(e2)
          case _ =>{
            limpiar(e2) match
              case Numero(0) => limpiar(Numero(0))
              case Numero(1) => limpiar(e1)
              case _ => (Prod(limpiar(e1),limpiar(e2)))
          }
      }
      case Div(e1, e2) => {
        if (limpiar(e1)==limpiar(e2)) Numero(1) else {
          (limpiar(e1), limpiar(e2)) match
            case (_, Numero(1)) => limpiar(e1)
            case (Numero(0), _) => Numero(0)
            case (_, _) => Div(limpiar(e1), limpiar(e2))
        }
      }
      case Expo(e1,e2) => {

            (limpiar(e1), limpiar(e2)) match
              case (_, Numero(1)) => limpiar(e1)
              case (Numero(0), _) => Numero(0)
              case (_, _) => Expo(limpiar(e1), limpiar(e2))

      }

      case Logaritmo(e1) =>
        {
          (limpiar(e1)) match
            case Numero(1) => Numero(0)
            case _ => Logaritmo(limpiar(e1))
        }

  }

  def raizNewton ( f : Expr , a : Atomo , x0 : Double , ba : ( Expr , Atomo , Double )=> Boolean ) : Double = {
      if(ba(f,a,x0)) x0 else raizNewton(f,a,x0-(evaluar(f,a,x0)/evaluar(derivar(f,a),a,x0)),ba)
    }
}
