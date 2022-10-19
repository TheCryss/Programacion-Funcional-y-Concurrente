import Derivacion._

val cte = ( x : Double ) => 2 . 0
val f = ( x : Double ) => ( x∗x )
val g = ( x : Double ) => ( x∗x∗x )
val h = ( x : Double ) => f ( x ) / g ( x )
val j = ( x : Double ) => g ( x ) / f ( x )
f
val h1 = derivadaDiv ( f , g )
val h2 = derivadaDiv ( f , g )
val h3 = derivadaDiv ( g , f )
val h4 = derivadaDiv ( g , f )
h1 ( 2 )
h2 ( 2 )
h3 ( 2 )
h4 ( 2 )
derivada ( cte ) ( 5 )
derivada ( cte ) ( 6 )