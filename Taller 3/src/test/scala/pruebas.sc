import Newton._

val expr0 =Numero(2)
val expr1=Suma(Atomo( 'y' ) , Numero ( 2 ) )
val expr2=Prod (Atomo( 'x' ) , Atomo( 'x' ) )
val expr3=Suma( expr1 , Expo ( expr2 , Numero ( 5 ) ) )
val expr4=Logaritmo (Atomo( 'x' ) )
val expr4_1 = Logaritmo(Numero(5))
val expr5=Prod ( Div ( expr1 , expr2 ) , Resta ( expr3 , expr4 ) )
val expr6=Expo (Atomo( 'x' ) , Numero ( 3 ) )
val expr7=Div(expr2,Atomo('x'))
val expr8=Atomo('x')
val expr9=Resta(Atomo('x'),Atomo('y'))
val expr10=Suma(Atomo('x'),Numero(0))

mostrar(expr0)
mostrar(expr1)
mostrar(expr2)
mostrar(expr3)
mostrar(expr4)
mostrar(expr4_1)
mostrar(expr5)
mostrar(expr6)

//Derivadas
mostrar(derivar(expr0,Atomo('x')))
mostrar(derivar(expr1,Atomo('x')))
mostrar(derivar(expr2,Atomo('x')))
mostrar(derivar(expr7,Atomo('x')))
mostrar(derivar(expr8,Atomo('k')))
mostrar(derivar(expr4,Atomo('y')))
mostrar(expr3)
mostrar(derivar(expr3,Atomo('y')))
mostrar(derivar(expr4_1,Atomo('t')))
mostrar(expr4)
mostrar(derivar(expr4,Atomo('x')))

//test profesor
mostrar ( derivar ( expr6 , Atomo( 'x' ) ) )
mostrar ( derivar ( expr2 , Atomo( 'x' ) ) )
mostrar ( derivar ( expr2 , Atomo( 'y' ) ) )
mostrar ( derivar (Suma(Atomo('k') , Prod (Numero ( 3.0 ) , Atomo( 'x' ) ) ) , Atomo( 'x') ) )

//test profesor evaluar
mostrar (Numero ( 5) )
evaluar(Numero ( 5) , Atomo('x' ) , 1 )
mostrar (Atomo('x' ) )
evaluar(Atomo('x' ) ,Atomo('x' ) , 5)
mostrar (Suma( expr1 , expr2 ) )
evaluar(Suma( expr1 , expr2 ) ,Atomo('x' ) , 5 )
mostrar ( Prod ( expr1 , expr2 ) )
evaluar( Prod ( expr1 , expr2 ) ,Atomo('x' ) , 5 )
mostrar ( Resta ( expr1 , expr2 ) )
evaluar( Resta ( expr1 , expr2 ) ,Atomo('x' ),5.0 )
mostrar ( Div ( expr1 , expr2 ) )
evaluar( Div ( expr1 , expr2 ) ,Atomo('x' ) , 5  )
mostrar (Expo ( expr1 , expr2 ) )
evaluar(Expo ( expr1 , expr2 ) ,Atomo('x' ) , 5  )
mostrar ( Logaritmo ( expr1 ) )
evaluar( Logaritmo ( expr1 ) ,Atomo('x' ) , 5  )

//Test limpiar
mostrar(limpiar(derivar(expr1,Atomo('x'))))
mostrar(expr9)
mostrar(derivar(expr9,Atomo('y')))
mostrar(limpiar(derivar(expr9,Atomo('y'))))

mostrar(derivar(expr1,Atomo('x')))
mostrar(limpiar(derivar(expr1,Atomo('x'))))

val test=Prod(Numero(0),Atomo('x'))
mostrar(test)
mostrar(limpiar(test))

mostrar(derivar ( expr2 , Atomo( 'y' ) ) )
mostrar(limpiar(derivar ( expr2 , Atomo( 'y' ) ) ))

mostrar(expr10)
mostrar(limpiar(expr10))

mostrar(expr2)
mostrar(limpiar(expr2))

mostrar(expr4_1)
mostrar(limpiar(derivar(expr4_1,Atomo('x'))))



mostrar ( derivar (Suma(Atomo('k') , Prod (Numero ( 3.0 ) , Atomo( 'x' ) ) ) , Atomo( 'x') ) )
mostrar(limpiar( derivar (Suma(Atomo('k') , Prod (Numero ( 3.0 ) , Atomo( 'x' ) ) ) , Atomo( 'x') ) ))

mostrar(Suma( expr1 , expr2 ))
mostrar(limpiar(Suma( expr1 , expr2 )))

mostrar(derivar(expr2,Atomo('x')))
mostrar(limpiar(derivar(expr2,Atomo('x'))))

val test2 = Resta(Suma(Numero(1),Numero(0)),Prod(Numero(1),Suma(Atomo('x'),Numero(0))))
mostrar(test2)
mostrar(limpiar(test2))

val test3 = Div(Numero(0),expr1)
mostrar(test3)
mostrar(limpiar(test3))

val test4= Div(expr1,expr1)
mostrar(test4)
mostrar(limpiar(test4))

//raiz Newton
def buenaAprox ( f : Expr , a : Atomo , d : Double ) : Boolean = {
  evaluar ( f , a , d ) < 0.001
}


val e5 = Prod(Numero(3),Atomo('x'))
mostrar(e5)
mostrar((derivar(e5,Atomo('x'))))

val e6 = Prod(Atomo('x'),Atomo('y'))
mostrar(e6)
mostrar(derivar(e6,Atomo('y')))

val e1= Resta ( Prod (Atomo( 'x') ,Atomo( 'x') ) , Numero ( 2.0 ))
val e2= Resta ( Prod (Atomo( 'x') ,Atomo( 'x') ) , Numero ( 4.0 ))
val e3 = Suma( Resta ( Prod (Atomo( 'x') ,Atomo( 'x') ) , Numero ( 4.0 ) ),Prod(Numero( 3.0 ),Atomo('x')))
//val e4 = Expo(Atomo('x'),Div(Numero(1),Numero(2)))





mostrar(e1)
mostrar(limpiar(derivar(e1,Atomo('x'))))

raizNewton ( e1 , Atomo( 'x') , 2.0 , buenaAprox )
raizNewton ( e2 , Atomo( 'x') , 2.0 , buenaAprox )
raizNewton ( e3 , Atomo( 'x') , 2.0 , buenaAprox )

/*

mostrar(e4)
mostrar(limpiar(derivar(e4,Atomo('x'))))
(evaluar(derivar(e4,Atomo('x')),Atomo('x'),2.0))
*/
//raizNewton(e4,Atomo('x'),2.0,buenaAprox)