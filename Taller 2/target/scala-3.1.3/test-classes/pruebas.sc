import Derivacion._

val cte = ( x : Double ) => 5.0
val f = ( x : Double ) => ( x*x )
val g = ( x : Double ) => ( x*x*x )
val r =(x:Double)=> (1/x)
val e =(x:Double)=> Math.exp(x)
val l =(x:Double) => Math.log(x)
val s =(x:Double) => Math.sin(x)
val sr =(x:Double)=> math.sqrt(x)
val p =(x:Double)=> x+2
val c =(x:Double)=> math.cos(x)

//1.1 Derivador Generico
derivada(f)(3) // f'=2*x=2*3=6
derivada(cte)(3) // f'=cte=0
derivada(g)(6) // f'=3*x*x=3*6*6=108
derivada(l)(1) //f'=1/x=1/1=1
derivada(s)(Math.PI/2) //f'=cos(PI/2)=cos(90°) =0

//1.2.1 Derivada de una Suma
derivadaSuma(f,g)(-2)
derivadaSuma(e,s)(math.Pi)
derivadaSuma(l,l)(math.E)
derivadaSuma(r,f)(2)
derivadaSuma(cte,sr)(4)

//1.2.2 Derivada de una resta
derivadaResta(f,g)(-2)
derivadaResta(e,s)(math.Pi)
derivadaResta(l,l)(math.E)
derivadaResta(r,f)(2)
derivadaResta(cte,sr)(4)

//1.2.3 Derivada de un Producto
derivadaMult(f,g)(2)
derivadaMult(g,r)(-3)
derivadaMult(f,cte)(6)
derivadaMult(cte,r)(3)
derivadaMult(l,sr)(2)

//1.2.4 Derivada de un Cociente
derivadaDiv(f,g)(-4)
derivadaDiv(cte,f)(2)
derivadaDiv(l,f)(3)
derivadaDiv(s,c)(math.Pi)
derivadaDiv(p,f)(-6)