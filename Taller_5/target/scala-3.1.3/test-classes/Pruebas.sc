import Matrices._
import org.scalameter._
import Benchmark._
//val a =matrizAlAzar(4,5)
//subMatriz(a,0,0,4)
//val b =matrizAlAzar(4,5)

//val Ma=Vector(Vector(3, 0, 1, 3, 5, 0, 4, 0), Vector(4, 0, 2, 1, 0, 2, 5, 5), Vector(2, 4, 1, 0, 5, 1, 4, 4), Vector(1, 2, 2, 4, 5, 3, 2, 2), Vector(2, 4, 5, 5, 5, 3, 2, 2), Vector(1, 4, 3, 1, 2, 2, 0, 4), Vector(4, 2, 1, 2, 0, 5, 2, 2), Vector(2, 3, 3, 2, 5, 4, 1, 1))
//val Mb= Vector(Vector(4, 5, 3, 5, 3, 0, 2, 3), Vector(0, 4, 5, 5, 5, 5, 0, 0), Vector(2, 0, 4, 0, 1, 2, 4, 4), Vector(5, 0, 0, 5, 0, 5, 2, 2), Vector(2, 1, 3, 5, 1, 3, 5, 3), Vector(1, 1, 1, 4, 0, 0, 0, 2), Vector(3, 2, 0, 3, 0, 1, 2, 0), Vector(1, 2, 0, 0, 0, 5, 4, 3))
val a =Vector(Vector(2, 3, 4, 3), Vector(1, 1, 1, 3), Vector(3, 3, 0, 4), Vector(4, 3, 2, 0))
val b =Vector(Vector(2, 0, 0, 1), Vector(3, 1, 2, 4), Vector(0, 4, 2, 1), Vector(2, 3, 3, 2))
multMatrizRec(a,b)


(multMatriz(a,b))
/*
(multMatrizPar(a,b))
(multMatriz(a,b))
compararAlgoritmos(multMatrizPar,multMatriz)(a,b)
*/


