import Matrices._
import org.scalameter._
import Benchmark._
val a10 =matrizAlAzar(1024,10)
val b10 =matrizAlAzar(1024,10)

val a0 =matrizAlAzar(1,10)
val b0 =matrizAlAzar(1,10)
val a1 =matrizAlAzar(2,10)
val b1 =matrizAlAzar(2,10)
val a2 =matrizAlAzar(4,10)
val b2 =matrizAlAzar(4,10)
val a3 =matrizAlAzar(8,10)
val b3 =matrizAlAzar(8,10)
val a4 =matrizAlAzar(16,10)
val b4 =matrizAlAzar(16,10)
val a5 =matrizAlAzar(32,10)
val b5 =matrizAlAzar(32,10)
val a6 =matrizAlAzar(64,10)
val b6 =matrizAlAzar(64,10)
val a7 =matrizAlAzar(128,10)
val b7 =matrizAlAzar(128,10)
val a8 =matrizAlAzar(256,10)
val b8 =matrizAlAzar(256,10)
val a9 =matrizAlAzar(512,10)
val b9 =matrizAlAzar(512,10)

compararAlgoritmos(multMatrizRec,multMatrizRecPar)(a0,b0)

compararAlgoritmos(multMatrizRec,multMatrizRecPar)(a1,b1)

compararAlgoritmos(multMatrizRec,multMatrizRecPar)(a2,b2)

compararAlgoritmos(multMatrizRec,multMatrizRecPar)(a3,b3)

compararAlgoritmos(multMatrizRec,multMatrizRecPar)(a4,b4)

compararAlgoritmos(multMatrizRec,multMatrizRecPar)(a5,b5)

compararAlgoritmos(multMatrizRec,multMatrizRecPar)(a6,b6)

compararAlgoritmos(multMatrizRec,multMatrizRecPar)(a7,b7)

compararAlgoritmos(multMatrizRec,multMatrizRecPar)(a8,b8)

compararAlgoritmos(multMatrizRec,multMatrizRecPar)(a9,b9)

compararAlgoritmos(multMatrizRec,multMatrizRecPar)(a10,b10)
//multMatrizRec(a,b)

/*
multMatriz(a,b)
multStrassen(a,b)
multStrassenPar(a,b)
*/
