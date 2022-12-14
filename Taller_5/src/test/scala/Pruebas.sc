import Matrices._
import org.scalameter._
import Benchmark._

val a3 = Vector(Vector(4, 7, 5, 0, 8, 3, 5, 6), Vector(5, 2, 3, 1, 1, 9, 0, 4), Vector(3, 8, 8, 4, 5, 4, 0, 0), Vector(7, 2, 1, 9, 4, 3, 0, 5), Vector(0, 7, 3, 5, 4, 9, 6, 6), Vector(2, 7, 7, 1, 3, 1, 9, 1), Vector(4, 9, 2, 0, 3, 1, 1, 5), Vector(8, 5, 7, 2, 2, 6, 6, 9))
val b3 = Vector(Vector(2, 3, 6, 1, 0, 1, 2, 4), Vector(8, 3, 8, 2, 6, 5, 3, 0), Vector(2, 8, 2, 0, 0, 3, 5, 3), Vector(3, 6, 8, 8, 8, 0, 3, 0), Vector(9, 9, 9, 6, 4, 0, 9, 2), Vector(4, 2, 2, 2, 2, 2, 1, 0), Vector(6, 5, 9, 7, 8, 0, 9, 8), Vector(6, 2, 1, 3, 1, 2, 3, 0))

val test1 ="Vector(Vector(224, 188, 219, 125, 126, 72, 192, 87), Vector(104, 86, 91, 53, 46, 50, 64, 31), Vector(159, 174, 183, 89, 108, 75, 131, 46), Vector(137, 141, 179, 128, 111, 36, 106, 39), Vector(221, 171, 216, 156, 170, 74, 168, 65), Vector(168, 165, 201, 110, 137, 62, 175, 107), Vector(151, 99, 143, 64, 81, 67, 97, 36), Vector(208, 185, 211, 127, 123, 84, 177, 105))"
val test2="Vector(Vector(224, 188, 219, 125, 126, 72, 192, 87), Vector(104, 86, 91, 53, 46, 50, 64, 31), Vector(159, 174, 183, 89, 108, 75, 131, 46), Vector(137, 141, 179, 128, 111, 36, 106, 39), Vector(221, 171, 216, 156, 170, 74, 168, 65), Vector(168, 165, 201, 110, 137, 62, 175, 107), Vector(151, 99, 143, 64, 81, 67, 97, 36), Vector(208, 185, 211, 127, 123, 84, 177, 105))"
val test3="Vector(Vector(224, 188, 219, 125, 126, 72, 192, 87), Vector(104, 86, 91, 53, 46, 50, 64, 31), Vector(159, 174, 183, 89, 108, 75, 131, 46), Vector(137, 141, 179, 128, 111, 36, 106, 39), Vector(221, 171, 216, 156, 170, 74, 168, 65), Vector(168, 165, 201, 110, 137, 62, 175, 107), Vector(151, 99, 143, 64, 81, 67, 97, 36), Vector(208, 185, 211, 127, 123, 84, 177, 105))"
val test4="Vector(Vector(224, 188, 219, 125, 126, 72, 192, 87), Vector(104, 86, 91, 53, 46, 50, 64, 31), Vector(159, 174, 183, 89, 108, 75, 131, 46), Vector(137, 141, 179, 128, 111, 36, 106, 39), Vector(221, 171, 216, 156, 170, 74, 168, 65), Vector(168, 165, 201, 110, 137, 62, 175, 107), Vector(151, 99, 143, 64, 81, 67, 97, 36), Vector(208, 185, 211, 127, 123, 84, 177, 105))"
val test5="Vector(Vector(224, 188, 219, 125, 126, 72, 192, 87), Vector(104, 86, 91, 53, 46, 50, 64, 31), Vector(159, 174, 183, 89, 108, 75, 131, 46), Vector(137, 141, 179, 128, 111, 36, 106, 39), Vector(221, 171, 216, 156, 170, 74, 168, 65), Vector(168, 165, 201, 110, 137, 62, 175, 107), Vector(151, 99, 143, 64, 81, 67, 97, 36), Vector(208, 185, 211, 127, 123, 84, 177, 105))"
val test6="Vector(Vector(224, 188, 219, 125, 126, 72, 192, 87), Vector(104, 86, 91, 53, 46, 50, 64, 31), Vector(159, 174, 183, 89, 108, 75, 131, 46), Vector(137, 141, 179, 128, 111, 36, 106, 39), Vector(221, 171, 216, 156, 170, 74, 168, 65), Vector(168, 165, 201, 110, 137, 62, 175, 107), Vector(151, 99, 143, 64, 81, 67, 97, 36), Vector(208, 185, 211, 127, 123, 84, 177, 105))"

val a =""+multMatriz(a3,b3)
val b=""+multMatrizPar(a3,b3)
val c=""+multMatrizRec(a3,b3)
val d=""+multMatrizRecPar(a3,b3)
val e=""+multStrassen(a3,b3)
val f=""+multStrassenPar(a3,b3)
if(test1==a) true else false
if(test2==b) true else false
if(test3==c) true else false
if(test4==d) true else false
if(test5==e) true else false
if(test6==f) true else false


