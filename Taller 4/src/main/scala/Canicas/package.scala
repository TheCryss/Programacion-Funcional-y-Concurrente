import scala.::

package object Canicas {
 type Frasco = (Int, Int);
 type Distr = List[Frasco];


 //ponemos el metodo for entre parentesis para convertir IndexSeq a List
 def canincasPosiblesFrasco(f: Int, c: Int): List[Frasco] = {
  (for {
   quantity <- 0 to c
  } yield (f, quantity)).toList
 }

 def canicasPorFrasco(n: Int, c: Int): List[Distr] = {
  (for {a <- 1 to n} yield (canincasPosiblesFrasco(a, c))).toList
 }


 def mezclarLCanicas(lc: List[Distr]): List[Distr] = {
  def aux(comb: List[Distr], frascos: List[Distr]): List[Distr] = {
   if (frascos.isEmpty) {
    comb
   } else {
    //toma cada valor dentro de la lista 'a' y le aplica la cola de lc, de la cual toma el primer valor y esa Lista la descompone y concatena cada valor por separado a 'a'
    aux(for (a <- comb; b <- frascos.head) yield a :+ b, frascos.tail)
   }
  }

  //List(List((1,0)), List((1,1)), List((1,2)), List((1,3)), List((1,4)), List((1,5)))
  val c = for (primer <- lc.head) yield List(primer)
  aux(c, lc.tail)
 }


 def distribucion(m: Int, n: Int, c: Int): List[Distr] = {
  val mezclas = mezclarLCanicas(canicasPorFrasco(n, c))
  def aux(canicas: List[Int]): Boolean = {if (canicas.sum == m) true else false}
  def aux2(combinacion: List[Frasco]): List[Int] = for (canicas <- combinacion) yield (canicas._2)
  for (a <- mezclas; if (aux(aux2(a)))) yield a
 }

 //val test = distribucion(10, 4, 10)


 /*
* Nos devuelve el tamaño del mayor conjunto de valores que forman un valor m
* Donde b es ese tamaño.
* */
 //def maxSize(a:Int,b:Int,lista:List[Int]): Int = {if(a>=10){b}else{maxSize(a+lista.head,b+1,lista.tail)}}

 def onlyInt(group: List[Distr]): List[List[Int]] = {
  def aux2(combinacion: List[Frasco]): List[Int] = for (canicas <- combinacion) yield (canicas._2)

  for (a <- group) yield aux2(a)
 }

 def noZero(group: List[List[Int]]): List[List[Int]] = {
 def aux(num: Int): Boolean = {
  if (num == 0) true else false
 }
 def aux2(value: List[Int]): List[Int] = {
  for (a <- value; if (!aux(a))) yield (a)
 }
 for (a <- group) yield aux2(a)
}

/*
 def agrupaciones(m: Int): List[List[Int]] = {
  def maxSize(a: Int, b: Int, lista: List[Int]): Int = {
   if (a >= 10) {b} else {maxSize(a + lista.head, b + 1, lista.tail)}}
  val listOfValues = (for (a <- 1 to m) yield a).toList
  val n = maxSize(0, 0, listOfValues)

  //Total de Combianciones que forman m
  val combinations = distribucion(m, n, m)
  //Me quedo unicamente con listas de Int

  def onlyInt(group: List[Distr]): List[List[Int]] = {
   def aux2(combinacion: List[Frasco]): List[Int] = for (canicas <- combinacion) yield (canicas._2)
   for (a <- group ) yield aux2(a)
  }
  val onlyNumbers = onlyInt(combinations)
  
  def noZero(group: List[List[Int]]):List[List[Int]]={
   def aux(num:Int):Boolean={if(num==0) true else false}
   def aux2(value: List[Int]): List[Int] = {for (a <- value;if (!aux(a))) yield (a)}
   for (a <- group) yield aux2(a)
  }
 }
*/
}

