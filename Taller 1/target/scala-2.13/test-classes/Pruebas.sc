import Recursion .{mcdTFA, mcdEBez , fibonacciA , fibonacciI }

// Pruebas mcdTFA
mcdTFA (List(2),List(3),List(5)) // mcd(25,125)

mcdTFA(List(1,2),List(2,1),List(2,4)) // mcd(32,16)

mcdTFA(List(1,0),List(0,1),List(5,7)) //mcd(5,7)

mcdTFA(List(1,0,2),List(2,1,1),List(2,3,4)) //mcd(32,48)

mcdTFA(List(3,0,2,1),List(2,1,1,0),List(2,3,4,5)) //mcd(640,48)

// Pruebas mcdEBez
mcdEBez(19,7)

mcdEBez(12,6)

mcdEBez(18,12)

mcdEBez(34,34)

mcdEBez(3890,2475)

// Pruebas fibonacciA
fibonacciA(0)

fibonacciA(1)

fibonacciA(5)

fibonacciA(6)

fibonacciA(7)

// Pruebas fibonacciI
fibonacciI(0)

fibonacciI(1)

fibonacciI(2)

fibonacciI(3)

fibonacciI(4)
