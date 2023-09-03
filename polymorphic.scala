
def findFirst[A](as: Array[A], p: A => Boolean): Int =
    @annotation.tailrec 
    def loop(n: Int): Int = 
        if n >= as.length then -1 
        else if p(as(n)) then n
        else loop(n + 1) 
    
    loop(0)

def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = 
    @annotation.tailrec
    def loop(n: Int): Boolean = 
        if n>= as.length-1 then true
        else if gt(as(n), as(n+1)) then false
        else loop(n+1) 
    loop(0)

def curry[A, B, C](f: (A, B) => C): A => (B => C) = 
    a => b => f(a,b) 

def uncurry[A, B, C](f: A => B => C): (A, B) => C = 
    (a, b) => f(a)(b)

def compose[A, B, C](f: B => C, g: A => B): A => C = 
    a => f(g(a))

@main def main: Unit = 
    println(isSorted(Array(1, 2, 3), _ > _)) 