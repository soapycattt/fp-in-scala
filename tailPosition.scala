def factorial(n: Int): Int = 
    def go(n: Int, acc: Int): Int = 
        if n <= 0 then acc 
        else go(n - 1, n * acc)
    go(n, 1)

def fib(n: Int): Int = 
    @annotation.tailrec
    def go(n: Int, firstNum: Int, secondNum: Int): Int = 
        if n <= 0 then secondNum
        else go(n-1, secondNum, firstNum + secondNum)
    go(n-2, 0, 1)

@main def main(): Unit = 
    println(fib(6))