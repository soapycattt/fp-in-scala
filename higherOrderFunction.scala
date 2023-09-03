object MyProgram:
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

    // private def formatAbs(x: Int) = 
    //     val msg = "The absolute value of %d is %d." 
    //     msg.format(x, abs(x)) 
  
    private def formatFactorial(n: Int) = 
        val msg = "The factorial of %d is %d." 
        msg.format(n, factorial(n))

    def formatResult(name: String, n: Int, f: Int => Int) = 
        val msg = "The %s of %d is %d." 
        msg.format(name, n, f(n))

    @main def printAbsAndFactorial: Unit = 
        println(formatFactorial(7))