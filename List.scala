/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
    
/**
    Implement the function tail for removing the first element of a List
(note that the function takes constant time). You can use
sys.error("message") to throw an exception if the List is Nil. In
the next chapter, we’ll look at different ways of handling errors. Be
careful to use the List enum and the Nil case defined here and not
the built-in Scala List and Nil types.
**/
    def tail[A](as: List[A]): List[A] = as match
        case Nil => sys.error("Error")
        case Cons(_, t) => t 

//     Using the same idea, implement the function setHead for replacing
// the first element of a List with a different value

    def setHead[A](as: List[A], newHead: A) = as match
        case Nil => List(newHead)
        case Cons(_, t) => Cons(newHead, t)

//     Implement the function drop, which removes the first n elements
// from a list. Dropping n elements from an empty list should return the
// empty list. Note that this function takes time proportional only to the
// number of elements being dropped—we don’t need to make a copy of
// the entire List
    
    
    
    def drop[A](as: List[A], n: Int): List[A] = 
            if n == 0 then as
            else as match 
                case Nil => Nil
                case Cons(_, t) => drop(t, n-1)

//  Implement dropWhile, which removes elements from the List prefix
// as long as they match a predicate
    def append[A](a1: List[A], a2: List[A]): List[A] = a1 match 
        case Nil => a2 
        case Cons(h, t) => Cons(h, append(t, a2))
    
    
    def dropWhile[A](as: List[A], f: A => Boolean): List[A] =
        as match
            case Cons(h, t) => if f(h) then dropWhile(t, f) else as 
            case Nil => Nil

//  Not everything works out so nicely. Implement a function, init, that
// returns a List consisting of all but the last element of a List, so
// given List(1,2,3,4), init will return List(1,2,3). Why can’t this
// function be implemented in constant time (that is, runtime that’s
// proportional to the size of the list) like tail?
    def init[A](as: List[A]): List[A] =
    as match
        case Nil => sys.error("init of empty list")
        case Cons(_,Nil) => Nil
        case Cons(h,t) => Cons(h,init(t))
    
    def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B = as match 
        case Nil => acc 
        case Cons(x, xs) => f(x, foldRight(xs, acc, f)) 
  
    def sumViaFoldRight(ns: List[Int]) = 
        foldRight(ns, 0, (x,y) => x + y) 
    
    def productViaFoldRight(ns: List[Double]) = 
        foldRight(ns, 1.0, _ * _) 

    // def length[A](ns: List[A]): Int = 
    //     foldRight(ns, 0, (_, y) => y + 1)

    def length[A](l: List[A]): Int =
    foldRight(l, 0, (_,acc) => acc + 1)



@main def main: Unit = 
    println(List.tail[Int](as=List(1,2,3,4,5)))
    println(List.setHead[Int](as=List(1,2,3,4,5), newHead=100))
    println(List.drop[Int](as=List(1,2,3,4,5), n=2))
    println(List.dropWhile[Int](as=List(1,2,3,4,5), f= _ < 3))
    println(List.init[Int](as=List(1,2,3,4,5)))
    
    println(List.sumViaFoldRight(ns=List(1,2,3,4,5)))

