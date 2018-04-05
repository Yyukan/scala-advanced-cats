/**
  * The naive implementation of foldRight below is not stack safe.
  * Make it so using Eval
  */

import cats.Eval

def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = as match {
  case head :: tail =>
    fn(head, foldRight(tail, acc)(fn))
  case Nil =>
    acc
}

def evalFoldRight[A, B](list: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] = list match {
  case Nil          => acc
  case head :: tail => Eval.defer(fn(head, evalFoldRight(tail, acc)(fn)))
}

foldRight((1 to 1000).toList, 0)(_ + _)
evalFoldRight((1 to 10000).toList, Eval.Zero)((a, b) => b.map(_ + a)).value