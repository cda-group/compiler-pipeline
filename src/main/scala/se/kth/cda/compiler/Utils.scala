package se.kth.cda.compiler

object Utils {

  // Y-combinator
  def fix[A, B](f: (A => B) => A => B): A => B = f(fix(f))(_)

}
