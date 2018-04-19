/**
 Try using foldLeft and foldRight with an empty list as the accumulator and :: as the binary operator.
 What results do you get in each case?
 */

List("a", "b", "c").foldLeft(List.empty[String])((acc, item) => item :: acc)
List("a", "b", "c").foldRight(List.empty[String])((item, acc) => item :: acc)
