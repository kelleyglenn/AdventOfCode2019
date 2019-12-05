package day4

object Puzzle1 {

  @scala.annotation.tailrec
  def hasDouble(s: String): Boolean = {
    if (s.length < 2) false else s.head == s.tail.head || hasDouble(s.tail)
  }

  @scala.annotation.tailrec
  def hasStrictDouble(s: String): Boolean = {
    if (s.length < 2) false
    else if (s.charAt(0) == s.charAt(1)) {
      // If there's not a 3rd character in a row that matches, then we have a strict double
      // Otherwise, we need to skip all consecutive matches and check the rest of the string
      if (s.length < 3 || s.charAt(0) != s.charAt(2)) true else {
        val next = s.indexWhere(_ != s.head)
        if (next < 0 || next >= s.length) false else
          hasStrictDouble(s.substring(next))
      }
    } else
      hasStrictDouble(s.tail)
  }

  @scala.annotation.tailrec
  def decreases(s: String): Boolean = {
    if (s.length < 2) false else s.head > s.tail.head || decreases(s.tail)
  }

  def meetsCriteria(i: Integer): Boolean = {
    val s = i.toString
    s.length == 6 && hasDouble(s) && !decreases(s)
  }

  def meetsCriteria2(i: Integer): Boolean = {
    val s = i.toString
    s.length == 6 && hasStrictDouble(s) && !decreases(s)
  }
}
