package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = r match {
    case 0 => 1
    case _ => {
      (c != r).compare(false) * pascal(c, r-1) + 
      (c != 0).compare(false) * pascal(c-1, r-1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(input: List[Char]): Boolean = {
    def balanceExt(input: List[Char], count: Int): Int = input match {
      case Nil => count
      case x :: xs => x match {
        case '(' => Math.min(count, balanceExt(xs, count+1))
        case ')' => Math.min(count, balanceExt(xs, count-1))
        case _   => balanceExt(xs, count)
      }
    }
    balanceExt(input, 0) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = coins match {
    case Nil => 0
    case _   => money compare 0 match {
      case -1 => 0
      case 0  => 1
      case _  => {
        countChange(money-coins.head, coins) +
        countChange(money, coins.tail)
      }
    }
  }
}
