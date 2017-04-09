package recfun

import scala.annotation.{switch, tailrec}

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
    def pascal(c: Int, r: Int): Int = c match {
      case 0 | `r` => 1
      case _ => pascal(c, r-1) + pascal(c-1, r-1)
    }
  
  /**
   * Exercise 2
   */

    def balance(chars: List[Char]): Boolean = {
      val charVal = Map[Char, Int](
        '(' -> 1,
        ')' -> -1
      ).withDefaultValue(0)

      @tailrec
      def inner(c: Int, chars: List[Char]): Boolean = {
        if (c < 0) false
        else chars match {
          case Nil => c == 0
          case char :: tail => inner(c + charVal(char), tail)
        }
      }

      inner(0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money < 0) 0
      else coins match {
        case Nil => 0
        case coin :: tail => countChange(money - coin, coins) + countChange(money, tail)
      }
    }
  }
