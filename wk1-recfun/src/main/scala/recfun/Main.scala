package recfun

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
  def pascal(column: Int, row: Int): Int = {
    if (column == 0 || column == row) 1
    else pascal(column - 1, row - 1) + pascal(column, row - 1)
  }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def countOpens(chars: List[Char], acc: Int): Int = {
      if (chars.isEmpty || acc < 0) acc
      else {
        countOpens(chars.tail, chars.head match {
          case '(' => acc + 1
          case ')' => acc - 1
          case _ => acc
        })
      }
    }
    countOpens(chars, 0) == 0
  }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countCombinations(money: Int, coins: List[Int]): Int = {
      if (money == 0 ) 1
      else if (money < 0 || coins.isEmpty) 0
      else countCombinations(money - coins.head, coins) + countCombinations(money, coins.tail)
    }
    countCombinations(money, coins)
  }

  }
