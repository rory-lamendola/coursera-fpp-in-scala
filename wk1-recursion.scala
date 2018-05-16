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

  def fib(n : Int) : Int = {
    @annotation.tailrec
    def go(n : Int, first : Int = 0, second : Int = 1) : Int = {
      if (n == 0) first
      else if (n == 1) second
      else go(n - 1, second, first + second)
    }
    go(n)
  }

  fib(1)

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || c == r) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def count(chars: List[Char], acc: Int): Int = {
      if (chars.isEmpty || acc < 0) acc
      else {
        val c = chars.head
        val n = {
          if (c == '(') acc + 1
          else if (c == ')') acc - 1
          else acc
        }
        count(chars.tail, n)
      }
    }
    count(chars, 0) == 0

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
