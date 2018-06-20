def countChange(money: Int, coins: List[Int]): Int = {
  def countCombinations(money: Int, coins: List[Int]): Int = {
    if (money == 0 ) 1
    else if (money < 0 || coins.isEmpty) 0
    else countCombinations(money - coins.head, coins) + countCombinations(money, coins.tail)
  }
  countCombinations(money, coins)
}