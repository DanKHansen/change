object Change:
   def findFewestCoins(amount: Int, coins: List[Int]): Option[List[Int]] =
      if amount > 0 then
         val dp = (0 to amount).foldLeft(Vector.fill(amount + 1)(None: Option[List[Int]])) { (acc, amt) =>
            if amt == 0 then acc.updated(amt, Some(List.empty[Int])) else
               val candidates = coins.flatMap { coin => if amt - coin >= 0 then acc(amt - coin).map(coin :: _) else None}
               acc.updated(amt, if candidates.isEmpty then None else Some(candidates.minBy(_.length)))
         }
         dp(amount)
      else None
