package day14

class Stoichiometry(val reactions: Set[Reaction]) {
  val productToReaction: Map[String, Reaction] = {
    reactions.map((r: Reaction) => (r.product.name, r)).toMap
  }
  var stockpile: Map[String, Reactant] = Map.empty

  def calcElementCountRequiredForTarget(element: String,
                                        product: String): Long = {
    calcElementRequiredForReactant(element, Reactant(product, 1))
  }

  def calcElementRequiredForReactants(element: String,
                                      reactants: Set[Reactant]): Long = {
    reactants
      .flatMap(takeFromStockpile)
      .map(calcElementRequiredForReactant(element, _))
      .sum
  }

  def calcElementRequiredForReactant(element: String,
                                     needed: Reactant): Long = {
    if (element == needed.name) needed.count
    else {
      takeFromStockpile(needed) match {
        case None => 0
        case Some(n) =>
          calcElementRequiredForReactants(element, reactantsNeeded(n))
      }
    }
  }

  def takeFromStockpile(needed: Reactant): Option[Reactant] = {
    if (stockpile.contains(needed.name)) {
      val stocked: Reactant = stockpile(needed.name)
      if (stocked.count == needed.count) {
        stockpile = stockpile.removed(needed.name)
        None
      } else if (stocked.count < needed.count) {
        stockpile = stockpile.removed(needed.name)
        Some(Reactant(needed.name, needed.count - stocked.count))
      } else {
        stockpile += (stocked.name -> Reactant(
          stocked.name,
          stocked.count - needed.count
        ))
        None
      }
    } else Some(needed)
  }

  def reactantsNeeded(target: Reactant): Set[Reactant] = {
    val reaction: Reaction = productToReaction(target.name)
    val multiplier: Double =
      Math.ceil(target.count.toDouble / reaction.product.count.toDouble)
    val reactants: Set[Reactant] = reaction.reactants.map(
      (r: Reactant) => Reactant(r.name, (r.count * multiplier).toInt)
    )
    val extraCt
      : Long = (reaction.product.count * multiplier).toInt - target.count
    if (extraCt > 0) {
      stockpile += (target.name -> Reactant(target.name, extraCt))
    }
    reactants.flatMap(takeFromStockpile)
  }
}

case class Reactant(name: String, count: Long)

case class Reaction(reactants: Set[Reactant], product: Reactant)
