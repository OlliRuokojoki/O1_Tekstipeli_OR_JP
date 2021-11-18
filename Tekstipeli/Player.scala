package o1.adventure

import scala.collection.mutable.Map


/** A `Player` object represents a player character controlled by the real-life user of the program.
  *
  * A player object's state is mutable: the player's location and possessions can change, for instance.
  *
  * @param startingArea  the initial location of the player */
class Player(startingArea: Area) {

  private var currentLocation = startingArea        // gatherer: changes in relation to the previous location
  private var quitCommandGiven = false              // one-way flag
  private var playerItems = Map[String, Item]()


  /** Determines if the player has indicated a desire to quit the game. */
  def hasQuit = this.quitCommandGiven


  /** Returns the current location of the player. */
  def location = this.currentLocation


  /** Attempts to move the player in the given direction. This is successful if there
    * is an exit from the player's current location towards the direction name. Returns
    * a description of the result: "You go DIRECTION." or "You can't go DIRECTION." */
  def go(direction: String) = {
    val destination = this.location.neighbor(direction)
    this.currentLocation = destination.getOrElse(this.currentLocation)
    if (destination.isDefined) "You go " + direction + "." else "You can't go " + direction + "."
  }


  /** Causes the player to rest for a short while (this has no substantial effect in game terms).
    * Returns a description of what happened. */
  def rest() = {
    "You rest for a while. Better get a move on, though."
  }


  /** Signals that the player wants to quit the game. Returns a description of what happened within
    * the game as a result (which is the empty string, in this case). */
  def quit() = {
    this.quitCommandGiven = true
    ""
  }


  /** Returns a brief description of the player's state, for debugging purposes. */
  override def toString = "Now at: " + this.location.name

  def drop(itemName: String): String = {
    if (playerItems.contains(itemName)) {
      currentLocation.addItem(playerItems.get(itemName).head)
      "You drop the " + playerItems.remove(itemName).head.name}
    else "You don't have that"
  }

  def examine(itemName: String): String = {
    if (!playerItems.contains(itemName)) "If you want to examine something, you need to pick it up first."
    else {
      playerItems.get(itemName) match {
        case None => "no item"
        case Some(loytyy) => "You look closely at the " + playerItems.getOrElse(itemName, "nothing") + ".\n" + loytyy.description
      }
    }
  }

  def get(itemName: String): String = {

    /*
    currentLocation.items.get(currentLocation.name) match {
      case None => "There is no " + itemName + " here to pick up."
      case Some(loytyy) => {
        if (loytyy.name == itemName) {
          playerItems += itemName -> loytyy
          currentLocation.items -= currentLocation.name
          "You pick up the " + itemName
        }
        else "There is no " + itemName + " here to pick up."
      }
    }*/

    if (currentLocation.contains(itemName)) {
      playerItems += itemName -> currentLocation.removeItem(itemName).head
      "You pick up the " + itemName
    }
    else {
     "There is no " + itemName + " here to pick up."
    }

  }

  def has(itemName: String): Boolean = {
    playerItems.contains(itemName)
  }

  def inventory: String = {
    if (playerItems.isEmpty) "You are empty-handed."
    else
      "You are carrying:\n" +
      playerItems.keys.mkString("\n")
  }


}


