package u06lab.code

import u06lab.code.TicTacToe.isOver

import scala.Console.println

object TicTacToe extends App{
  sealed trait Player{
    def other: Player = this match {case X => O; case _ => X}
    override def toString: String = this match {case X => "X"; case _ => "O"}
  }
  case object X extends Player
  case object O extends Player

  case class Mark(x: Int, y: Int, player: Player)
  type Board = List[Mark]
  type Game = List[Board]

  def find(board: Board, x: Int, y: Int): Option[Player] = board collectFirst {
    case Mark(`x`, `y`, player) => player
  }

  def placeAnyMark(board: Board, player: Player): Seq[Board] = {
    for {
      y <- 0 to 2
      x <- 0 to 2
      mark = Mark(x, y, player)
      if find(board, x, y).isEmpty //filter (add to board only if there is not a mark already placed)
      } yield mark :: board
  }

  def computeAnyGame(player: Player, moves: Int): Stream[Game] = moves match {
    case 0 => Stream[Game](List[Board](Nil))
    case _ => for {
      game <- computeAnyGame(player.other, moves - 1)
      nextBoard <- placeAnyMark(game.head, player)
      if !isOver(nextBoard) //filter
    } yield nextBoard :: game
  }

  def isOver(board: Board): Boolean = board.groupBy(_.player).values.exists(marks =>
    marks.groupBy(_.y).values.size >= 3 //rows
      || marks.groupBy(_.x).values.size >= 3 //cols
      || marks.count(m => m.x == m.y) >= 3 //diagonal
      || marks.count(m => m.x + m.y == 2) >= 3 //the addition must be 2 in the anti diagonal
  )


  def printBoards(game: Seq[Board]): Unit =
    for (y <- 0 to 2; board <- game.reverse; x <- 0 to 2) {
      print(find(board, x, y) map (_.toString) getOrElse ("."))
      if (x == 2) { print(" "); if (board == game.head) println()}
    }

  // Exercise 1: implement find such that..
  println(find(List(Mark(0,0,X)),0,0)) // Some(X)
  println(find(List(Mark(0,0,X),Mark(0,1,O),Mark(0,2,X)),0,1)) // Some(O)
  println(find(List(Mark(0,0,X),Mark(0,1,O),Mark(0,2,X)),1,1)) // None


  // Exercise 2: implement placeAnyMark such that..
  printBoards(placeAnyMark(List(),X))
  //... ... ..X ... ... .X. ... ... X..
  //... ..X ... ... .X. ... ... X.. ...
  //..X ... ... .X. ... ... X.. ... ...

  printBoards(placeAnyMark(List(Mark(0,0,O)),X))
  //O.. O.. O.X O.. O.. OX. O.. O..
  //... ..X ... ... .X. ... ... X..
  //..X ... ... .X. ... ... X.. ...

  // Exercise 3 (ADVANCED!): implement computeAnyGame such that..
  computeAnyGame(O, 6) foreach {g => printBoards(g); println()}
  //... X.. X.. X.. XO.
  //... ... O.. O.. O..
  //... ... ... X.. X..
  //              ... computes many such games (they should be 9*8*7*6 ~ 3000).. also, e.g.:
  //
  //... ... .O. XO. XOO
  //... ... ... ... ...
  //... .X. .X. .X. .X.
/*
  // Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
  */
}
