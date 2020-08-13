package blobs

sealed trait Coords {
  val x: Int
  val y: Int
}

case class Coords2D(x: Int, y: Int) extends Coords

case class Coords3D(x: Int, y: Int, z: Int) extends Coords
