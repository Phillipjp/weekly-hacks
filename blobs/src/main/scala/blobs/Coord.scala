package blobs

trait Coord {
  val x: Int
  val y: Int
}

case class Coord2D(x: Int, y: Int) extends Coord
case class Coord3D(x: Int, y: Int, z: Int) extends Coord

trait CoordWrapper[C <: Coord]{
  val coords: Seq[C]
}

case class Coord2DWrapper(coords: Seq[Coord2D]) extends CoordWrapper[Coord2D]
case class Coord3DWrapper(coords: Seq[Coord3D]) extends CoordWrapper[Coord3D]