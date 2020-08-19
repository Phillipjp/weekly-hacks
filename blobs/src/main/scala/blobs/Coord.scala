package blobs

trait Coord {
  val x: Int
  val y: Int
}

case class Coord2D(x: Int, y: Int) extends Coord
case class Coord3D(x: Int, y: Int, z: Int) extends Coord

trait CoordWrapper{
  val coords: Seq[Coord]
}

case class Coord2DWrapper(coords: Seq[Coord2D]) extends CoordWrapper
case class Coord3DWrapper(coords: Seq[Coord3D]) extends CoordWrapper