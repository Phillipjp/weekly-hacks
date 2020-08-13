package blobs

object Cube {

  def makeCubeCoords(startX: Int, startY: Int, startZ: Int, endX: Int, endY: Int, endZ: Int): Seq[(Int, Int, Int)] = {
    val grid = Grid.makeGridCoords(startX, startY, endX, endY)
    (startZ to endZ).flatMap(z => grid.map{case(x,y) => (x,y,z)})
  }

  def makeBlobCube(coords: Seq[(Int, Int, Int)], blobs: Seq[Blob3D]): Map[(Int, Int, Int), Seq[Blob3D]] = {
    val groupedBlobs: Map[(Int, Int, Int), Seq[Blob3D]] = blobs.groupBy(blob => (blob.coord.x, blob.coord.y, blob.coord.z))

    coords.map{ coord =>
      coord -> groupedBlobs.getOrElse(coord, Seq())
    }.toMap
  }

  def printCubeWithBlobs(cube: Map[(Int, Int, Int), Seq[Blob3D]]): Unit = {
    val xCoords = cube.keys.map(_._1)
    val maxX = xCoords.max
    val minX = xCoords.min

    val yCoords = cube.keys.map(_._2)
    val maxY = yCoords.max
    val minY = yCoords.min

    val zCoords = cube.keys.map(_._3)
    val maxZ = zCoords.max
    val minZ = zCoords.min

    for(z <- maxZ to minZ by -1) {
      for (y <- maxY to minY by -1) {
        for (x <- minX to maxX) {
          val blobs = cube((x, y, z))
          if (blobs.isEmpty) {
            print(". ")
          }
          else {
            print(s"${blobs.head.size} ")
          }
        }
        println()
      }
      println("------------------------")
    }
    println()
  }

}
