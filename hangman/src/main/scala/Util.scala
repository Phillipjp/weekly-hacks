
object Util  {

  def readInVocab[T](path: String): Seq[T]={
    val source = scala.io.Source.fromFile(path)
    val lines = source.getLines().toSeq.map(_.asInstanceOf[T])
    lines
  }


}
