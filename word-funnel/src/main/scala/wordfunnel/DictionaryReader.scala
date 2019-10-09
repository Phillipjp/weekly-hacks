package wordfunnel

object DictionaryReader {

  def readInDictionary(path: String): Seq[String]={
    val source = scala.io.Source.fromFile(path)
    val lines = source.getLines().toSeq
    lines
  }

}
