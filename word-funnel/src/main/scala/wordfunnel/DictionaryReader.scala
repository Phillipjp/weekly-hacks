package wordfunnel

object DictionaryReader {

  def readInDictionary(path: String): Set[String]={
    val source = scala.io.Source.fromFile(path)
    val lines = source.getLines().toSet
    lines
  }

}
