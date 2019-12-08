package day8

object Puzzle1 {
  def seqToLayers(s:Seq[Char], dimensions: (Int, Int)): Seq[Seq[Char]] ={
    val charsPerLayer: Int = dimensions._1 * dimensions._2
    if (s.size % charsPerLayer != 0) throw new IllegalArgumentException("Incomplete layers")
    val layerNum = s.size / charsPerLayer
    for(i <- 0 until layerNum) yield s.splitAt(i * charsPerLayer)._2.take(charsPerLayer)
  }

  def findLayerWithLeast(layers: Seq[Seq[Char]], c:Char): Seq[Char] = {
    layers.map((layer: Seq[Char]) => {(layer, layer.count(_ == c))}).minBy(_._2)._1
  }

  def combineLayers(layers: Seq[Seq[Char]]) : Seq[Char] = {
    for (c <- layers.head.indices) yield {
      (for (l <- layers.indices) yield layers(l)(c)).filterNot(_ == '2').head
    }
  }

  def layerToLines(layer: Seq[Char], lineWidth: Int): Seq[String] = {
    val lineNum = layer.size / lineWidth
    for(i <- 0 until lineNum) yield layer.splitAt(i * lineWidth)._2.take(lineWidth).mkString
  }
}
