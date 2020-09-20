package codes.quine.labo.redos
package util

import guru.nidi.graphviz.engine.Format
import minitest.SimpleTestSuite

object GraphRendererSuite extends SimpleTestSuite {
  test("GraphRenderer.graphviz (with GraphRenderer.InstanceForGraph)") {
    val g = Graph.from(Seq((0, Option('a'.toInt), 1), (0, Option('b'.toInt), 2), (1, None, 2)))
    val graphviz = GraphRenderer.graphviz(g)
    val dot = graphviz.render(Format.DOT).toString()

    assert(dot.contains(""""rankdir"="LR""""))
    assert(dot.contains(""""0" -> "1""""))
    assert(dot.contains(""""label"="a""""))
    assert(dot.contains(""""0" -> "2""""))
    assert(dot.contains(""""label"="b""""))
    assert(dot.contains(""""1" -> "2""""))
    assert(dot.contains(""""label"=<<U>?</U>>"""))
  }

  test("GraphRenderer.graphviz (without GraphRenderer.InstanceForGraph)") {
    val g = Graph.from(Seq((0, Option('a'.toInt), 1), (0, Option('b'.toInt), 2), (1, None, 2)))
    val graphviz = GraphRenderer.graphviz(g)(new GraphRenderer[Graph[Int, Option[Int]]] {
      type Vertex = Int
      type EdgeLabel = Option[Int]
      def graph(g: Graph[Int, Option[Int]]): Graph[Int, Option[Int]] = g
    })
    val dot = graphviz.render(Format.DOT).toString()

    assert(!dot.contains(""""rankdir"="LR""""))
    assert(dot.contains(""""0" -> "1""""))
    assert(dot.contains(""""0" -> "2""""))
    assert(dot.contains(""""1" -> "2""""))
  }
}
