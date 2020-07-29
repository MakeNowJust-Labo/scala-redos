package codes.quine.labo.redos
package util

import minitest.SimpleTestSuite

import guru.nidi.graphviz.engine.Format

object GraphRendererSuite extends SimpleTestSuite {
  test("GraphRenderer.graphviz (with GraphRenderer.InstanceForGraph)") {
    val g = Graph.from(Seq((0, Option('a'), 1), (0, Option('b'), 2), (1, None, 2)))
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
    val g = Graph.from(Seq((0, Option('a'), 1), (0, Option('b'), 2), (1, None, 2)))
    val graphviz = GraphRenderer.graphviz(g)(new GraphRenderer[Graph[Int, Option[Char]]] {
      type Vertex = Int
      type EdgeLabel = Option[Char]
      def graph(g: Graph[Int, Option[Char]]): Graph[Int, Option[Char]] = g
    })
    val dot = graphviz.render(Format.DOT).toString()

    assert(!dot.contains(""""rankdir"="LR""""))
    assert(dot.contains(""""0" -> "1""""))
    assert(dot.contains(""""0" -> "2""""))
    assert(dot.contains(""""1" -> "2""""))
  }
}
