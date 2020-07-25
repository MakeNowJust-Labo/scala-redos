package codes.quine.labo.redos
package util

import scala.math.Ordering.Implicits._

import minitest.SimpleTestSuite

object GraphSuite extends SimpleTestSuite {
  test("Graph.from") {
    val g = Graph.from(Seq((1, (), 2), (1, (), 3), (2, (), 3), (3, (), 2)))
    assertEquals(g.neighbors, Map(1 -> Seq(((), 2), ((), 3)), 2 -> Seq(((), 3)), 3 -> Seq(((), 2))))
  }

  test("Graph#edges") {
    val es = Seq((1, (), 2), (1, (), 3), (2, (), 3), (3, (), 2))
    val g = Graph.from(es)
    assertEquals(g.edges, es)
  }

  test("Graph#vertices") {
    val g = Graph.from(Seq((1, (), 2), (1, (), 3), (2, (), 3), (3, (), 4)))
    assertEquals(g.vertices, Set(1, 2, 3, 4))
  }

  test("Graph#reverse") {
    val es = Seq((1, (), 2), (1, (), 3), (2, (), 3), (3, (), 2))
    val g = Graph.from(es)
    assertEquals(g.reverse.edges.sorted, es.map { case (q1, l, q2) => (q2, l, q1) }.sorted)
  }

  test("Graph#scc") {
    val g = Graph.from(Seq((1, (), 2), (2, (), 3), (3, (), 1), (1, (), 4)))
    assertEquals(g.scc.map(_.sorted).sorted, Seq(Seq(1, 2, 3), Seq(4)))
  }

  test("Graph#path") {
    val g = Graph.from(Seq((1, 'a', 2), (2, 'b', 3), (3, 'c', 1), (1, 'd', 4)))
    assertEquals(g.path(Set(1), 2), Some(Seq('a')))
    assertEquals(g.path(Set(1), 3), Some(Seq('a', 'b')))
    assertEquals(g.path(Set(1), 5), None)
  }

  test("Graph#reachable") {
    val g = Graph.from(Seq((1, (), 2), (2, (), 1), (3, (), 3)))
    assertEquals(g.reachable(Set(1)), Graph.from(Seq((1, (), 2), (2, (), 1))))
  }

  test("Graph#reachableMap") {
    val g = Graph.from(Seq((1, (), 2), (1, (), 3), (2, (), 4), (3, (), 4)))
    assertEquals(g.reachableMap, Map(1 -> Set(1, 2, 3, 4), 2 -> Set(2, 4), 3 -> Set(3, 4), 4 -> Set(4)))
  }
}
