package codes.quine.labo.redos
package util

import java.io.PrintWriter

import scala.collection.mutable
import scala.sys.process.Process

object Graph {
  def from[V, L](edges: Seq[(V, L, V)]): Graph[V, L] =
    Graph(edges.groupMap(_._1)(vlv => (vlv._2, vlv._3)).withDefaultValue(Seq.empty))
}

final case class Graph[V, L] private (neighbors: Map[V, Seq[(L, V)]]) {
  def edges: Seq[(V, L, V)] =
    neighbors.flatMap { case v1 -> lvs => lvs.map { case (l, v2) => (v1, l, v2) } }.toSeq

  def vertices: Set[V] = neighbors.keySet | neighbors.values.flatMap(_.map(_._2)).toSet

  def render(name: String = "graph"): Unit = {
    val buf = new mutable.StringBuilder
    def quote(x: Any): String = "\"" + x.toString() + "\""
    buf ++= "digraph {\n"
    for ((v1, l, v2) <- edges) {
      buf ++= s"  ${quote(v1)} -> ${quote(v2)} [label=${quote(l)}];\n"
    }
    buf ++= "}\n"
    new PrintWriter(s"$name.dot") { write(buf.result()); close() }
    Process(Seq("dot", "-Tsvg", s"-o$name.svg", s"$name.dot")).!
  }

  def reverse: Graph[V, L] =
    Graph.from(edges.map { case (v1, l, v2) => (v2, l, v1) })

  def scc: Seq[Seq[V]] = {
    var clock = 0
    val visited = mutable.Map.empty[V, Int]
    val lowlinks = mutable.Map.empty[V, Int]

    val stack = mutable.Stack.empty[V]
    val inStack = mutable.Set.empty[V]

    val components = Seq.newBuilder[Seq[V]]

    def dfs(v1: V): Unit = {
      visited(v1) = clock
      lowlinks(v1) = clock
      clock += 1

      stack.push(v1)
      inStack.add(v1)

      for ((_, v2) <- neighbors.getOrElse(v1, Seq.empty)) {
        if (!visited.contains(v2)) {
          dfs(v2)
          lowlinks(v1) = Math.min(lowlinks(v1), lowlinks(v2))
        } else if (inStack.contains(v2)) {
          lowlinks(v1) = Math.min(lowlinks(v1), visited(v2))
        }
      }

      if (lowlinks(v1) == visited(v1)) {
        val component = Seq.newBuilder[V]
        var v2 = stack.pop()
        inStack.remove(v2)
        while (v1 != v2) {
          component.addOne(v2)
          v2 = stack.pop()
          inStack.remove(v2)
        }
        component.addOne(v1)
        components.addOne(component.result())
      }
    }

    for (v <- vertices) {
      if (!visited.contains(v)) {
        dfs(v)
      }
    }

    components.result()
  }

  def path(sources: Set[V], target: V): Option[Seq[L]] = {
    val queue = mutable.Queue.empty[(V, Seq[L])]
    val visited = mutable.Set.empty[V]

    queue.enqueueAll(sources.map((_, Seq.empty)))
    visited.addAll(sources)

    while (queue.nonEmpty) {
      val (v1, path) = queue.dequeue()
      if (v1 == target) {
        return Some(path)
      }
      for ((l, v2) <- neighbors(v1); if !visited.contains(v2)) {
        queue.enqueue((v2, path :+ l))
        visited.add(v2)
      }
    }

    return None
  }

  def reachable(init: Set[V]): Graph[V, L] = {
    val queue = mutable.Queue.empty[V]
    val reachable = mutable.Set.empty[V]
    val newEdges = Map.newBuilder[V, Seq[(L, V)]]

    queue.enqueueAll(init)
    reachable.addAll(init)

    while (queue.nonEmpty) {
      val v1 = queue.dequeue()
      val es = neighbors(v1)
      val vs = es.map(_._2)
      newEdges.addOne(v1 -> es)
      queue.enqueueAll(vs.filterNot(reachable.contains(_)))
      reachable.addAll(vs)
    }

    Graph(newEdges.result())
  }

  def reachableMap: Map[V, Set[V]] = {
    val map = mutable.Map.empty[V, Set[V]]
    def dfs(v1: V): Set[V] =
      map.getOrElseUpdate(v1, Set(v1) ++ neighbors(v1).flatMap { case (l, v) => dfs(v) }.toSet)
    vertices.foreach(dfs(_))
    map.toMap
  }
}
