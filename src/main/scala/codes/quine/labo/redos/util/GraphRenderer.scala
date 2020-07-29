package codes.quine.labo.redos
package util

import scala.util.chaining._

import guru.nidi.graphviz.attribute.{Attributed, ForGraph, ForLink, ForNode, Label, Shape, Rank, Attributes}
import guru.nidi.graphviz.engine.Graphviz
import guru.nidi.graphviz.model
import guru.nidi.graphviz.model.{Link, Node, Factory}
import guru.nidi.graphviz.engine.Format

/**
  * GraphRenderer is a type class for rendering graph-like data structure by Graphviz.
  *
  * @tparam T data type
  */
trait GraphRenderer[T] {

  /** A type of vertex. */
  type Vertex

  /** A type of edge label. */
  type EdgeLabel

  /**
    * Convert this data to graph for rendering.
    *
    * @param t a data
    * @return a graph corresponding to the data
    */
  def graph(t: T): Graph[Vertex, EdgeLabel]

  /**
    * Apply graph attributes into Graphviz data.
    *
    * @param t a data
    * @param attr attributes data for Graphviz graph
    * @return GraphViz data applied attributes
    */
  def globalGraphAttributes(t: T, attr: Attributed[model.Graph, ForGraph]): model.Graph = attr.`with`()

  /**
    * Apply vertex (node) attributes into Graphviz data.
    *
    * @param t a data
    * @param attr attributes data for Graphviz node
    * @return GraphViz data applied attributes
    */
  def globalVertexAttributes(t: T, attr: Attributed[model.Graph, ForNode]): model.Graph = attr.`with`()

  /**
    * Apply vertex (node) attributes into Graphviz data.
    *
    * @param t a data
    * @param attr attributes data for Graphviz node
    * @return GraphViz data applied attributes
    */
  def globalEdgeAttributes(t: T, attr: Attributed[model.Graph, ForLink]): model.Graph = attr.`with`()

  /**
    * Get a label of each vertices.
    *
    * @param t a data
    * @param v a vertex
    * @return a Graphviz label
    */
  def vertexLabel(t: T, v: Vertex): Label = Label.of(v.toString)

  /**
    * Apply vertex (node) attributes of each vertices into Graphviz data.
    *
    * @param t a data
    * @param v a vertex
    * @param node a Graphviz node
    * @return a Graphviz node applied attributes
    */
  def vertexAttributes(t: T, v: Vertex, node: Node): Node = node

  /**
    * Apply edge (link) attributes of each vertices into Graphviz data.
    *
    * @param t a data
    * @param v1 a source vertex
    * @param l an edge label
    * @param v2 a target vertex
    * @param link a Graphviz link
    * @return a Graphviz link applied attributes
    */
  def edgeAttributes(t: T, v1: Vertex, l: EdgeLabel, v2: Vertex, link: Link): Link = link
}

object GraphRenderer {
  type Aux[T, V, L] = GraphRenderer[T] {
    type Vertex = V
    type EdgeLabel = L
  }

  /** Summon [[GraphRenderer]] instance. */
  def apply[T](implicit T: GraphRenderer[T]): Aux[T, T.Vertex, T.EdgeLabel] = T

  /**
    * Build Graphviz instance for rendering.
    *
    * @param t a rendered data
    * @return Graphviz instance for rendering
    */
  def graphviz[T](t: T)(implicit T: GraphRenderer[T]): Graphviz = {
    val graphviz = Factory
      .graph()
      .directed()
      .graphAttr()
      .pipe(T.globalGraphAttributes(t, _))
      .linkAttr()
      .pipe(T.globalEdgeAttributes(t, _))
      .nodeAttr()
      .pipe(T.globalVertexAttributes(t, _))
      .toMutable()

    val graph = T.graph(t)
    for (v <- graph.vertices) {
      graphviz.add(Factory.node(T.vertexLabel(t, v)).pipe(T.vertexAttributes(t, v, _)))
    }
    for (((v1, l, v2), i) <- graph.edges.zipWithIndex) {
      val source = Factory.node(T.vertexLabel(t, v1))
      val target = Factory.node(T.vertexLabel(t, v2))
      // An `id` attribute for each edges is needed to distinct edges having the same attributes.
      val to = Factory.to(target).`with`(Attributes.attr("id", f"edge$i%03d")).pipe(T.edgeAttributes(t, v1, l, v2, _))
      graphviz.add(source.link(to))
    }

    Graphviz.fromGraph(graphviz)
  }

  /**
    * AlphabetLabel is a type class of alphabet for rendering.
    *
    * @tparam A alphabet type
    */
  trait AlphabetLabel[A] {

    /**
      * Convert an alphabet character into a Graphviz label.
      *
      * @param A an alphabet character
      * @return a GraphViz label
      */
    def label(a: A): Label
  }

  object AlphabetLabel {

    /** Summon [[AlphabetLabel]] instance. */
    def apply[A](implicit A: AlphabetLabel[A]): AlphabetLabel[A] = A

    /**
      * Build Graphviz label from alphabet character.
      *
      * @param a an alphabet character
      * @return a GraphViz label
      */
    def of[A: AlphabetLabel](a: A): Label = AlphabetLabel[A].label(a)

    /**
      * Create an [[AlphabetLabel]] instance from function.
      *
      * @param f a label function
      * @return an AlphabetLabel instance
      */
    def from[A](f: A => Label): AlphabetLabel[A] =
      new AlphabetLabel[A] {
        def label(t: A): Label = f(t)
      }

    implicit def InstanceForOptionChar: AlphabetLabel[Option[Char]] =
      from {
        case Some(c) => Label.of(String.valueOf(c))
        case None    => Label.html("""<U>?</U>""")
      }
  }

  /**
    * StateLabel is a type class of state for rendering.
    *
    * @tparam Q state type
    */
  trait StateLabel[Q] {

    /**
      * Convert a state into a label text.
      *
      * @param q a state
      * @return a label text
      */
    def label(q: Q): String
  }

  object StateLabel {

    /** Summon [[StateLabel]] instance. */
    def apply[Q](implicit Q: StateLabel[Q]): StateLabel[Q] = Q

    /**
      * Build Graphviz label from state.
      *
      * @param q a state
      * @return a GraphViz label
      */
    def of[Q: StateLabel](q: Q): Label = Label.of(StateLabel[Q].label(q))

    /**
      * Create a [[StateLabel]] instance from function.
      *
      * @param f a label function
      * @return a StateLabel instance
      */
    def from[Q](f: Q => String): StateLabel[Q] =
      new StateLabel[Q] {
        def label(q: Q): String = f(q)
      }

    implicit def InstanceForInt: StateLabel[Int] = from(_.toString)
    implicit def InstanceForSeq[Q: StateLabel]: StateLabel[Seq[Q]] =
      from(_.map(StateLabel[Q].label(_)).mkString("[", ",", "]"))
    implicit def InstanceForSet[Q: StateLabel: Ordering]: StateLabel[Set[Q]] =
      from(_.toSeq.sorted.map(StateLabel[Q].label(_)).mkString("{", ",", "}"))
    implicit def InstanceForPair[Q1: StateLabel, Q2: StateLabel]: StateLabel[(Q1, Q2)] =
      from { case (q1, q2) => s"(${StateLabel[Q1].label(q1)}, ${StateLabel[Q2].label(q2)})" }
  }

  /**
    * Create a [[GraphRenderer]] instance for finite automata.
    *
    * @param toGraph a graph converter function
    * @param isInit a prediction to decide initial state
    * @param isAccept a prediction to decide accept state
    * @return a GraphRenderer instance
    */
  def automaton[FA[_, _], A: AlphabetLabel, Q: StateLabel](
      toGraph: FA[A, Q] => Graph[Q, A],
      isInit: (FA[A, Q], Q) => Boolean,
      isAccept: (FA[A, Q], Q) => Boolean
  ): Aux[FA[A, Q], Option[Q], Option[A]] =
    new GraphRenderer[FA[A, Q]] {
      type Vertex = Option[Q]
      type EdgeLabel = Option[A]
      def graph(fa: FA[A, Q]): Graph[Option[Q], Option[A]] = {
        val g = toGraph(fa)
        val edges1 = g.edges.map { case (q1, a, q2) => (Some(q1), Some(a), Some(q2)) }
        val edges2 = g.vertices.filter(isInit(fa, _)).map(q0 => (None, None, Some(q0)))
        Graph.from(edges1 ++ edges2)
      }
      override def globalGraphAttributes(t: FA[A, Q], attr: Attributed[model.Graph, ForGraph]): model.Graph =
        attr.`with`(Rank.dir(Rank.RankDir.LEFT_TO_RIGHT))
      override def vertexLabel(fa: FA[A, Q], q: Option[Q]): Label =
        q.map(StateLabel.of(_)).getOrElse(Label.of(""))
      override def vertexAttributes(fa: FA[A, Q], q: Option[Q], node: Node): Node =
        q match {
          case Some(q) if isAccept(fa, q) => node.`with`(Shape.DOUBLE_CIRCLE)
          case Some(q)                    => node.`with`(Shape.CIRCLE)
          case None                       => node.`with`(Shape.POINT)
        }
      override def edgeAttributes(fa: FA[A, Q], q1: Option[Q], a: Option[A], q2: Option[Q], link: Link): Link =
        a match {
          case Some(a) => link.`with`(AlphabetLabel.of(a))
          case None    => link
        }
    }

  implicit def InstanceForGraph[V: StateLabel, L: AlphabetLabel]: Aux[Graph[V, L], V, L] =
    new GraphRenderer[Graph[V, L]] {
      type Vertex = V
      type EdgeLabel = L
      def graph(g: Graph[V, L]): Graph[V, L] = g
      override def globalGraphAttributes(t: Graph[V, L], attr: Attributed[model.Graph, ForGraph]): model.Graph =
        attr.`with`(Rank.dir(Rank.RankDir.LEFT_TO_RIGHT))
      override def vertexLabel(t: Graph[V, L], v: V): Label = StateLabel.of(v)
      override def edgeAttributes(t: Graph[V, L], v1: V, l: L, v2: V, link: Link): Link =
        link.`with`(AlphabetLabel.of(l))
    }
}
