package kuplrg

import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap}

/** The parser of a context-free grammar */
final private class Parser private (cfg: CFG, input: String, debug: Boolean) {
  enum Point:
    case Entry(i: Int, nt: Nt)
    case Inner(i: Int, nt: Nt, j: Int, k: Int)
    case Exit(i: Int, nt: Nt)

  import Point.*, ParseTree.*, ParseTreePtr.*, ParseResult.*
  val CFG(nts, symbols, start, origRules) = cfg
  val rules = nts.map(x => x -> origRules(x).map(_.seq.toArray).toArray).toMap
  val init = Entry(0, start)
  val result = MMap(init -> Set((0, Vector[ParseTreePtr]())))
  val back = MMap[Exit, Set[Inner]]()
  class Worklist[T](init: Iterable[T] = Nil) {
    var list = init.toList
    var set = Set[T]()
    def +=(t: T): Unit =
      if (!set(t))
        list ::= t
        set += t
    def get: Option[T] = list match
      case Nil => None
      case t :: rest =>
        list = rest
        set -= t
        Some(t)
  }
  val worklist = Worklist[Point](List(init))

  def getResult(point: Point): Set[(Int, Vector[ParseTreePtr])] =
    result.getOrElseUpdate(point, Set())
  def putResult(point: Point, newSet: Set[(Int, Vector[ParseTreePtr])]): Unit =
    val oldSet = getResult(point)
    if (!newSet.subsetOf(oldSet))
      result(point) = oldSet ++ newSet
      worklist += point

  def getBack(exit: Exit): Set[Inner] =
    back.getOrElseUpdate(exit, Set())
  def putBack(exit: Exit, inner: Inner): Unit =
    back(exit) = getBack(exit) + inner
    worklist += exit

  def showLog =
    println("- result:")
    for {
      (key, set) <- result.toList.sortBy(_._1.toString)
    } {
      println(s"  $key ->")
      for {
        (to, tree) <- set.toList.sortBy((x, y) => (x, y.toString))
      } println(s"    $to: ${tree.mkString("[", ", ", "]")}")
    }
    println("- back:")
    for {
      (key, set) <- back.toList.sortBy(_._1.toString)
    } {
      println(s"  $key ->")
      for {
        to <- set.toList.sortBy(_.toString)
      } println(s"    $to")
    }

  @tailrec
  def parse: ParseResult[ParseTree] =
    worklist.get match
      case None =>
        if (debug) showLog
        toResult(NodePtr(start, 0, input.length))
      case Some(work) =>
        work match
          case Entry(i, nt) =>
            val set = getResult(work)
            for (j <- 0 until rules(nt).length)
              putResult(Inner(i, nt, j, 0), set)
          case Inner(i, nt, j, k) =>
            val rhs = rules(nt)(j)
            if (k < rhs.length) rhs(k) match
              case a: Symbol =>
                for {
                  (to, ps) <- getResult(work)
                  if to < input.length && a == input(to)
                } putResult(
                  Inner(i, nt, j, k + 1),
                  Set((to + 1, ps :+ LeafPtr(a))),
                )
              case targetNt: Nt =>
                for ((to, ps) <- getResult(work))
                  putResult(Entry(to, targetNt), Set((to, Vector())))
                  putBack(Exit(to, targetNt), Inner(i, nt, j, k + 1))
            else putResult(Exit(i, nt), getResult(work))
          case exit @ Exit(i, nt) =>
            for {
              next <- getBack(exit)
              prev = Inner(next.i, next.nt, next.j, next.k - 1)
              (to, rs) <- getResult(prev)
              if to == i
            } putResult(
              next,
              getResult(work).map {
                case (to, _) => (to, rs :+ NodePtr(nt, i, to))
              },
            )
        parse

  def toResult(ptr: ParseTreePtr): ParseResult[ParseTree] = ptr match
    case NodePtr(nt, from, until) =>
      val res = ParseResult(for {
        (to, ps) <- getResult(Exit(from, nt))
        if to == until
      } yield ps)
      for {
        ps <- res
        tree <- ps
          .map(toResult)
          .foldLeft(Success(Vector[ParseTree]())) {
            case (vs, trees) =>
              for {
                v <- vs
                t <- trees
              } yield v :+ t
          }
          .map(Node(nt, _))
      } yield tree
    case LeafPtr(symbol) => Success(Leaf(symbol))
}

/** The parser of a context-free grammar */
object Parser {
  def apply(cfg: CFG, input: String, debug: Boolean): ParseResult[ParseTree] =
    new Parser(cfg, input, debug).parse
}

/** Parse tree of a context-free grammar */
enum ParseTree:
  case Node(nt: Nt, children: Vector[ParseTree])
  case Leaf(symbol: Symbol)
  def +(child: ParseTree): ParseTree = this match
    case Node(nt, children) => Node(nt, children :+ child)
    case Leaf(symbol)       => ???
  override def toString: String = this match
    case Node(nt, children) => s"[$nt ${children.mkString(" ")}]"
    case Leaf(symbol)       => s"'$symbol'"

enum ParseTreePtr:
  case NodePtr(nt: Nt, from: Int, until: Int)
  case LeafPtr(symbol: Symbol)

enum ParseResult[+T]:
  case Ambiguous extends ParseResult[Nothing]
  case Success(data: T) extends ParseResult[T]
  case Failure extends ParseResult[Nothing]

  def map[U](f: T => U): ParseResult[U] = this match
    case Ambiguous     => Ambiguous
    case Success(data) => Success(f(data))
    case Failure       => Failure

  def flatMap[U](f: T => ParseResult[U]): ParseResult[U] = this match
    case Ambiguous     => Ambiguous
    case Success(data) => f(data)
    case Failure       => Failure

  def isSuccessful: Boolean = this match
    case Success(_) => true
    case _          => false

  def isAmbiguous: Boolean = this == Ambiguous

  def isFailure: Boolean = this == Failure

object ParseResult:
  def apply[T](seq: Iterable[T]): ParseResult[T] = seq.size match
    case 0 => Failure
    case 1 => Success(seq.head)
    case _ => Ambiguous
