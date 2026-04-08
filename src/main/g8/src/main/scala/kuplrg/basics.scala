package kuplrg

import scala.Console.*

/** The line separator */
val LINE_SEP = sys.props("line.separator")

// The type definitions of states and symbols
type State = Int

// The type definitions of states and symbols
type Symbol = Char

// The type definition of epsilon transition
type EPS = Unit
val EPS: EPS = ()

// The type definition of words
type Word = String

// A helper function to extract first symbol and rest of word
object `<|` { def unapply(w: Word) = w.headOption.map((_, w.drop(1))) }

/** The word acceptable type */
trait Acceptable {

  /** The set of symbols */
  def symbols: Set[Symbol]

  /** The acceptance of a word by the language
    *
    * @param w
    *   the word to be checked for acceptance
    * @return
    *   `true` if the word is accepted, `false` otherwise
    */
  def accept(w: Word): Boolean

  /** The language of the acceptable definition */
  def lang: Lang = Lang(symbols, accept)

  /** Checks if the language is equal to the given language
    *
    * @param expected
    *   the expected language
    * @param trial
    *   the number of random words to be checked
    */
  def mustEqual(expected: Acceptable, trial: Int): Unit =
    mustEqual(accept, expected, trial)

  def mustEqual(
    accept: Word => Boolean,
    expected: Acceptable,
    trial: Int,
  ): Unit =
    val list = expected.symbols.toList.sorted
    val m = list.length
    def check(w: Word): Unit =
      val result = accept(w)
      val answer = expected.accept(w)
      if (result != answer)
        val neg = if (answer) "" else " not"
        error(s"the word '$w' should$neg be in the language.")
    def aux(n: Int, k: Int, prevSize: Int): Unit =
      val curSize = (prevSize * m) min n
      for {
        i <- 0 until curSize
        (w, _) = (0 until k).foldLeft(("", i)) {
          case ((w, j), _) => (list(j % m).toString + w, j / list.length)
        }
      } check(w)
      if (curSize < n) aux(n - curSize, k + 1, curSize)
    m match
      case 0 => check("")
      case 1 =>
        val w = symbols.head.toString
        (0 until trial).map(i => check(w * i))
      case _ => aux(trial, 0, 1)
}

/** The languages */
case class Lang(
  symbols: Set[Symbol],
  contains: Word => Boolean,
) extends Acceptable {

  /** The acceptance of a word by the regular expression.
    *
    * @param word
    *   the word to be checked for acceptance
    * @return
    *   `true` if the word is accepted, `false` otherwise
    */
  def accept(w: Word): Boolean = contains(w)
}

// -----------------------------------------------------------------------------
// Helper Functions
// -----------------------------------------------------------------------------
/** A helper function to print a message in green */
def show(msg: String): Unit = println(green(msg))

/** A helper function to color a string in green */
def green(str: String): String = s"$GREEN$str$RESET"

/** A helper function to compare the fixed point */
def fixpoint[A](init: Set[A], f: (Set[A], A) => Set[A]): Set[A] =
  def aux(
    visited: Set[A],
    worklist: List[A],
  ): Set[A] = worklist match
    case Nil => visited
    case x :: xs =>
      val set = f(visited, x) -- visited
      aux(visited ++ set, set.toList ++ xs)
  aux(init, init.toList)

/** binary number to decimal integer */
def toInt(binary: String): Int =
  if (binary == "") 0
  else Integer.parseInt(binary, 2)

/** binary number to decimal big integer */
def toBigInt(binary: String): BigInt =
  if (binary == "") BigInt(0)
  else BigInt(binary, 2)
