package kuplrg

import scala.util.parsing.combinator.*

/** The type definition of nonterminal symbol */
type Nt = String

/** The definition of right-hand side of a production rule
  *
  * @constructor
  *   create a new right-hand side of a production rule
  *
  * @param seq
  *   the sequence of nonterminal and terminal symbols
  */
case class Rhs(seq: List[Nt | Symbol])

/** The definition of context-free grammar (CFG)
  *
  * @constructor
  *   create a new context-free grammar (CFG).
  *
  * @param nts
  *   the set of nonterminal symbols
  * @param symbols
  *   the set of terminal symbols
  * @param start
  *   the start nonterminal symbol
  * @param rules
  *   the production rules
  */
final case class CFG(
  nts: Set[Nt],
  symbols: Set[Symbol],
  start: Nt,
  rules: Map[Nt, List[Rhs]],
) extends Acceptable:

  /** The acceptance of a word by the context-free grammar
    *
    * @param word
    *   the word to be checked for acceptance
    * @return
    *   `true` if the word is accepted, `false` otherwise
    */
  def accept(word: Word): Boolean = parse(word) != ParseResult.Failure

  /** Parse a word using the context-free grammar */
  def parse(word: Word, debug: Boolean = false): ParseResult[ParseTree] =
    Parser(this, word, debug)

/** A parser for context-free grammars */
object CFG extends RegexParsers with PackratParsers:

  /** Create a new context-free grammar from a list of production rules
    *
    * @param rules
    *   the list of production rules
    * @return
    *   a new context-free grammar
    */
  def apply(rules: List[(Nt, List[Rhs])]): CFG =
    val ntList = rules.map { case (nt, _) => nt }
    val symbols = (for {
      (_, rhsList) <- rules
      rhs <- rhsList
      x <- rhs.seq
      s <- x match
        case s: Symbol => Some(s)
        case _         => None
    } yield s).toSet
    val start = ntList.head
    val rulesMap = rules
      .groupMap(_._1)(_._2)
      .map { case (x, y) => x -> y.flatten }
    CFG(ntList.toSet, symbols, start, rulesMap)

  /** Parse a context-free grammar from a string
    *
    * @param str
    *   the string representation of the context-free grammar
    * @return
    *   a new context-free grammar
    */
  def apply(str: String): CFG = parseAll(cfg, str).get
  lazy val symbol: Parser[Symbol] = "'[-+*{}()0-9a-z]'".r ^^ { _.charAt(1) }
  lazy val nt: Parser[Nt] = "[A-Z]+".r
  lazy val rhs: Parser[Rhs] =
    "<e>" ^^^ { Rhs(List()) } |
    rep1sep(symbol | nt, "") ^^ { Rhs(_) }
  lazy val rule: Parser[(Nt, List[Rhs])] =
    nt ~ ("->" ~> rep1sep(rhs, "|")) ^^ { case nt ~ rhsList => nt -> rhsList }
  lazy val sep: Parser[String] = ";"
  lazy val cfg: Parser[CFG] = rep1sep(rule, sep) <~ opt(sep) ^^ { rules =>
    CFG(rules)
  }
