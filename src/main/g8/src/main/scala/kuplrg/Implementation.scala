package kuplrg

object Implementation extends Template {

  /** This is the playground for you to run your implementation. Do whatever you
    * want here and run `sbt run` to see the result.
    */
  @main def playground: Unit = {
    println("------------------- PLAYGROUND -------------------")

    // You can check your implementation here.
    println(s"cfg_an_bn.accept(\"ab\")   = ${cfg_an_bn.accept("ab")}")
    println(s"cfg_an_bn.accept(\"aba\")  = ${cfg_an_bn.accept("aba")}")
    println(s"cfg_an_bn.accept(\"aabb\") = ${cfg_an_bn.accept("aabb")}")

    println("--------------------------------------------------")
  }

  // CFG accepting L = { a^n b^n | n >= 0 }
  def cfg_an_bn: CFG = CFG("""
    S -> A | <e> ;
    A -> 'a' S 'b' ;
  """)

  // CFG accepting L = { w \in { (, ) }* | w is a balanced parentheses string }
  def cfg_balanced: CFG = ???

  // CFG accepting L = { a^n b^2n | n >= 0 }
  def cfg_an_b2n: CFG = ???

  // CFG accepting L = { a^n b^m | n <= m <= 2n }
  def cfg_an_bm_between: CFG = ???

  // CFG accepting L = { a^n b^m | n != m }
  def cfg_an_bm_neq: CFG = ???

  // CFG accepting L = { a^i b^j a^k | i + k = j }
  def cfg_ai_bj_ak: CFG = ???

  // CFG accepting L = { w \in {a, b}* | N_a(w) <= N_b(w) }
  def cfg_more_bs: CFG = ???

  // CFG accepting L = { w \in {a, b}* | all non-empty prefix u of w satisfies
  // N_a(u) != N_b(u) }
  def cfg_prefix_neq: CFG = ???

  // CFG accepting L = { w \in {a, b}* | N_a(w) <= N_b(w) <= 2N_a(w) }
  def cfg_between: CFG = ???

  // CFG accepting L = { w \in {a, b}* | w != w^R }
  def cfg_neq_reverse: CFG = ???

  // CFG accepting L = { w w | w \in {a, b}* }^c
  def cfg_not_ww: CFG = ???
}
