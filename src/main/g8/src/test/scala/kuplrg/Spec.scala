package kuplrg

import Implementation.*

class Spec extends SpecBase {

  // Number of trials for `mustEqual`
  private val TRIAL = 512

  // ---------------------------------------------------------------------------
  // Context Free Grammar (CFG)
  // ---------------------------------------------------------------------------
  // L = { w \in { (, ) }* | w is a balanced parentheses string }
  {
    val lang: Lang = Lang(
      "()".toSet,
      w =>
        w.count(_ == '(') == w.count(_ == ')') && w
          .scanLeft(0) {
            case (d, c) => if (c == '(') d + 1 else d - 1
          }
          .tail
          .forall { _ >= 0 },
    )
    check(cfg_balanced.mustEqual(lang, TRIAL))
  }

  // L = { a^n b^2n | n >= 0 }
  {
    val lang: Lang = Lang(
      "ab".toSet,
      w => "a*b*".r.matches(w) && 2 * w.count(_ == 'a') == w.count(_ == 'b'),
    )
    check(cfg_an_b2n.mustEqual(lang, TRIAL))
  }

  // L = { a^n b^m | n <= m <= 2n }
  {
    val lang: Lang = Lang(
      "ab".toSet,
      w =>
        val (a, b) = (w.count(_ == 'a'), w.count(_ == 'b'))
        "a*b*".r.matches(w) && a <= b && b <= 2 * a,
    )
    check(cfg_an_bm_between.mustEqual(lang, TRIAL))
  }

  // L = { a^n b^m | n != m }
  {
    val lang: Lang = Lang(
      "ab".toSet,
      w => "a*b*".r.matches(w) && w.count(_ == 'a') != w.count(_ == 'b'),
    )
    check(cfg_an_bm_neq.mustEqual(lang, TRIAL))
  }

  // L = { a^i b^j a^k | i + k = j }
  {
    val lang: Lang = Lang(
      "ab".toSet,
      w => "a*b*a*".r.matches(w) && w.count(_ == 'a') == w.count(_ == 'b'),
    )
    check(cfg_ai_bj_ak.mustEqual(lang, TRIAL))
  }

  // L = { w \in {a, b}* | N_a(w) <= N_b(w) }
  {
    val lang: Lang = Lang(
      "ab".toSet,
      w => w.count(_ == 'a') <= w.count(_ == 'b'),
    )
    check(cfg_more_bs.mustEqual(lang, TRIAL))
  }

  // L = { w \in {a, b}* | all non-empty prefix u of w satisfies
  // N_a(u) != N_b(u) }
  {
    val lang: Lang = Lang(
      "ab".toSet,
      w =>
        w.isEmpty || w
          .scanLeft(0) {
            case (d, c) =>
              if (c == 'a') d + 1 else d - 1
          }
          .tail
          .forall { case d => d != 0 },
    )
    check(cfg_prefix_neq.mustEqual(lang, TRIAL))
  }

  // L = { w \in {a, b}* | N_a(w) <= N_b(w) <= 2N_a(w) }
  {
    val lang: Lang = Lang(
      "ab".toSet,
      w =>
        val (a, b) = (w.count(_ == 'a'), w.count(_ == 'b'))
        a <= b && b <= 2 * a,
    )
    check(cfg_between.mustEqual(lang, TRIAL))
  }

  // L = { w \in {a, b}* | w != w^R }
  {
    val lang: Lang = Lang(
      "ab".toSet,
      w => w != w.reverse,
    )
    check(cfg_neq_reverse.mustEqual(lang, TRIAL))
  }

  // L = { w w | w \in {a, b}* }^c
  {
    val lang: Lang = Lang(
      "ab".toSet,
      w => w.take(w.length / 2) != w.drop(w.length / 2),
    )
    check(cfg_not_ww.mustEqual(lang, TRIAL))
  }
}
