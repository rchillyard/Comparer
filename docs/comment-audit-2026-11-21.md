# Comment audit: untagged comments, and tags that understate what they say

Run 2026-11-21 over all twelve libraries, after the comment sweep of 2026-11-20 had given every
*tagged* comment an issue. Two questions were asked:

1. which comments carry **no tag** and should at least have `ASIDE`;
2. which `NOTE` or `CONSIDER` comments are really `TODO` or `FIXME`.

The second question is where the value is, and the rest of this document says why.

**Nothing here is filed as an issue.** It is a list to read and decide on.

---

## 1. Untagged comments

### The scale, and why blanket tagging is the wrong goal

| | count |
|---|---|
| Untagged Scaladoc blocks (`/** … */`) | **7,413** |
| Untagged line comments (`//`) | **3,351** |
| Untagged non-Scaladoc block comments | 4 |

The Scaladoc is API documentation. Tagging 7,413 doc comments `ASIDE` would say nothing and cost
a day; they are excluded from everything below.

Of the 3,351 line comments, a third are not prose at all:

| kind | count | example |
|---|---|---|
| prose | **1,279** | `// Direct edge 0->2 costs 4.0; via 1 costs 1.0+2.0=3.0` |
| separator or heading | 504 | `// ------------------------ Graph mutation ------------------------` |
| short label | 458 | `// t: T` |
| diagram or data | 380 | `// X.X  XX.  .XX` (a Gambit board), MST edge tables |
| BDD label | 187 | `// given`, `// when`, `// then` — nearly all Majabigwaduce |
| tool directive | 53 | `// noinspection unchecked`, `// scalastyle:ignore var.field` |
| commented-out code | see below | |
| empty | 1 | |

**1,124 of them — separators, diagrams, BDD labels and tool directives — should not be tagged at
all.** `// noinspection unchecked` is an instruction to IntelliJ. `// given` is test structure.
A row of dashes is a heading. An `ASIDE` on any of those adds a word and no information, which is
the thing the comment standard exists to prevent.

### Per project

| project | prose | separator | short label | diagram | BDD | directive |
|---|---|---|---|---|---|---|
| Number | 516 | 145 | 319 | 311 | 0 | 5 |
| Gambit | 187 | 89 | 27 | 17 | 0 | 0 |
| Gryphon | 135 | 205 | 24 | 15 | 0 | 0 |
| TableParser | 134 | 0 | 17 | 25 | 0 | 13 |
| XmlDoc | 124 | 3 | 24 | 3 | 0 | 0 |
| LaScala | 60 | 0 | 32 | 5 | 0 | 17 |
| Visitor | 55 | 62 | 8 | 3 | 0 | 0 |
| Majabigwaduce | 29 | 0 | 3 | 0 | 187 | 13 |
| Flog | 16 | 0 | 2 | 0 | 0 | 3 |
| Args | 9 | 0 | 0 | 0 | 0 | 2 |
| Comparer | 8 | 0 | 0 | 0 | 0 | 0 |
| Matchers | 6 | 0 | 2 | 1 | 0 | 0 |

### Commented-out code: not reliably countable

Three successive heuristics were tried and all three were wrong, in opposite directions. The
final one scored about three in ten on a hand-checked sample, and the reason is particular to
these libraries: **in a mathematical codebase, prose ends in `)` as often as code does.**

```
// x^1 = x (identity)
// exp(a + b·i)  →  Euler(exp(a), b)  (commuted inner)
// just i should be e^(i) = cos(1) + i*sin(1)
```

Every one of those trips a "looks like code" rule, and none is code. A count is not offered,
because any number produced this way would be wrong by an unknown margin in an unknown direction.
What can be said: the candidates are concentrated in Number, and a human reading
`git grep -n "^\s*// *\(val\|def\|it should\|case \)"` per project will find the real ones faster
than a classifier will.

### Recommendation

**Do not retrofit `ASIDE` to the 1,279 prose comments.** The cost is a day of mechanical edits
across twelve repositories and about 1,300 diff lines, for an outcome where every surviving
comment carries a tag that, by definition, says "this comment may not be worth much".

The tag earns its place **going forward**: when a comment is written, and when a file is open for
another reason. That is already how `XXX` → `ASIDE` is being handled, and it costs nothing extra.

The one thing worth doing in bulk is the opposite of tagging: the 1,124 structural comments
confirm that an untagged comment is often correct, so a sweep that treats "untagged" as a defect
would generate more noise than signal.

---

## 2. Tags that understate what they say

**This is the actionable half.** 97 comments tagged `NOTE`, `CONSIDER` or `ASIDE` contain language
describing something wrong — *fails*, *doesn't work*, *broken*, *flaky*, *re-run*, *for now*,
*wrong*, *workaround*. Each was read; the false positives are called out below rather than
silently dropped.

This matters because of how Number #601 escaped the sweep. `RationalPropertySpec` carried, since
2016:

> NOTE: it is possible that these tests will fail due to randomly generating a Double precision
> number out of range of Double (E311). Recommend just re-run.

A defect report, tagged `NOTE` — the one tag the sweep trusts to mean "documentation, nothing to
do". 959 comments were read in the sweep and that one was skipped on the strength of its first
word. Everything in this section is the same shape.

Ordered by library urgency, per `triage-2026-11-20.md`.

### XmlDoc

| file:line                | tag      | text                                                                                        | verdict                                        | Robin's update                  |
|--------------------------|----------|---------------------------------------------------------------------------------------------|------------------------------------------------|---------------------------------|
| `NamedFunction.scala:57` | NOTE     | *"for now we do nothing to actually evaluate the implicit evidence of NamedFunction[N]"*    | **→ TODO.** "For now" is a promise to someone. | retag as CONSIDER, create issue | 
| `Extractor.scala:505`    | NOTE     | *"an empty element has no children at all — that's just empty text content, not a failure"* | correct as `NOTE`                              |
| `KML.scala:969`          | NOTE     | explains why `sequenceRenderer` is not used here                                            | correct as `NOTE`                              |
| `Renderer.scala:239`     | ASIDE    | *"if maybeAttribute is defined, then isInternal will usually be true"*                      | correct                                        |
| `Renderer.scala:381`     | CONSIDER | already cites #133                                                                          | tracked                                        |

### TableParser

| file:line | tag | text | verdict | Robin's update                                                                            |
|---|---|---|---|-------------------------------------------------------------------------------------------|
| `zio/.../TableSpec.scala:73` | NOTE | ***"this test can be flaky. Perhaps we should just use zip instead of parProduct."*** | **→ TODO.** A defect *and* a proposed fix. Same spec as [#141](https://github.com/rchillyard/TableParser/issues/141), now Tier 1 item 2 — this comment names a likely cause the issue does not. |
| `CellParser.scala:69` | NOTE | *"not used"* | **→ CONSIDER** — dead code is a decision, not documentation | Note is incorrect                                                                         |
| `CellParsers.scala:110`, `:117` | NOTE | *"not used"* | as above | Note is correct for this implementation: but we need a TODO/Issue that covers this design |
| `Aggregation.scala:7` | NOTE | *"unused"* | as above |
| `TableParserSpec.scala:26` | NOTE | *"unused"* | as above |
| `Table.scala:403` | NOTE | *"up to 2.0.2, this didn't close the file … nor report a failure"* | correct — history, and the fix shipped |
| `InputTransformer.scala:285` | NOTE | *"This cannot fail because we filtered out any failures"* | correct |
| `ZIOTable.scala:459` | NOTE | *"opening a Source can fail … (a failure, not a defect)"* | correct |
| `Table.scala:571`, `:870` | NOTE | usage guidance | correct |
| `TableParser.scala:416` | CONSIDER | cites #183 | tracked |
| `EvaluateZIO.scala:56`, `:72` | CONSIDER/NOTE | in the commented-out helper, see [#140](https://github.com/rchillyard/TableParser/issues/140) | tracked |

### Number

| file:line | tag | text | verdict | Robin's update |
|---|---|---|---|---|
| `GeneralNumber.scala:844` | NOTE | ***"this is not used, but it doesn't seem to handle fuzziness (of the exponent) properly either"*** | **→ TODO.** A suspected defect, stated, untracked. | Create issue |
| `ComplexFunctionSpec.scala:143`, `:173` | NOTE | ***"Using explicit wide Gaussian tolerance as workaround pending fix to Fuzziness.combine"*** | **→ TODO.** A workaround awaiting a named fix — and it names the thing to fix. |
| `PrimesSpec.scala:52` | NOTE | ***"This has been known to fail occasionally"*** | **→ TODO.** The #601 pattern exactly. |
| `BenchmarkSpec.scala:35` | NOTE | *"this takes a long time and may fail"* | **→ TODO** or a `Slow` tag honoured in CI |
| `ErrorCommutativeMonoid{,Func}Spec` ×4 | NOTE | *"simple wall-clock comparison; if flaky in CI, relax…"* | **→ TODO.** A wall-clock assertion in CI is a flake waiting to happen. |
| `MaybeFuzzySpec.scala:163` | NOTE | *"the previous value was incorrect for two reasons"* | correct — history |
| `Valuable.scala:71` | NOTE | *"this should never fail. Famous last words!"* | **borderline** — the joke is doing the work of a `CONSIDER` |
| `Expression.scala:274` | NOTE | debugging aid that "can also cause…" | worth re-reading in full |
| `Bumperator.scala:55`, `ExpressionSpec.scala:134`, `EmptySpec.scala:49`, `ExpressionMatchers.scala:240`, `RationalPropertySpec.scala:50`, `RationalSpec.scala:148` | NOTE | explanatory | correct |
| nine `CONSIDER`s citing issues (#245, #263, #305, #307, #429, #443, #479, #527, #544, #548) | | | tracked |

### Flog — ten in one file

| file:line | tag | text | verdict | Robin's update |
|---|---|---|---|---|
| `FlogSpec.scala:110`, `:341`, `:423` | NOTE | ***"sometimes this test will fail. Not to worry."*** |
| `FlogSpec.scala:123`, `:154`, `:163`, `:180`, `:190`, `:354`, `:436` | NOTE | ***"sb should not be empty but it might be if you run this unit test on its own"*** |

**All ten → TODO.** This is the single clearest instance of the pattern in the twelve projects.
"Not to worry" is "Recommend just re-run" in different words, and the second group describes a
test that passes or fails depending on what else ran — shared mutable state between tests, which
is a defect in the suite, not a quirk to document.

### LaScala

| file:line | tag | text | verdict | Robin's update |
|---|---|---|---|---|
| `Tree.scala:974` | NOTE | ***"this looks totally wrong!!"*** | **→ FIXME.** Nothing is tracking it. | Create issue |
| `FPSpec.scala:332` | NOTE | ***"this was originally a Failure and I have broken that."*** | **→ FIXME.** An admitted regression. | Create issue |
| `CSVSpec.scala:48`, `:67` | NOTE | *"works only with 2.11"* / *"only works with Scala 2.11"* | **stale** — LaScala is on 3.9 |
| `PredicateSpec.scala:240`, `:265` | NOTE | *"fails for 2.10"* | **stale** |
| `AccountRecordTest.scala:133` | NOTE | *"only works with Scala 2.11"* | **stale** |
| `RenderableFunctionSpec.scala:59` | NOTE | *"this has failed (rarely) with true in all caps"* | **→ TODO** — a flake |
| `MPTTSpec.scala:163` | NOTE | *"the logic that follows doesn't make any sense for an UnvaluedBinaryTree"* | **→ CONSIDER** at least |
| `Predicate.scala:574`, `:581` | NOTE | *"for now, we will just turn an RPN list into a String"* | **→ TODO** |
| `TaxonomicTreeSpec.scala:210` | NOTE | *"addNode is protected so, for now at least, we use :+ as a surrogate"* | **→ TODO** |
| `Spy.scala:207` | NOTE | explanatory | correct |
| six `CONSIDER`s | | #57, Tree:855, Scalar:186/209/238, ClosureSpec:39, TreeSpec:233/321, RenderableFunctionSpec:232 | tracked by the sweep |

**The five stale ones are worth a moment**: they describe behaviour under Scala 2.10 and 2.11 in a
project built with 3.9. Either the limitation is gone and the note misleads, or it survived the
migration under a different cause and the note is now wrong about why.

### Majabigwaduce, Args

All ten Majabigwaduce hits and the one in Args are **correct as `NOTE`**. They are post-migration
explanations — *"prior to the Typed migration, this test exercised a genuine ClassCastException"*,
*"MatrixOperation doesn't terminate the ActorSystem it's given, so each test owns that
responsibility"* — which is exactly what `NOTE` is for. The regex matched words like *fail* and
*exception* appearing in explanations of why something is correct.

---

## Summary of what is proposed

| | count |
|---|---|
| `NOTE` → `TODO` or `FIXME` | **26** |
| `NOTE` → `CONSIDER` (dead code, design doubt) | **6** |
| Stale notes about Scala 2.10/2.11 | **5** |
| Correct as tagged (false positives) | **36** |
| Already citing an issue | **24** |

The 26 are the list worth acting on. Nine of them are in two files — `FlogSpec` and the three
`ErrorCommutativeMonoid` specs — and every one of those nine says some version of *this test
sometimes fails and that is fine*.

## Method, and what it cannot see

Comments were parsed per file, a Scaladoc block or a run of adjacent `//` lines counting as one
comment. Tagged comments were matched against a list of symptom words and the 97 hits read
individually; the verdicts above are judgement, not pattern-matching, and the false positives are
listed so the ratio is visible.

What this cannot find is a comment that describes a defect without using any of those words, and
a defect nobody commented on at all. The sweep of 2026-11-20 found what someone had written down
and tagged as backlog; this pass finds what someone wrote down and tagged as documentation.
Neither finds what nobody wrote down — which is how Number #601 was eventually caught, by a build
failing.
