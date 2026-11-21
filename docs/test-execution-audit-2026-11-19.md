# Audit: are all test directories actually compiled, run and covered? (12 projects)

Run 2026-11-19. Method: count test classes on disk, compare against what `sbt test` reports
running; build files then explain *why* a mismatch exists. The raw `sbt test` logs lived in a
session scratchpad and have not been kept — the figures below are what was taken from them.

Kept here, next to `scala3-playbook.md`, because it is about all twelve projects rather than
about Comparer. Its findings are issues in the projects concerned; what is worth keeping here
is the method, and in particular the `Passed: Total` discriminator described below.

## Issues filed

| Project | Issue |
|---|---|
| Comparer | https://github.com/rchillyard/Comparer/issues/29 — `src/it/scala` wired into nothing; rotted |
| Gambit | https://github.com/rchillyard/Gambit/issues/39 — 39 JUnit 4 tests never run (no sbt interface) |
| Gambit | https://github.com/rchillyard/Gambit/issues/40 — `IT` config invoked by neither `sbt test` nor CI |
| LaScala | https://github.com/rchillyard/LaScala/issues/29 — `IT` config invoked by neither `sbt test` nor CI |
| Number | https://github.com/rchillyard/Number/issues/216 — `core/src/it/scala` wired into nothing; rotted |

## Summary

| Project | Test dirs found | Suites disk → run | Frameworks / interface | Verdict |
|---|---|---|---|---|
| Args | `test/scala`, `test/java` | 2→2 ScalaTest; 4 `@Test`→4 | ScalaTest; JUnit 4 + `novocode:junit-interface:0.11` ✓ | clean |
| Comparer | `test/scala`, **`it/scala`** | 8→8; **2→0** | ScalaTest ✓ | **DEFECT** |
| Flog | `test/scala` | 3→3 | ScalaTest ✓ | clean |
| Gambit | `test/scala`, `test/java`, **`it/scala`** | 14→14; **39 `@Test`→0**; **2→0** | JUnit 4 lib, **no sbt interface** | **2 DEFECTS** |
| Gryphon | `test/scala`, `test/java` | 39 + 18 JUnit cls (not measured) | JUnit 5 + `jupiter-interface` 0.19.0 + plugin ✓ | clean (static only) |
| LaScala | `test/scala`, `test/java`, **`it/scala`** | 53→53 (+2 ScalaCheck); **3→0** | ScalaTest, ScalaCheck ✓; java tests commented out | **DEFECT** |
| Majabigwaduce | `test/scala`, `it/scala`, `cluster/test/scala` | 13+6→19 ✓ | ScalaTest ✓ | clean — reference impl |
| Matchers | `test/scala` | 3→3 (1 pending, deliberate) | ScalaTest ✓ | clean |
| Number | 6 modules `test/scala`, `core/test/java`, **`core/it/scala`** | 151→151; 140 `@Test`→140 (clean build); **8→0** | ScalaTest, ScalaCheck, JUnit 4 ✓ | **DEFECT** (`it` only) |
| TableParser | 8 modules `test/scala`, 4 × `it/scala` | 74→74; `it` via `integration`, not in CI | ScalaTest ✓ | clean; see note |
| Visitor | `test/scala`, `test/java` | 35→35; 18 `@Test`→18 | JUnit 5 + jupiter-interface ✓ | clean (fixed earlier) |
| XmlDoc | 6 aggregated modules + `kml-it`, `kafka-it` | 46→46; `kml-it` 3 + `kafka-it` 1 run by CI | ScalaTest ✓ | clean; coverage note |

## The discriminator that made the JUnit cases decidable

sbt prints a final `[info] Passed: Total N, …` line only when **more than one** test framework
reports. ScalaTest-only projects print just the ScalaTest summary. So a project with `@Test`
methods on disk and no such line ran none of them. Cross-checked in every case with
`sbt clean 'show Test/definedTestNames'`, which is authoritative.

- Args `Passed: Total 121` = 117 ScalaTest + 4 JUnit → runs.
- Visitor `Test run started (JUnit Jupiter)`, `Passed: Total 249` = 231 + 18 → runs.
- LaScala `Passed: Total 840` = 838 ScalaTest + 2 **ScalaCheck** (not JUnit — LaScala's Java
  tests are entirely commented out, so nothing is being skipped).
- Number `Passed: Total 1903` = 1758 + 5 ScalaCheck + 140 JUnit → runs (see trap below).
- **Gambit: no such line at all, `junit` appears 0 times in the log**, `TicTacToeOpsTest`
  absent from `definedTestNames` after `clean`, and the test classpath holds `junit-4.13.2.jar`
  and nothing else. 39 tests compiled, never run.

## One trap worth remembering: stale Zinc analysis

I nearly filed a second Number issue for "0 of 140 JUnit tests run". It was wrong. In Robin's
working tree the four Java test classes were missing from `core/Test/definedTestNames` even
though they had just been recompiled; after `core/clean` all four appear and all 140 run.

So **an incremental local `sbt test` can silently drop every Java test** with no indication.
CI always builds from a fresh checkout and is unaffected. Any future audit of this kind must
check from clean before concluding a test does not run.

## Deliberate behaviour — correctly *not* reported as defects

- Slow-test splits: Gambit, LaScala, Majabigwaduce and Number exclude
  `org.scalatest.tags.Slow` from the fast CI job on purpose. Gambit additionally runs
  `-n org.scalatest.tags.Slow` in a `slow-tests` job on `main`.
- `pending` / `ignore`: Matchers 1 pending, LaScala 16 ignored, Number 7 pending,
  Majabigwaduce 1 ignored, XmlDoc 1 ignored — all deliberate markers.
- `Gambit/src/main/scala/com/phasmidsoftware/gambit/attic` — under `main`, not a test dir.
- XmlDoc `kml-it` / `kafka-it`: unaggregated on purpose (documented in README, DESIGN.md and
  `build.sbt`) **and** explicitly run by `.circleci/config.yml` (`sbt kmlIt/test`,
  `sbt kafkaIt/test`). Execution is fine.
- TableParser `integration`: wires `core`, `cats`, `kafka-cats`, `kafka-zio` `src/it/scala`
  into one unpublished project, documented in `build.sbt` as not run by `sbt test` or CI.

## Observations reported but not filed (Robin's call)

1. **TableParser** — the `integration` project is never *compiled* by CI, so its 4 `it`
   directories have no rot guard. They do compile today (`sbt integration/Test/compile`
   succeeds, 8 sources). But Comparer and Number are what unguarded `it` trees become. A
   compile-only CI step would cost seconds.
2. **XmlDoc** — `kml-it` and `kafka-it` run in CI but are outside root's `aggregate`, so
   `sbt clean coverage test coverageAggregate` excludes them: the production code in `kml`,
   `kafka-cats` and `kafka-zio` that they exercise gets no coverage credit.
3. **Number CI** uses `-l Slow` where every other project uses `-l org.scalatest.tags.Slow`.
   ScalaTest's `-l` wants the fully-qualified tag, so this likely excludes nothing — it makes
   CI run *more* than intended, not fewer, so it is not a silent skip, but it is a wart.
4. `TableParser/zio/src/it/scala` exists but holds no files (empty package dirs).
   `TableParser/core/src/it/scala` carries several committed `.DS_Store` files, and its one
   `.scala` file (`TeamProject.scala`) is a support class, not a suite.
5. **Visitor** has a stray `circleCIconfig.yml` at the repo root beside the real
   `.circleci/config.yml`.

## Caveats on the measurements

- **Gryphon was not run.** Another session held that checkout (on `chore/83-self-merge-guards`
  with 7 modified files). Everything above for Gryphon is from `origin/main` only: `build.sbt`
  declares `junit-jupiter-api`, `junit-jupiter-engine` **and**
  `com.github.sbt.junit % jupiter-interface % 0.19.0`, and `project/plugins.sbt` adds
  `sbt-jupiter-interface % 0.19.0`. That is the correct JUnit 5 wiring, and there is no
  `src/it`. Its 18 Java test classes / 204 `@Test` were counted on disk but never executed here.
- Several projects sit on branches other than `main`. Measured from the working tree as found:
  Args `tag-triage`, Comparer `post-release-bump`, Flog `post-release-bump`, Gambit `main`,
  LaScala `post-release-bump`, Majabigwaduce `tag-triage` (1 modified), Matchers `tag-triage`,
  **Number `standardization` (17 modified files — pre-existing, not mine)**, TableParser
  `module_test_paths`, Visitor `docs/binaryheap-equivalent-mutants`, XmlDoc `main`.
  The Comparer and LaScala defects were re-verified against `origin/main` before filing.
- Nothing was modified in any repo. The two "does it still compile?" probes used session-only
  `sbt set` commands; `git status` was clean afterwards in both.
