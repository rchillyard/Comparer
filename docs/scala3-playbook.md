# Scala 3 Playbook: what we decided for Comparer, and why

This captures the decisions made while moving **Comparer** from Scala 2.13 to Scala 3 (October 2026),
so that the same moves go more smoothly for **XmlDoc**, **TableParser** and other Phasmid libraries.

Comparer is the reference implementation:
- 1.1.0 (PR #13): cross-built for Scala 2.13 and 3, with minimal changes.
- 2.0.0 (PR #15): Scala 3 only, idiomatic, and derivation-based.

Each section gives the decision, the reason, and anything that will be different for the next project.

---

## 1. Release strategy: two steps

1. **A 1.x release cross-built for 2.13 and 3, with no API changes.** Fix only what Scala 3 won't compile, using constructs that still compile on 2.13 (see §4). Existing users get a Scala 3 artifact without changing their code.
2. **A 2.0 release that is Scala 3 only, where breaking changes are allowed.** Rewrite in Scala 3 style, replace hand-written arity families with derivation, and publish a migration table (see §8). Point 2.13 users at the 1.x release.

**Compile with the oldest Scala 3 LTS you can (3.3.x); test on the current LTS (3.9.x) too.**
Scala 3 compiled code works forwards: code built with 3.3 can be used from 3.3 through 3.9+, but code built with 3.9 can only be used from 3.9+.
- Don't put the second Scala 3 version in `crossScalaVersions`. Both would publish as `foo_3` and overwrite each other. Add a CI step instead: `sbt "++3.9.0!" test`.
- **XmlDoc** is currently on 3.8.4, so its users must already be on 3.8+. Going down to 3.3 is only possible if it uses no 3.4+ features. That's worth checking, and the decision is Robin's.
- **TableParser** (2.13.17) needs step 1 before derivation is possible.

---

## 2. Build and tooling (Gambit is the template)

| Item | Decision | Why |
|---|---|---|
| sbt | **1.11.7** | Matches Gambit. Old launchers (e.g. 1.4.9) fail on JDK 21, and Central publishing needs 1.11 (`localStaging`). |
| Publishing | Sonatype **Central Portal**: copy Gambit's `publish.sbt` (releases to `localStaging`, snapshots to `central-snapshots`, credentials from `~/.sbt/sonatype_credentials`). Keep each project's own licence and SCM URLs. | `oss.sonatype.org` no longer accepts releases. |
| sbt-sonatype | 3.12.2 | Same as Gambit. |
| `versionScheme` | `early-semver` | Same as Gambit. |
| sbt-scoverage | **2.4.3** | 2.4.4 needs sbt 1.12. The resolver reports this as a misleading "not found". |
| scalac flags | `-deprecation -feature -unchecked -Wunused:all` | Catches dead code and leftovers. |
| Lint noise | `Global / excludeLintKeys ++= Set(pomIncludeRepository, publishMavenStyle)` | False "unused key" warnings for `ThisBuild` publish settings. |

**CircleCI** (`.circleci/config.yml`, version 2.1):
- Image: `cimg/openjdk:17.0` (`circleci/openjdk:8-jdk` is retired).
- Cache key: `v2-dependencies-{{ checksum "build.sbt" }}-{{ checksum "project/build.properties" }}`, caching `~/.sbt`, `~/.ivy2/cache` and `~/.cache/coursier`. Make sure the save key and the restore key match exactly. Comparer's old config had a stray `--`, so the cache never restored.
- Steps: `sbt Test/compile`, `sbt test`, then `sbt "++3.9.0!" test`.
- For a cross-built 1.x release, use `sbt +Test/compile` and `sbt +test`.
- Gambit lesson: a job filtered to `branches: only: main` never runs if the default branch is `master`.

**Releasing:**
```
sbt clean publishSigned sonaUpload      # then publish in the Central Portal (or use sonaRelease)
```
- Use `+publishSigned` only for a cross-built release.
- Always start with `clean` if a coverage run has happened: `coverage` leaves instrumented classes in `target/`.
- If the upload fails with `NoSuchMethodError: SSLParameters.setApplicationProtocols`, sbt is running on Java 8 before update 251. Use `sbt -java-home "$(/usr/libexec/java_home -v 21)" …`.
- **javadoc.io no longer builds docs by itself (as of October 2026).** After a release, its page (`javadoc.io/doc/GROUP/ARTIFACT`) lists the new version as "Not uploaded yet". Someone must tick the version and click **Upload selected** before any docs appear. Until then, the page has no content and the badge says "unknown". This needs doing after every release, so it's part of releasing. (The published `-javadoc.jar` was fine all along: Comparer 2.0.0's has 436 files.) Even after upload, javadoc.io's badges lag for hours or days.
- **Alternative: host the Scaladoc yourself, as Gambit does.** Use sbt-ghpages (`enablePlugins(GhpagesPlugin, SiteScaladocPlugin)`, `git.remoteRepo`) to publish to `rchillyard.github.io/PROJECT/latest/api/`, with a static "api docs" badge linking there. It never goes stale and doesn't depend on javadoc.io.
- GitHub caches README badge images through its Camo proxy. Clear a stale one with `curl -X PURGE <camo-url>`; you can get the Camo URLs from `gh api repos/OWNER/REPO/readme -H "Accept: application/vnd.github.html"`.

---

## 3. Working agreements (how Robin likes to work)

- **Branches:** one branch per version, e.g. `V2_0_0`, off the current release branch, with one commit per logical step and the tests passing after each.
- **Pushing:** Claude commits but never pushes (the permission settings refuse it anyway). Robin pushes. Once a branch is pushed, Claude may open the PR with `gh pr create`.
- **Infix methods:** keep alphanumeric methods infix (`a orElse b`). In Scala 3, mark the definition `infix def`. Don't rewrite callers to dot syntax. Symbolic names (`:|`, `&&`) need nothing.
- **Formatting:** leading commas in multi-line lists.
- **Visibility:** never choose public/private for a new repository without asking.
- **Before and during edits:** look at Robin's existing style first (e.g. Gambit) and match it. Say when editing a file Robin may have open.

---

## 4. Cross-building 2.13 and 3: the fixes that compile on both

| Scala 3 error | Fix that also compiles on 2.13 |
|---|---|
| `{ x: Int => … }`: parentheses required | `{ (x: Int) => … }` |
| `Foo.unapply(f).get`: no `get` on `Foo` | Use the fields: `(f.a, f.b)`. In Scala 3, a case class's generated `unapply` returns the instance itself, not an `Option`. Pattern matching is unaffected. |
| `comparer3(DateJ)`: a companion object is not a function | `comparer3(DateJ.apply)` |
| `DateJ.curried` | `(DateJ.apply _).curried` |
| Explicitly passing arguments to a context bound (Scala 3.6+, i.e. on 3.9) | Declare an explicit `(implicit …)` parameter list instead of the context bound. Scala 3's `using` doesn't exist in 2.13. |

---

## 5. Scala 3 idioms adopted for 2.0

- **Givens:**
  - `given`/`using`/`summon` replace `implicit`/`implicitly`.
  - Name the givens (`given intComparer: Comparer[Int]`) and put them in the type class's companion object, so users never need an import.
  - Move "wrapper" instances (Option, Seq, Try, …) into the companion as well. Comparer 1.x made users `extends Comparers`.
- **No `given Conversion`s for users.** Every use site then gets a feature warning unless it imports `scala.language.implicitConversions`. We tested this. Provide an explicit factory (`Comparer.fromOrdering`).
- **Enums for ADTs:**
  - Turn sealed traits with case classes and case objects into `enum`s, with the methods matching on `this`.
  - Export the cases at package level so existing code still compiles: `export Comparison.{Different, Less, More, Same}`. Export frequently used companion values too. Comparer forgot `Less` and `More` at first, and the README examples silently needed an import.
- **Top-level definitions** replace the `package object`.
- **A trait shouldn't be lazy.** Comparer 1.x had `Comparison extends Thunk[Kleenean]`, but nothing was deferred. It was just a `()` that hid a conversion. Use explicitly named conversions (`toKleenean`, `toOption`), and get laziness from by-name parameters (`orElse(c: => …)`). A concrete class that really defers work (e.g. `Sorted`) may still extend a function.
- **Check conversions against the operators.** Comparer's old `apply()` mapped `Less` to `Truth(true)`, while `&`/`|`/`toInt` treated `More` as true. Make the conversion agree with the operators, and add a property test over all pairs of values.
- **Never a given of a primitive type.** Comparer had `implicit val TotalThreads: Int`, which any `using Int` could pick up. Use a default parameter.
- **Explicitly passed context bounds:** write an explicit `using` clause when callers pass the instances explicitly. This is what broke compilation on 3.9.
- **`self =>` becomes `this`.** In Scala, a lambda's `this` is the enclosing object, so the alias is only needed for anonymous classes or self-types.
- **Indentation syntax**, applied with the compiler's own rewrites in two passes, each needing a clean compile:
  ```
  sbt 'set scalacOptions ++= Seq("-new-syntax", "-rewrite")' clean Test/compile
  sbt 'set scalacOptions ++= Seq("-indent", "-rewrite")' clean Test/compile
  ```
  - Put this in its own commit, so it can be dropped.
  - The `-indent` pass silently skips some files (e.g. ones ending with commented-out code), so check with `grep -rlE "^(class|object|trait|enum) .*\{$" src` and convert any stragglers by hand.
  - ScalaTest blocks become `it should "…" in:`, which is valid on 3.3.
- **`_` in types becomes `?`.** Drop trailing ` _` for eta-expansion, which 3.9 warns about.

---

## 6. Derivation: the core pattern (this is what XmlDoc and TableParser need)

**What it replaces:**
- Comparer had `comparer1`…`comparer11`, plus a `strip` helper using `asInstanceOf` casts, plus `tuple2`…`tuple9`.
- **TableParser** has `cellParser` (up to 13), `renderer` (up to 19), `generator`, `rendererGenerator` and `converter`.
- **XmlDoc** has `extractor`, `extractorPartial`, `renderer`, `multiExtractor` and `rendererSuper`.
- Both TableParser and XmlDoc get field names by reflection (`Reflection.extractFieldNames`, `getDeclaredFields` plus a `ClassTag`). Java doesn't guarantee that `getDeclaredFields` returns fields in declaration order, and it happens at runtime. **`Mirror` provides the field names in declaration order, at compile time.**

**`derives TC` is sugar:** `case class X(...) derives TC` adds `given TC[X] = TC.derived` to `X`'s companion object. All the type class needs is a `derived` method in its companion.

### 6.1 Products: one generic tuple instance does most of the work

```scala
given emptyTupleComparer: Comparer[EmptyTuple] = same

// By-name and lazy, so that a recursive type can use its own instance while it is still being defined.
given tupleComparer[H, R <: Tuple](using hc: => Comparer[H], rc: => Comparer[R]): Comparer[H *: R] =
  lazy val headComparer = hc
  lazy val restComparer = rc
  t1 => t2 => headComparer(t1.head)(t2.head) orElse restComparer(t1.tail)(t2.tail)
```

A case class becomes a tuple of its fields (`Mirror.ProductOf[T].MirroredElemTypes`), so the tuple instance handles every field. There's no arity limit, and Comparer tests one with 23 fields.

### 6.2 `derived` handles products and sums

```scala
inline def derived[T](using m: Mirror.Of[T]): Comparer[T] = inline m match
  case s: Mirror.SumOf[T] =>
    sumComparer(s, caseComparers[s.MirroredElemTypes])
  case p: Mirror.ProductOf[T] =>
    productComparer(p, summonInline[Comparer[p.MirroredElemTypes]])

private def productComparer[T](p: Mirror.ProductOf[T], ec: => Comparer[p.MirroredElemTypes]): Comparer[T] =
  lazy val fieldsComparer = ec
  // A type with Mirror.ProductOf is always a Product.
  def fields(t: T): p.MirroredElemTypes = Tuple.fromProduct(t.asInstanceOf[Product]).asInstanceOf[p.MirroredElemTypes]
  t1 => t2 => fieldsComparer(fields(t1))(fields(t2))

private inline def caseComparers[Ts <: Tuple]: List[Comparer[?]] = inline erasedValue[Ts] match
  case _: EmptyTuple => Nil
  case _: (t *: ts) => caseComparer[t] :: caseComparers[ts]

// Prefer a case's own given instance; otherwise derive one for it.
private inline def caseComparer[A]: Comparer[A] = summonFrom:
  case c: Comparer[A] => c
  case _ => derived[A](using summonInline[Mirror.Of[A]])
```

- `sumComparer` compares `s.ordinal(t1)` against `s.ordinal(t2)`, which is declaration order, then that case's instance.
- Imports: `scala.compiletime.{erasedValue, summonFrom, summonInline}` and `scala.deriving.Mirror`.
- For a type class that *builds* values (a parser or extractor) rather than *consuming* them, `m.fromProduct(tuple)` turns the parsed tuple back into a `T`.

### 6.3 Field names, the replacement for `Reflection.extractFieldNames`

Tested on 3.3.8 and 3.9.0; for `Player(name, rating, active)` it gives `List(name, rating, active)`:

```scala
import scala.compiletime.{constValue, erasedValue}
import scala.deriving.Mirror

inline def labels[Ls <: Tuple]: List[String] = inline erasedValue[Ls] match
  case _: EmptyTuple => Nil
  case _: (l *: ls) => constValue[l].asInstanceOf[String] :: labels[ls]

inline def fieldNames[T](using m: Mirror.ProductOf[T]): List[String] = labels[m.MirroredElemLabels]
```

### 6.4 Lessons from deriving Comparer

- **Sealed traits versus enums.** Enum cases widen to the enum type (`Shape.Circle(1): Shape`). A sealed trait's case classes keep their own types. Because the type class is invariant, callers must name the trait: `Compare[Contact](Email(…), Phone(…))`. Document this.
- **Field order.** Derivation uses declaration order. For any other order, or to rename or skip fields, keep an explicit builder; Comparer uses `same :| (_.year) :| (_.month)`. Expect the same for TableParser columns that don't match field names.
- **Recursive types** (`enum Tree { case Leaf; case Node(l: Tree, v: Int, r: Tree) }`) work on 3.3 and 3.9, thanks to the by-name and lazy instances above. Test one.
- **Placing test types.** Enums and sealed traits used by tests go at the top level of the spec file. Local case classes inside a test can use `derives`.

---

## 7. Testing and quality

- **Keep each ported test's expected result.** When an API is replaced (e.g. `comparerN` with `derives`), rewrite the call but keep the assertion, which proves the semantics didn't change.
- **Test on every Scala version the build supports** before each commit (`sbt test "++3.9.0!" test`).
- **Coverage:** Comparer is at 100% statements and branches (`sbt clean coverage test coverageReport`). Robin also measures with IntelliJ's built-in coverage tool (classes and methods).
- **Async tests:** ScalaTest's default patience is **150 ms**, which isn't enough on a shared CI machine. In specs that use `whenReady`, add `override implicit val patienceConfig: PatienceConfig = PatienceConfig(timeout = Span(5, Seconds))`.
- **Fix slow code when a timeout points at it.** `List :+` in a loop is quadratic; use a `Vector` accumulator.
- **README examples:** compile them by pasting them into a temporary file under `src/test/scala`, running it, then deleting it. Do it again whenever the API changes.
- **Scala 3.9 surprises:** besides the context-bound errors, expect warnings about alphanumeric infix use, unnecessary eta-expansion `_`, and pattern exhaustivity where `Seq` is matched with `List` patterns (type the parameters as `List`).

---

## 8. Documentation

- **README for 2.0:**
  - Install line on the new version, badges on `foo_3`, and a note to use 1.x for 2.13.
  - Rewrite the examples in Scala 3.
  - Add a **"Migrating from 1.x" table** (old → new) covering every removed or changed API. Include changes in meaning (e.g. Comparer's `Truth(true)` changed from Less to More) and removed type aliases.
- **The API section should be a complete reference, not a copy of 1.x.** Comparer's 2.0 README first kept the 1.x shape (a selection of members, with long copied doc comments) and silently omitted much of the API. List every public member, grouped by purpose, with one line each, and check the behaviours it describes against the library.
- **Audit the README before release:** check each statement against the code. Doc comments copied from the code carry their typos with them, and "100% coverage"-style claims drift. A sentence like "Full Scaladoc is available via javadoc.io" is only true once the version has been uploaded there (see §2).
- **Versioning:** add an entry for each release, with accurate method names.

---

## 9. Checklist for the next project

1. Read this playbook, the project's README and build files, and Gambit's `publish.sbt` and `.circleci/config.yml`.
2. Ask Robin which branch to start from, and about the minimum Scala version (3.3 or the project's current version).
3. Bring the build up to date: sbt 1.11.7, Central publishing, sbt-scoverage 2.4.3, the CircleCI template, and the scalac flags. Then measure test results and coverage as a baseline.
4. *(2.13 projects only)* Cross-build for 2.13 and 3, using the fixes in §4. That's a 1.x release.
5. Scala 3 only: givens, enums, top-level definitions, no thunks, no `given Conversion`s, `infix def`.
6. Derivation: the generic tuple instance, `derived` handling products and sums, and field names from `MirroredElemLabels`. Retire the arity families and reflection-based field names. Keep explicit builders for reordering and renaming.
7. Indentation syntax rewrite, as a separate commit.
8. Tests on 3.3 and 3.9, 100% coverage, and patience settings for async specs.
9. README: examples, API, migration table, versioning. Compile the examples.
10. Commit per step; Robin pushes; open the PR; turn on Auto-fix; then `sbt clean publishSigned sonaUpload`.
11. After the release: upload the new version on javadoc.io (or publish the Scaladoc to GitHub Pages), and check that the README's badges and docs links show it.
