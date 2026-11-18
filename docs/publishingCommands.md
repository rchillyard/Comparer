Publishing Commands for 12 open-source repositories
===================================================

All twelve default branches are `main`. The six that were `master` were renamed
on 2026-11-03.

Before publishing anything, check what actually needs it
-------------------------------------------------------

As of 2026-11-18 **every project below declares a version already on Maven
Central**, so publishing as-is would attempt to republish an existing version.
What each needs first is the post-release bump, and in four cases a backfilled
tag:

| project     | declared | on Central | tag for it | ready to publish? |
|-------------|----------|------------|------------|-------------------|
| Flog        | 1.1.0    | 1.1.0      | none       | no — bump + tag first |
| Matchers    | 1.1.1    | 1.1.1      | `V1_1_1`   | no — and 0 commits since the tag, so nothing to release |
| Args        | 2.1.0    | 2.1.0      | none       | no — bump + tag first |
| Comparer    | 2.1.0    | 2.1.0      | none       | no — bump + tag first |
| LaScala     | 2.2.0    | 2.2.0      | none       | no — bump + tag first |
| TableParser | 2.5.1    | 2.5.1      | `V2.5.1`   | no — has 2 unreleased commits, but needs a bump |
| Visitor     | 1.8.1    | 1.8.0      | `V1.8.0`   | bump is correct; only publish if 1.8.1 is wanted |

Run this to re-check before any release:

```bash
for p in Flog Matchers Args Comparer Visitor LaScala TableParser Gambit Gryphon Majabigwaduce Number XmlDoc; do a=$(echo "$p" | tr 'A-Z' 'a-z'); v=$(grep -hoE '^ *(ThisBuild / )?version *:= *"[^"]+"' ~/IdeaProjects/$p/build.sbt ~/IdeaProjects/$p/version.sbt 2>/dev/null | head -1 | grep -oE '"[^"]+"' | tr -d '"'); c=$(curl -s "https://repo1.maven.org/maven2/com/phasmidsoftware/${a}_3/maven-metadata.xml" | grep -oE '<latest>[^<]*</latest>' | sed 's/<[^>]*>//g'); printf '%-14s declared %-10s central %s\n' "$p" "${v:-?}" "${c:-?}"; done
```

Publishing one project
----------------------

Not as a single sbt invocation. `clean` must run on its own, and a compile step
between `clean` and the publish avoids a classloader failure seen on
2026-11-17. Both lessons came from real failed releases.

```bash
cd ~/IdeaProjects/PROJECT && git switch -q main && git pull -q --ff-only
```

```bash
grep -nE '^ *(ThisBuild / )?version *:=' build.sbt version.sbt 2>/dev/null
```

```bash
sbt clean
```

```bash
sbt Test/compile
```

```bash
sbt test mimaReportBinaryIssues doc publishSigned sonaUpload
```

Then Publish the deployment in the Central Portal, tag, create the GitHub
release, `sbt doc javaApiDoc makeSite ghpagesPushSite`, and open a post-release
PR moving `previousRelease` on and `version` to the next patch.

Why not a loop
--------------

A loop cannot read the version before publishing, which is the check that
matters most — it is what caught a wrongly-numbered bundle for Gryphon 1.8.0.
It also cannot notice that a bundle validated but was uploaded twice, or decide
whether a given project has anything worth releasing. Each publish wants a
person between the build and the Portal.
