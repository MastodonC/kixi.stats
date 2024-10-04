## Dev dependencies

- [node.js](https://nodejs.org/en/)
- The [Clojure command line tool](https://clojure.org/guides/install_clojure)
- [Babashka](https://github.com/babashka/babashka#installation)

## Publishing to Clojars

The template for the project's `pom.xml` lives at
[`template/pom.xml`](https://github.com/MastodonC/kixi.stats/blob/main/template/pom.xml).

To create a new release:

- Update the version in [build.clj](https://github.com/MastodonC/kixi.stats/blob/main/build.clj)
- Make a new [Github Release](https://github.com/MastodonC/kixi.stats/releases) with tag `v<the-new-version>`.

Submitting the release will create the new tag and trigger the following
command:

```
bb release
```

The new release will appear on Clojars.

## Linting

Code is linted with [`clj-kondo`](https://github.com/clj-kondo/clj-kondo):

```
bb lint
```

The first time you interact with the project, run the following command to lint
all dependencies and populate the `clj-kondo` cache:

```
bb lint-deps
```
