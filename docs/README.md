During the Play JSON build these docs are packaged into a `playdoc`
JAR file and published. The docs are eventually aggregated together
with the docs from other Play projects by
[Omnidoc](https://github.com/playframework/omnidoc).

The packaging and publishing of the docs and running the documentation
tests is handled by the main `sbt` build at the root of the Play JSON
project.

There is also a separate `sbt` build configuration included in this
`docs` directory. This build configuration has been kept separate
from the main configuration at the root of the Play JSON project in
order to avoid a circular dependency between Play JSON and the Play
project's `play-docs-sbt-plugin`.

## How to the `docs` sbt build

You only need to follow these steps if you want to validate the docs
markdown or view the markdown interactively in a browser. If you just
want to compile Play JSON and publish its documentation then simply
run `sbt` at the root of the Play JSON project.

Assuming you want to validate the markdown or view it in a browser,
here's what you need to do:

1. Build Play JSON by running `sbt +publishLocal` in the root directory
   of this project.

2. Build the main Play Framework project. This is the second step
   because Play Framework has a dependency on the Play JSON project.

3. Go to the `docs` directory and run `sbt validateDocs` or `sbt run`.
   This is the third step because it depends on the
   `play-docs-sbt-plugin` which is built in the second step.

Voila!