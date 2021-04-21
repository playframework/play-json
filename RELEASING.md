# Releasing

This is released from the `main` branch from `2.9.0` forward. Unless an older version needs patching, then it must be released from the maintenance branch, for instance `2.8.x` branch. If there is no maintenance branch for the release that needs patching, create it from the tag.

## Cutting the release

### Requires contributor access

- Check the [draft release notes](https://github.com/playframework/play-json/releases) to see if everything is there
- Wait until [main build finished](https://travis-ci.com/github/playframework/play-json/builds) after merging the last PR
- Update the [draft release](https://github.com/playframework/play-json/releases) with the next tag version (eg. `2.9.0`), title and release description
- Check that Travis CI release build has executed successfully (Travis will start a [CI build](https://travis-ci.com/github/playframework/play-json/builds) for the new tag and publish artifacts to Sonatype)

### Requires Sonatype access

- Release the staging repos at https://oss.sonatype.org/#stagingRepositories

### Check Maven Central

- The artifacts will become visible at https://repo1.maven.org/maven2/com/typesafe/play/
