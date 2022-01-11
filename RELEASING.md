# Releasing

This is released from the `main` branch from `2.9.0` forward. Unless an older version needs patching, then it must be released from the maintenance branch, for instance `2.8.x` branch. If there is no maintenance branch for the release that needs patching, create it from the tag.

## Cutting the release

### Requires contributor access

- Check the [draft release notes](https://github.com/playframework/play-json/releases) to see if everything is there
- Wait until [main build finished](https://github.com/playframework/play-json/actions/workflows/publish.yml) after merging the last PR
- Update the [draft release](https://github.com/playframework/play-json/releases) with the next tag version (eg. `2.9.0`), title and release description
- Check that GitHub Actions release build has executed successfully (GA will start a [CI build](https://github.com/playframework/play-json/actions/workflows/publish.yml) for the new tag and publish artifacts to Sonatype)

### Check Maven Central

- The artifacts will become visible at https://repo1.maven.org/maven2/com/typesafe/play/
