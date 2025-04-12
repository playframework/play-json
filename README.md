# Play JSON

[![Twitter Follow](https://img.shields.io/twitter/follow/playframework?label=follow&style=flat&logo=twitter&color=brightgreen)](https://twitter.com/playframework)
[![Discord](https://img.shields.io/discord/931647755942776882?logo=discord&logoColor=white)](https://discord.gg/g5s2vtZ4Fa)
[![GitHub Discussions](https://img.shields.io/github/discussions/playframework/playframework?&logo=github&color=brightgreen)](https://github.com/playframework/playframework/discussions)
[![StackOverflow](https://img.shields.io/static/v1?label=stackoverflow&logo=stackoverflow&logoColor=fe7a16&color=brightgreen&message=playframework)](https://stackoverflow.com/tags/playframework)
[![YouTube](https://img.shields.io/youtube/channel/views/UCRp6QDm5SDjbIuisUpxV9cg?label=watch&logo=youtube&style=flat&color=brightgreen&logoColor=ff0000)](https://www.youtube.com/channel/UCRp6QDm5SDjbIuisUpxV9cg)
[![Twitch Status](https://img.shields.io/twitch/status/playframework?logo=twitch&logoColor=white&color=brightgreen&label=live%20stream)](https://www.twitch.tv/playframework)
[![OpenCollective](https://img.shields.io/opencollective/all/playframework?label=financial%20contributors&logo=open-collective)](https://opencollective.com/playframework)

[![Build Status](https://github.com/playframework/play-json/actions/workflows/build-test.yml/badge.svg)](https://github.com/playframework/play-json/actions/workflows/build-test.yml)
[![Maven](https://img.shields.io/maven-central/v/org.playframework/play-json_2.13.svg?logo=apache-maven)](https://mvnrepository.com/artifact/org.playframework/play-json_2.13)
[![Repository size](https://img.shields.io/github/repo-size/playframework/play-json.svg?logo=git)](https://github.com/playframework/play-json)
[![Scala Steward badge](https://img.shields.io/badge/Scala_Steward-helping-blue.svg?style=flat&logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAQCAMAAAARSr4IAAAAVFBMVEUAAACHjojlOy5NWlrKzcYRKjGFjIbp293YycuLa3pYY2LSqql4f3pCUFTgSjNodYRmcXUsPD/NTTbjRS+2jomhgnzNc223cGvZS0HaSD0XLjbaSjElhIr+AAAAAXRSTlMAQObYZgAAAHlJREFUCNdNyosOwyAIhWHAQS1Vt7a77/3fcxxdmv0xwmckutAR1nkm4ggbyEcg/wWmlGLDAA3oL50xi6fk5ffZ3E2E3QfZDCcCN2YtbEWZt+Drc6u6rlqv7Uk0LdKqqr5rk2UCRXOk0vmQKGfc94nOJyQjouF9H/wCc9gECEYfONoAAAAASUVORK5CYII=)](https://scala-steward.org)
[![Mergify Status](https://img.shields.io/endpoint.svg?url=https://api.mergify.com/v1/badges/playframework/play-json&style=flat)](https://mergify.com)

Play JSON is a powerful Scala JSON library, originally developed by the Play team for use with Play Framework. It uses Jackson for JSON parsing and has no Play dependencies.

We've provided some documentation here on how to use Play JSON in your app (without Play). For more information on how to use Play JSON in Play, please refer to the [Play documentation](https://www.playframework.com/documentation/2.8.x/ScalaJson).

## Getting Started

To get started, you can add play-json as a dependency in your project:

* sbt
  ```scala
  libraryDependencies += "org.playframework" %% "play-json" % -version-
  ```
* Gradle
  ```
  compile group: 'org.playframework', name: 'play-json_2.13', version: -version-
  ```
* Maven
  ```xml
  <dependency>
    <groupId>org.playframework</groupId>
    <artifactId>play-json_2.13</artifactId>
    <version>-version-</version>
  </dependency>
  ```

See [GitHub releases](https://github.com/playframework/play-json/releases) for the correct version.
 
Play JSON supports Scala 2.12, 2.13 and Scala 3.3+. Choosing the right JAR is automatically managed in sbt. If you're using Gradle or Maven then you need to use the correct version in the `artifactId`.

## JSON AST

The base type in Play JSON is `play.api.libs.json.JsValue`, and has several subtypes representing different JSON types:
 - `JsObject`: a JSON object, represented as a Map. Can be constructed from an ordered `Seq` or any kind of `Map` using `JsObject.apply`
 - `JsArray`: a JSON array, consisting of a `Seq[JsValue]`
 - `JsNumber`: a JSON number, represented as a `BigDecimal`.
 - `JsString`: a JSON string.
 - `JsBoolean`: a JSON boolean, either `JsTrue` or `JsFalse`.
 - `JsNull`: the JSON `null` value.

The `play.api.libs.json` package includes several features for constructing JSON from scratch, as well as for converting to and from case classes.

## Basic reading and writing

The `play.api.libs.json.Json` object has several methods for reading and writing:

`Json.parse` parses a JSON string or `InputStream` into a JSON tree:

```scala
val json: JsValue = Json.parse("""
{
  "name" : "Watership Down",
  "location" : {
    "lat" : 51.235685,
    "long" : -1.309197
  },
  "residents" : [ {
    "name" : "Fiver",
    "age" : 4,
    "role" : null
  }, {
    "name" : "Bigwig",
    "age" : 6,
    "role" : "Owsla"
  } ]
}
""")
```

and `Json.stringify` is used to convert a `JsValue` to a `String` of JSON:

```scala
val jsonString = Json.stringify(json)
// {"name":"Watership Down","location":{"lat":51.235685,"long":-1.309197},"residents":[{"name":"Fiver","age":4,"role":null},{"name":"Bigwig","age":6,"role":"Owsla"}]}
```

## Traversing a `JsValue`

Play JSON provides a traversal DSL that lets you query fields in the JSON:

### Simple path \
Applying the `\` operator will return the property corresponding to the field argument, supposing this is a JsObject.

```scala
val lat = (json \ "location" \ "lat").get
// returns JsNumber(51.235685)
```

The `(json \ "location" \ "lat")` returns a `JsLookupResult` which may or may not contain a value. Note that the `get` operation is not always safe; it throws an exception if the path doesn't exist.

You can also use `\` to look up indices within a `JsArray`:

```scala
val bigwig = (json \ "residents" \ 1).get
// returns {"name":"Bigwig","age":6,"role":"Owsla"}
```

### Recursive path `\\`
Applying the `\\` operator will do a lookup for the field in the current object and all descendants.

```scala
val names = json \\ "name"
// returns Seq(JsString("Watership Down"), JsString("Fiver"), JsString("Bigwig"))
```

### Index lookup
You can retrieve a value in a JsObject or JsArray using an apply operator with the index number or key.

```scala
val name = json("name")
// returns JsString("Watership Down")

val bigwig = json("residents")(1)
// returns {"name":"Bigwig","age":6,"role":"Owsla"}
```

Like `get`, this will throw an exception if the index doesn't exist. Use the Simple Path `\` operator and `validate` or `asOpt` (described below) if you expect that they key may not be present.

## Reading and writing objects

To convert a Scala object to and from JSON, we use `Json.toJson[T: Writes]` and `Json.fromJson[T: Reads]` respectively. Play JSON provides the `Reads` and `Writes` typeclasses to define how to read or write specific types. You can get these either by using Play's automatic JSON macros, or by manually defining them.

You can also read JSON from a `JsValue` using `validate`, `as` and `asOpt` methods. Generally it's preferable to use `validate` since it returns a `JsResult` which may contain an error if the JSON is malformed.

For example:
```scala
val unsafeName = (json \ "name").as[String]
// "Watership Down"

val unsafeBogusName = (json \ "bogus").as[String]
// throws exception

val nameOption = (json \ "name").asOpt[String]
// Some("Watership Down")

val bogusOption = (json \ "bogus").asOpt[String]
// None

val nameResult = (json \ "name").validate[String]
// JsSuccess("Watership Down")

val bogusResult = (json \ "bogus").validate[String]
// JsError

val unsafeName2 = json("name").as[String]
// "Watership Down"

val unsafeBogusName2 = json("bogus").as[String]
// throws exception

```

### Automatic conversion

Usually you don't need to traverse JSON AST directly. Play JSON comes equipped with some convenient macros to convert to and from case classes.

For example, suppose I have the following class:

```scala
case class Resident(name: String, age: Int, role: Option[String])
```

I can define a `Reads` (JSON parser), `Writes` (JSON writer) using convenient macros:

```scala
implicit val residentReads = Json.reads[Resident]
implicit val residentWrites = Json.writes[Resident]
```
I can also define a `Format` that does both:

```scala
implicit val residentFormat = Json.format[Resident]
```

With the `Reads` and/or `Writes` in scope, I can then easily convert my class using `toJson` and `fromJson`

### Constructing `Reads` and `Writes`

Play JSON provides a convenient functional DSL for constructing `Reads` and `Writes`. For example, assume I have the following classes:

```scala
case class Location(lat: Double, long: Double)
case class Resident(name: String, age: Int, role: Option[String])
case class Place(name: String, location: Location, residents: Seq[Resident])
```

Then I could construct `Reads` for them as follows:

```scala
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

implicit val locationReads: Reads[Location] = (
  (JsPath \ "lat").read[Double](min(-90.0) keepAnd max(90.0)) and
  (JsPath \ "long").read[Double](min(-180.0) keepAnd max(180.0))
)(Location.apply _)

implicit val residentReads: Reads[Resident] = (
  (JsPath \ "name").read[String](minLength[String](2)) and
  (JsPath \ "age").read[Int](min(0) keepAnd max(150)) and
  (JsPath \ "role").readNullable[String]
)(Resident.apply _)

implicit val placeReads: Reads[Place] = (
  (JsPath \ "name").read[String](minLength[String](2)) and
  (JsPath \ "location").read[Location] and
  (JsPath \ "residents").read[Seq[Resident]]
)(Place.apply _)


val json = { ... }

json.validate[Place] match {
  case s: JsSuccess[Place] => {
    val place: Place = s.get
    // do something with place
  }
  case e: JsError => {
    // error handling flow
  }
}
```

Similarly, I could construct `Writes` like this:

```scala
import play.api.libs.json._
import play.api.libs.functional.syntax._

implicit val locationWrites: Writes[Location] = (
  (JsPath \ "lat").write[Double] and
  (JsPath \ "long").write[Double]
)(location => {
  val Location(lat, long) = location
  (lat, long)
})

implicit val residentWrites: Writes[Resident] = (
  (JsPath \ "name").write[String] and
  (JsPath \ "age").write[Int] and
  (JsPath \ "role").writeNullable[String]
)(resident => {
  val Resident(name, age, role) = resident
  (name, age, role)
})

implicit val placeWrites: Writes[Place] = (
  (JsPath \ "name").write[String] and
  (JsPath \ "location").write[Location] and
  (JsPath \ "residents").write[Seq[Resident]]
)(place => {
  val Place(name, location, residents) = place
  (name, location, residents)
})


val place = Place(
  "Watership Down",
  Location(51.235685, -1.309197),
  Seq(
    Resident("Fiver", 4, None),
    Resident("Bigwig", 6, Some("Owsla"))
  )
)

val json = Json.toJson(place)
```

It is also possible to implement custom logic by implementing the `Reads`, `Writes` and/or `Format` traits manually, but we recommend using the automatic conversion macros or the functional DSL if possible.

### Manual JSON construction

JSON can also be manually constructed using a DSL:

```scala
val json: JsValue = Json.obj(
  "name" -> "Watership Down",
  "location" -> Json.obj("lat" -> 51.235685, "long" -> -1.309197),
  "residents" -> Json.arr(
    Json.obj(
      "name" -> "Fiver",
      "age" -> 4,
      "role" -> JsNull
    ),
    Json.obj(
      "name" -> "Bigwig",
      "age" -> 6,
      "role" -> "Owsla"
    )
  )
)
```

## Releasing a new version

See https://github.com/playframework/.github/blob/main/RELEASING.md

## License

Play JSON is licensed under the Apache license, version 2. See the LICENSE file for more information.
