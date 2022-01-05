/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package scalaguide.json

import org.specs2.mutable.Specification

class ScalaJsonCombinatorsSpec extends Specification {
  val sampleJson = {
    //#sample-json
    import play.api.libs.json._

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
    //#sample-json
    json
  }

  object SampleModel {
    //#sample-model
    case class Location(lat: Double, long: Double)
    case class Resident(name: String, age: Int, role: Option[String])
    case class Place(name: String, location: Location, residents: Seq[Resident])
    //#sample-model
  }

  "Scala JSON" should {
    "allow using JsPath" in {
      //#jspath-define
      import play.api.libs.json._

      //###replace: val json = { ... }
      val json: JsValue = sampleJson

      // Simple path
      val latPath = JsPath \ "location" \ "lat"

      // Recursive path
      val namesPath = JsPath \\ "name"

      // Indexed path
      val firstResidentPath = (JsPath \ "residents")(0)
      //#jspath-define

      //#jspath-define-alias
      val longPath = __ \ "location" \ "long"
      //#jspath-define-alias

      //#jspath-traverse
      val lat: List[JsValue] = latPath(json)
      // List(JsNumber(51.235685))
      //#jspath-traverse

      // val name = (JsPath \ "name").read[String] and (JsPath \ "location").read[Int]
      longPath.toString === "/location/long" and {
        latPath.toString === "/location/lat"
      } and {
        namesPath.toString === "//name"
      } and {
        firstResidentPath.toString === "/residents(0)"
      } and {
        lat.must(contain(exactly[JsValue](JsNumber(51.235685D))))
      }
    }

    "allow creating simple Reads" in {
      //#reads-imports
      import play.api.libs.json._       // JSON library
      import play.api.libs.json.Reads._ // Custom validation helpers
      //#reads-imports

      //###replace: val json = { ... }
      val json: JsValue = sampleJson

      //#reads-simple
      val nameReads: Reads[String] = (JsPath \ "name").read[String]
      //#reads-simple

      json
        .validate(nameReads)
        .must(beLike {
          case JsSuccess(v, _) =>
            v must_=== "Watership Down"
        })
    }

    "allow creating complex Reads" in {
      import SampleModel._

      import play.api.libs.json._

      //###replace: val json = { ... }
      val json: JsValue = sampleJson

      //#reads-complex-builder
      import play.api.libs.functional.syntax._ // Combinator syntax

      val locationReadsBuilder =
        (JsPath \ "lat").read[Double] and
          (JsPath \ "long").read[Double]
      //#reads-complex-builder

      //#reads-complex-buildertoreads
      implicit val locationReads: Reads[Location] = locationReadsBuilder.apply(Location.apply _)
      //#reads-complex-buildertoreads

      val locationResult = (json \ "location").validate[Location]
      locationResult.must(beLike { case JsSuccess(l: Location, _) => l.lat === 51.235685 })
    }

    "allow creating complex Reads in a single statement" in {
      import SampleModel._

      import play.api.libs.json._
      import play.api.libs.functional.syntax._

      //###replace: val json = { ... }
      val json: JsValue = sampleJson

      //#reads-complex-statement
      implicit val locationReads: Reads[Location] = (
        (JsPath \ "lat").read[Double] and
          (JsPath \ "long").read[Double]
      )(Location.apply _)
      //#reads-complex-statement

      val locationResult = (json \ "location").validate[Location]
      locationResult.must(beLike { case JsSuccess(l: Location, _) => l.lat === 51.235685 })
    }

    "allow mapping Reads using usual combinators" in {
      import play.api.libs.json._

      //#reads-usual-combinators
      val strReads: Reads[String] = JsPath.read[String]

      // .map
      val intReads: Reads[Int] = strReads.map { str => str.toInt }
      // e.g. reads JsString("123") as 123

      // .flatMap
      val objReads: Reads[JsObject] = strReads.flatMap { rawJson =>
        // consider something like { "foo": "{ \"stringified\": \"json\" }" }
        Reads { _ => Json.parse(rawJson).validate[JsObject] }
      }

      // .collect
      val boolReads1: Reads[Boolean] = strReads.collect(JsonValidationError("in.case.it.doesn-t.match")) {
        case "no" | "false" | "n" => false
        case _                    => true
      }

      // .orElse
      val boolReads2: Reads[Boolean] = JsPath.read[Boolean].orElse(boolReads1)

      // .andThen
      val postprocessing: Reads[Boolean] = Reads[JsBoolean] {
        case JsString("no" | "false" | "n") =>
          JsSuccess(JsFalse)

        case _ => JsSuccess(JsTrue)
      }.andThen(JsPath.read[Boolean])
      //#reads-usual-combinators

      intReads.reads(JsString("123")) must_=== JsSuccess(123) and {
        objReads.reads(JsString("{\"foo\":1}")) must_=== JsSuccess(
          Json.obj(
            "foo" -> 1
          )
        )
      } and {
        boolReads1.reads(JsString("no")) must_=== JsSuccess(false) and {
          boolReads1.reads(JsString("yes")) must_=== JsSuccess(true)
        }
      } and {
        boolReads2.reads(JsBoolean(true)) must_=== JsSuccess(true) and {
          boolReads2.reads(JsString("n")) must_=== JsSuccess(false)
        }
      } and {
        postprocessing.reads(JsString("false")) must_=== JsSuccess(false)
      }
    }

    "allow filtering Reads results with combinators" in {
      import play.api.libs.json._

      //#reads-filter-combinators
      val positiveIntReads = JsPath.read[Int].filter(_ > 0)
      val smallIntReads    = positiveIntReads.filterNot(_ > 100)

      val positiveIntReadsWithCustomErr = JsPath
        .read[Int]
        .filter(JsonValidationError("error.positive-int.expected"))(_ > 0)
      //#reads-filter-combinators

      positiveIntReads.reads(JsNumber(10)) must_=== JsSuccess(10) and {
        positiveIntReads.reads(JsNumber(-20)).must(beAnInstanceOf[JsError])
      } and {
        smallIntReads.reads(JsNumber(20)) must_=== JsSuccess(20)
      } and {
        smallIntReads.reads(JsNumber(200)).must(beAnInstanceOf[JsError])
      } and {
        positiveIntReadsWithCustomErr.reads(JsNumber(-2)) must_=== JsError("error.positive-int.expected")
      }
    }

    "allow pre-processing Reads with combinators" in {
      import play.api.libs.json._

      //#reads-preprocessing-combinators
      // .composeWith
      val preprocessing1: Reads[Boolean] =
        JsPath
          .read[Boolean]
          .composeWith(Reads[JsBoolean] {
            case JsString("no" | "false" | "n") =>
              JsSuccess(JsFalse)

            case _ => JsSuccess(JsTrue)
          })

      val preprocessing2: Reads[Boolean] = JsPath.read[Boolean].preprocess {
        case JsString("no" | "false" | "n") =>
          JsFalse

        case _ => JsTrue
      }
      //#reads-preprocessing-combinators

      preprocessing1.reads(JsString("false")) must_=== JsSuccess(false) and {
        preprocessing2.reads(JsString("false")) must_=== JsSuccess(false)
      }
    }

    "allow mapping Reads with convenience combinators" in {
      import play.api.libs.json._

      //#reads-preprocessing-combinators
      val strReads: Reads[String] = JsPath.read[String]

      // .flatMapResult (see .flatMap)
      val objReads: Reads[JsObject] = strReads.flatMapResult { rawJson =>
        // consider something like { "foo": "{ \"stringified\": \"json\" }" }
        Json.parse(rawJson).validate[JsObject]
      }

      // .widen (as List <: Seq)
      val seqReads: Reads[Seq[String]] =
        JsPath.read[List[String]].widen[Seq[String]]
      //#reads-preprocessing-combinators

      objReads.reads(JsString("{\"foo\":1}")) must_=== JsSuccess(
        Json.obj(
          "foo" -> 1
        )
      ) and {
        seqReads.reads(JsArray(Seq(JsString("foo"), JsString("bar")))) must_=== JsSuccess(Seq("foo", "bar"))
      }
    }

    "allow validation with Reads" in {
      import play.api.libs.json._
      import play.api.libs.json.Reads._

      //#reads-validation-simple
      //###replace: val json = { ... }
      val json: JsValue = sampleJson

      val nameReads: Reads[String] = (JsPath \ "name").read[String]

      val nameResult: JsResult[String] = json.validate[String](nameReads)

      nameResult match {
        case JsSuccess(nme, _) => println(s"Name: $nme")
        case e: JsError        => println(s"Errors: ${JsError.toJson(e)}")
      }
      //#reads-validation-simple
      nameResult.must(beLike { case x: JsSuccess[String] => x.get === "Watership Down" })

      //#reads-validation-custom
      val improvedNameReads =
        (JsPath \ "name").read[String](minLength[String](2))
      //#reads-validation-custom
      json.validate[String](improvedNameReads).must(beLike { case x: JsSuccess[String] => x.get === "Watership Down" })
    }

    "allow creating Reads for model" in {
      import SampleModel._

      //#reads-model
      import play.api.libs.json._
      import play.api.libs.json.Reads._
      import play.api.libs.functional.syntax._

      implicit val locationReads: Reads[Location] = (
        (JsPath \ "lat").read[Double](min(-90.0).keepAnd(max(90.0))) and
          (JsPath \ "long").read[Double](min(-180.0).keepAnd(max(180.0)))
      )(Location.apply _)

      implicit val residentReads: Reads[Resident] = (
        (JsPath \ "name").read[String](minLength[String](2)) and
          (JsPath \ "age").read[Int](min(0).keepAnd(max(150))) and
          (JsPath \ "role").readNullable[String]
      )(Resident.apply _)

      implicit val placeReads: Reads[Place] = (
        (JsPath \ "name").read[String](minLength[String](2)) and
          (JsPath \ "location").read[Location] and
          (JsPath \ "residents").read[Seq[Resident]]
      )(Place.apply _)

      //###replace: val json = { ... }
      val json: JsValue = sampleJson

      json.validate[Place] match {
        case JsSuccess(place, _) => {
          val _: Place = place
          // do something with place
        }
        case e: JsError => {
          // error handling flow
        }
      }
      //#reads-model

      json.validate[Place].must(beLike { case JsSuccess(p: Place, _) => p.name === "Watership Down" })
    }

    "allow creating Writes for model" in {
      import SampleModel._

      //#writes-model
      import play.api.libs.json._
      import play.api.libs.functional.syntax._

      implicit val locationWrites: Writes[Location] = (
        (JsPath \ "lat").write[Double] and
          (JsPath \ "long").write[Double]
      )(l => (l.lat, l.long))

      implicit val residentWrites: Writes[Resident] = (
        (JsPath \ "name").write[String] and
          (JsPath \ "age").write[Int] and
          (JsPath \ "role").writeNullable[String]
      )(r => (r.name, r.age, r.role))

      implicit val placeWrites: Writes[Place] = (
        (JsPath \ "name").write[String] and
          (JsPath \ "location").write[Location] and
          (JsPath \ "residents").write[Seq[Resident]]
      )(p => (p.name, p.location, p.residents))

      val place = Place(
        "Watership Down",
        Location(51.235685, -1.309197),
        Seq(
          Resident("Fiver", 4, None),
          Resident("Bigwig", 6, Some("Owsla"))
        )
      )

      val json = Json.toJson(place)
      //#writes-model

      (json \ "name").get must_=== JsString("Watership Down")
    }

    "allow transforming values with Writes" in {
      import play.api.libs.json._

      //#writes-combinators
      val plus10Writes: Writes[Int] = implicitly[Writes[Int]].contramap(_ + 10)

      val doubleAsObj: Writes[Double] =
        implicitly[Writes[Double]].transform { js => Json.obj("_double" -> js) }

      val someWrites: Writes[Some[String]] =
        implicitly[Writes[Option[String]]].narrow[Some[String]]
      //#writes-combinators

      plus10Writes.writes(2) must_=== JsNumber(12) and {
        doubleAsObj.writes(1.23D) must_=== Json.obj("_double" -> 1.23)
      } and {
        someWrites.writes(Some("foo")) must_=== JsString("foo")
      }
    }

    "allow creating Reads/Writes for recursive types" in {
      import play.api.libs.json._
      import play.api.libs.json.Reads._
      import play.api.libs.functional.syntax._

      //#reads-writes-recursive
      case class User(name: String, friends: Seq[User])

      implicit
      lazy val userReads: Reads[User] = (
        (__ \ "name").read[String] and
          (__ \ "friends").lazyRead(Reads.seq[User](userReads))
      )(User.apply _)

      implicit lazy val userWrites: Writes[User] = (
        (__ \ "name").write[String] and
          (__ \ "friends").lazyWrite(Writes.seq[User](userWrites))
      )(u => (u.name, u.friends))
      //#reads-writes-recursive

      // Use Reads for JSON -> model
      val json: JsValue = Json.parse("""
      {
        "name" : "Fiver",
        "friends" : [ {
          "name" : "Bigwig",
          "friends" : []
        }, {
          "name" : "Hazel",
          "friends" : []
        } ]
      }
      """)
      val userResult = json.validate[User]
      userResult.must(beLike { case JsSuccess(u: User, _) => u.name === "Fiver" })

      // Use Writes for model -> JSON
      val jsonFromUser = Json.toJson(userResult.get)
      (jsonFromUser \ "name").as[String] === "Fiver"
    }

    "allow creating Format from components" in {
      import SampleModel._

      import play.api.libs.json._
      import play.api.libs.json.Reads._
      import play.api.libs.functional.syntax._

      //#format-components
      val locationReads: Reads[Location] = (
        (JsPath \ "lat").read[Double](min(-90.0).keepAnd(max(90.0))) and
          (JsPath \ "long").read[Double](min(-180.0).keepAnd(max(180.0)))
      )(Location.apply _)

      val locationWrites: Writes[Location] = (
        (JsPath \ "lat").write[Double] and
          (JsPath \ "long").write[Double]
      )(l => (l.lat, l.long))

      implicit val locationFormat: Format[Location] =
        Format(locationReads, locationWrites)
      //#format-components

      // Use Reads for JSON -> model
      val json: JsValue = Json.parse("""
      {
        "lat" : 51.235685,
        "long" : -1.309197
      }
      """)
      val location = json.validate[Location].get
      location === Location(51.235685, -1.309197)

      // Use Writes for model -> JSON
      val jsonFromLocation = Json.toJson(location)
      (jsonFromLocation \ "lat").as[Double] === 51.235685
    }

    "allow creating Format from combinators" in {
      import SampleModel._

      import play.api.libs.json._
      import play.api.libs.json.Reads._
      import play.api.libs.functional.syntax._

      //#format-combinators
      implicit val locationFormat: Format[Location] = (
        (JsPath \ "lat").format[Double](min(-90.0).keepAnd(max(90.0))) and
          (JsPath \ "long").format[Double](min(-180.0).keepAnd(max(180.0)))
      )(Location.apply, l => (l.lat, l.long))
      //#format-combinators

      // Use Reads for JSON -> model
      val json: JsValue = Json.parse("""
      {
        "lat" : 51.235685,
        "long" : -1.309197
      }
      """)
      val location = json.validate[Location].get
      location must_=== Location(51.235685, -1.309197)

      // Use Writes for model -> JSON
      val jsonFromLocation = Json.toJson(location)
      (jsonFromLocation \ "lat").asOpt[Double].must(beSome(51.235685))
    }

    "allow functional combinators on Format" in {
      import play.api.libs.json._

      //#format-functional-combinators
      val strFormat = implicitly[Format[String]]
      val intFormat: Format[Int] =
        strFormat.bimap(_.size, List.fill(_: Int)('?').mkString)
      //#format-functional-combinators

      intFormat.reads(JsString("foo")) must_=== JsSuccess(3) and {
        intFormat.writes(5) must_=== JsString("?????")
      }
    }
  }
}
