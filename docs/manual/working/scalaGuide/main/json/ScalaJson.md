<!--- Copyright (C) 2009-2017 Lightbend Inc. <https://www.lightbend.com> -->
# JSON basics

Modern web applications often need to parse and generate data in the JSON (JavaScript Object Notation) format. Play supports this via its [JSON library](api/scala/play/api/libs/json/package.html).

JSON is a lightweight data-interchange format and looks like this:

```json
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
```

> To learn more about JSON, see [json.org](http://json.org/).

## The Play JSON library

The [`play.api.libs.json`](api/scala/play/api/libs/json/package.html) package contains data structures for representing JSON data and utilities for converting between these data structures and other data representations. Some of the features of this package are:

 - [[Automatic conversion|ScalaJsonAutomated]] to and from case classes with minimal boilerplate. If you want to get up and running quickly with minimal code, this is probably the place to start.
 - [[Custom validation|ScalaJsonCombinators#Validation-with-Reads]] while parsing.
 - [[Automatic parsing|ScalaBodyParsers#The-default-body-parser]] of JSON in request bodies, with auto-generated errors if content isn't parseable or incorrect Content-type headers are supplied.
 - Can be used outside of a Play application as a standalone library. Just add `libraryDependencies += "com.typesafe.play" %% "play-json" % playVersion` to your `build.sbt` file.
 - Highly customizable.

The package provides the following types:

### [`JsValue`](api/scala/play/api/libs/json/JsValue.html)

This is a trait representing any JSON value. The JSON library has a case class extending `JsValue` to represent each valid JSON type:

- [`JsString`](api/scala/play/api/libs/json/JsString.html)
- [`JsNumber`](api/scala/play/api/libs/json/JsNumber.html)
- [`JsBoolean`](api/scala/play/api/libs/json/JsBoolean.html)
- [`JsObject`](api/scala/play/api/libs/json/JsObject.html)
- [`JsArray`](api/scala/play/api/libs/json/JsArray.html)
- [`JsNull`](api/scala/play/api/libs/json/JsNull$.html)

Using the various `JsValue` types, you can construct a representation of any JSON structure.

### [`Json`](api/scala/play/api/libs/json/Json$.html)

The `Json` object provides utilities, primarily for conversion to and from `JsValue` structures.

### [`JsPath`](api/scala/play/api/libs/json/JsPath.html)

Represents a path into a `JsValue` structure, analogous to XPath for XML. This is used for traversing `JsValue` structures and in patterns for implicit converters.

## Converting to a `JsValue`

### Using string parsing

@[convert-from-string](code/ScalaJsonSpec.scala)

### Using class construction

@[convert-from-classes](code/ScalaJsonSpec.scala)

`Json.obj` and `Json.arr` can simplify construction a bit. Note that most values don't need to be explicitly wrapped by JsValue classes, the factory methods use implicit conversion (more on this below).

@[convert-from-factory](code/ScalaJsonSpec.scala)

### Using Writes converters

Scala to `JsValue` conversion is performed by the utility method `Json.toJson[T](T)(implicit writes: Writes[T])`. This functionality depends on a converter of type [`Writes[T]`](api/scala/play/api/libs/json/Writes.html) which can convert a `T` to a `JsValue`.

The Play JSON API provides implicit `Writes` for most basic types, such as `Int`, `Double`, `String`, and `Boolean`. It also supports `Writes` for collections of any type `T` that a `Writes[T]` exists.

@[convert-from-simple](code/ScalaJsonSpec.scala)

To convert your own models to `JsValue`s, you must define implicit `Writes` converters and provide them in scope.

@[sample-model](code/ScalaJsonSpec.scala)

@[convert-from-model](code/ScalaJsonSpec.scala)

Alternatively, you can define your `Writes` using the combinator pattern:

> Note: The combinator pattern is covered in detail in [[JSON Reads/Writes/Formats Combinators|ScalaJsonCombinators]].

@[convert-from-model-prefwrites](code/ScalaJsonSpec.scala)

## Traversing a JsValue structure

You can traverse a `JsValue` structure and extract specific values. The syntax and functionality is similar to Scala XML processing.

> Note: The following examples are applied to the JsValue structure created in previous examples.

### Simple path `\`

Applying the `\` operator to a `JsValue` will return the property corresponding to the field argument in a `JsObject`, or the item at that index in a `JsArray`

@[traverse-simple-path](code/ScalaJsonSpec.scala)


The `\` operator returns a `JsLookupResult`, which is either `JsDefined` or `JsUndefined`. You can chain multiple `\` operators, and the result will be `JsUndefined` if any intermediate value cannot be found. Calling `get` on a `JsLookupResult` attempts to get the value if it is defined and throws an exception if it is not.

You can also use the Direct lookup `apply` method (below) to get a field in an object or index in an array. Like `get`, this method will throw an exception if the value does not exist.

### Recursive path `\\`

Applying the `\\` operator will do a lookup for the field in the current object and all descendants.

@[traverse-recursive-path](code/ScalaJsonSpec.scala)

### Direct lookup

You can retrieve a value in a `JsArray` or `JsObject` using an `.apply` operator, which is identical to the Simple path `\` operator except it returns the value directly (rather than wrapping it in a `JsLookupResult`) and throws an exception if the index or key is not found:

@[traverse-array-index](code/ScalaJsonSpec.scala)

This is useful if you are writing quick-and-dirty code and are accessing some JSON values you *know* to exist, for example in one-off scripts or in the REPL.

## Converting from a JsValue

### Using String utilities

Minified:

@[convert-to-string](code/ScalaJsonSpec.scala)

```json
{"name":"Watership Down","location":{"lat":51.235685,"long":-1.309197},"residents":[{"name":"Fiver","age":4,"role":null},{"name":"Bigwig","age":6,"role":"Owsla"}]}
```

Readable:

@[convert-to-string-pretty](code/ScalaJsonSpec.scala)

```json
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
```

### Using JsValue.as/asOpt

The simplest way to convert a `JsValue` to another type is using `JsValue.as[T](implicit fjs: Reads[T]): T`. This requires an implicit converter of type [`Reads[T]`](api/scala/play/api/libs/json/Reads.html) to convert a `JsValue` to `T` (the inverse of `Writes[T]`). As with `Writes`, the JSON API provides `Reads` for basic types.

@[convert-to-type-as](code/ScalaJsonSpec.scala)

The `as` method will throw a `JsResultException` if the path is not found or the conversion is not possible. A safer method is `JsValue.asOpt[T](implicit fjs: Reads[T]): Option[T]`.

@[convert-to-type-as-opt](code/ScalaJsonSpec.scala)

Although the `asOpt` method is safer, any error information is lost.

### Using validation

The preferred way to convert from a `JsValue` to another type is by using its `validate` method (which takes an argument of type `Reads`). This performs both validation and conversion, returning a type of [`JsResult`](api/scala/play/api/libs/json/JsResult.html). `JsResult` is implemented by two classes:

- [`JsSuccess`](api/scala/play/api/libs/json/JsSuccess.html): Represents a successful validation/conversion and wraps the result.
- [`JsError`](api/scala/play/api/libs/json/JsError.html): Represents unsuccessful validation/conversion and contains a list of validation errors.

You can apply various patterns for handling a validation result:

@[convert-to-type-validate](code/ScalaJsonSpec.scala)

### JsValue to a model

To convert from JsValue to a model, you must define implicit `Reads[T]` where `T` is the type of your model.

> Note: The pattern used to implement `Reads` and custom validation are covered in detail in [[JSON Reads/Writes/Formats Combinators|ScalaJsonCombinators]].

@[sample-model](code/ScalaJsonSpec.scala)

@[convert-to-model](code/ScalaJsonSpec.scala)
