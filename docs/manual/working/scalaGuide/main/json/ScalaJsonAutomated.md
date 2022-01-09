<!--- Copyright (C) Lightbend Inc. <https://www.lightbend.com> -->
# JSON automated mapping

If the JSON maps directly to a class, we provide a handy macro so that you don't have to write the `Reads[T]`, `Writes[T]`, or `Format[T]` manually. Given the following case class:

@[model](code/ScalaJsonAutomatedSpec.scala)

The following macro will create a `Reads[Resident]` based on its structure and the name of its fields:

@[auto-reads](code/ScalaJsonAutomatedSpec.scala)

When compiling, the macro will inspect the given class and
inject the following code, exactly as if you had written it manually:

@[manual-reads](code/ScalaJsonAutomatedSpec.scala)

This is done **at compile-time**, so you don't lose any type safety or performance.
Similar macros exists for a `Writes[T]` or a `Format[T]` :

@[auto-writes](code/ScalaJsonAutomatedSpec.scala)
@[auto-format](code/ScalaJsonAutomatedSpec.scala)

So, a complete example of performing automated conversion of a case class to JSON is as follows:

@[auto-case-class-to-JSON](code/ScalaJsonAutomatedSpec.scala)

And a complete example of automatically parsing JSON to a case class is:

@[auto-JSON-to-case-class](code/ScalaJsonAutomatedSpec.scala)

The [value classes](https://docs.scala-lang.org/overviews/core/value-classes.html) are also supported. Given the following value class, based on a `String` value:

@[valueClass](code-2/Scala2JsonAutomatedSpec.scala)

Then it's also possible to generate a `Reads[IdText]` using the following macro (as `String` is already supported):

@[value-reads](code-2/Scala2JsonAutomatedSpec.scala)

As for case classes, similar macros exists for a `Writes[T]` or a `Format[T]`:

@[value-writes](code-2/Scala2JsonAutomatedSpec.scala)
@[value-format](code-2/Scala2JsonAutomatedSpec.scala)

> Note: To be able to access JSON from `request.body.asJson`, the request must have a `Content-Type` header of `application/json`. You can relax this constraint by using the [[`tolerantJson` body parser|ScalaBodyParsers#Choosing-an-explicit-body-parser]].

The above example can be made even more concise by using body parsers with a typed validation function. See the [[savePlaceConcise example|ScalaJsonHttp#Creating-a-new-entity-instance-in-JSON]] in the JSON with HTTP documentation. 

### Requirements

The macros work for classes and traits meeting the following requirements.

**Class in Scala 2.x:**

- It must have a companion object having `apply` and `unapply` methods.
- The return types of the `unapply` must match the argument types of the `apply` method.
- The parameter names of the `apply` method must be the same as the property names desired in the JSON.

**Class in Scala 3.x:**

- It must be provided a [`Conversion`](https://dotty.epfl.ch/api/scala/Conversion.html) to a `_ <: Product`.
- It must be provided a valid [`ProductOf`](https://dotty.epfl.ch/api/scala/deriving/Mirror$.html#ProductOf-0).

Case classes automatically meet these requirements. For custom classes or traits, you might have to implement them.

A trait can also supported, if and only if it's a sealed one and if the sub-types comply with the previous requirements:

@[model3](code/ScalaJsonAutomatedSpec.scala)

The JSON representation for instances of a sealed family includes a discriminator field, which specify the effective sub-type (a text field, with default name `_type`).

@[trait-representation](code/ScalaJsonAutomatedSpec.scala)

Then the macros are able generate `Reads[T]`, `OWrites[T]` or `OFormat[T]`.

@[auto-JSON-sealed-trait](code/ScalaJsonAutomatedSpec.scala)

## Custom Naming Strategies

To use a custom Naming Strategy you need to define a implicit `JsonConfiguration` object and a `JsonNaming`.

Two naming strategies are provided: the default one, using as-is the names of the class properties,
and the `JsonNaming.SnakeCase` case one.

A strategy other than the default one can be used as following:

@[auto-naming-reads](code/ScalaJsonAutomatedSpec.scala)
@[auto-naming-writes](code/ScalaJsonAutomatedSpec.scala)
@[auto-naming-format](code/ScalaJsonAutomatedSpec.scala)

The trait representation can also be configured, with a custom name for the discriminator field or the way the names of the sub-types are encoded as value for this field:

@[trait-custom-representation](code/ScalaJsonAutomatedSpec.scala)

To do so, the settings `discriminator` and `typeNaming` can be defined in the resolved `JsonConfiguration`:

@[auto-JSON-custom-trait](code/ScalaJsonAutomatedSpec.scala)

### Implementing your own Naming Strategy

To implement your own Naming Strategy you just need to implement the `JsonNaming` trait:

@[auto-custom-naming-format](code/ScalaJsonAutomatedSpec.scala)

## Customize the macro to output null

The macro can be configured to output `null` values in the Json instead of removing the empty fields:

@[auto-writes-null](code/ScalaJsonAutomatedSpec.scala)
