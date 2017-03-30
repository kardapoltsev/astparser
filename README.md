# astparser [![Build Status](https://travis-ci.org/kardapoltsev/astparser.svg?branch=master)](https://travis-ci.org/kardapoltsev/astparser) [![Coverage Status](https://coveralls.io/repos/github/kardapoltsev/astparser/badge.svg)](https://coveralls.io/github/kardapoltsev/astparser)

Schema parser for different code generators


## DSL description

### Type
Defines a model.
`Animal` is called `Type`, `Cat`and `Dog` are called classes in this documentation
```
type Animal {

  cat

  dog ::
   name: String

}
```
Example above is equivalent to:
```scala
abstract class Animal
class Cat extends Animal
class Dog(name: String) extends Animal
```


### Call
Defines a API request definition
```
call requestName ::
  param1: Int
  => Long
```
It's equivalent to a request which returns `Long` as a result
```scala
class RequestName(param1: Int)
```

### Trait
Defines a trait (mixin). `<:` is used to specify traits for call or type
```
trait TraitName1
trait TraitName2

call CallName <: TraitName1 TraitName2 ::
  param1: Int
  => Int

```

### Type alias
Defines a type alias
```
type AliasName = Int
```

### External type
Defines a predefined schema type like Int, Long, String etc
```
external type Int
```

### Package
Defines a package (scope of visibility)
```
package packageName {
}
```

### Import
Defines an import
```
package a {
  type X {
    x
  }
}
package b {
  import a.x
  call Y ::
    param1: x
  => Int
}
```
### Supported features
#### Inheritance
`Type`, `Call` and class could extend any number of `Trait`s.
Any class inherit traits from the enclosing `Type`
```
trait T1
trait T2
type Type1 <: T1 {
  className <: T2
}
```
#### References
`package1.package2.TypeName` is called reference.
Both absolute and relative reference are supported
```
package p1 {
  package p2 {
    type T1 {
      t1
    }
    type T2 {
      t2
    }
    type T3 {
      t3
    }
    type T4 {
      t4
    }
  }
  package p3 {
    import p2.T1
    import p1.p2.T2
    type AliasT4 = p2.T4

    call C1 ::
      param1: T1
      param2: T2
      param3: p1.p3.T3
      param4: AliasT4
    => Int

  }
}
```
#### Documentation
Documentation could include references to another schema elements.
` `` ` is used for it.
```
/** Docs for type with a link to some other `T2` */
type T1 {
  /** Docs for class
      with a multipe lines
   */
  t1 ::
    field1: Int -- docs for field1
}
```
#### Versioning
Any class or `Call` may refer to a specific API version.
`(1-3)` means that c1 is available from the 1st to the 3rd versions.
`(1-)` means that c1 is available from the 1st to the latest versions
`(-3)` means that call is available from the oldest to the 3rd versions
```
/** Parameterless call */
call c1 (1-3)
  => Int
```
#### Quotation
Any schema keyword may be used as a `Type` or parameter name. 
Use ``` `type`: Int ``` for it.
#### Http definiton
Call may have an http definition.
- `param1` is a path parameter
- `param2` is a query parameter
- `param3` is a body parameter

Supported HTTP methods are `GET`, `POST`, `PUT`, `PATCH`, `DELETE`
```
@POST /api/{param1}?{param2}
call C1 <: Request ::
  param1 : Int
  param2 : Option[Long]
  param3 : String
  => String
```
