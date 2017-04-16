# 1

## e_mail: String ?

type = set of values
func = map from domain to codomain (deterministic)
map.apply - throws - not deterministic
handle everything in codomain and always return the same value for the same input

ADT = composition of (product|sum)

Address = ProductType[Street, City] // And/Both

Either = SumTypes[Left, Right] // Or

## State monad

 * functions.scala
 * composed of functions
 * Parser domain String (product type)
 * Parser codomain Either (sum type = Product(product + A?))
 * no polymorphic functions in Scala => object { apply() }
 * def foo[A](a: A) // 2 parameter list: type parameter + value parameter

##  Higher-Kinded Types

 * values are members of sets, what about types?
 * Int: Types
 * List[A]: TypeConstructor=type level functions (domain and codomain is set of types)
 * List.apply(Type) => List[Type] // List.apply
 * TypeConstructors have kinds (not types)
 * star kind is set of Types
 * star => star kind of type level functions that accepts a type and return a type (domain and codomain are set of types)
 * codomain is product of types then [star, star] => star (e.g. Map[A,B])
 * Kinds are signatures of type level functions
 * Higher order functions analogy in Kinds is Functor which accepts

## Type Lambdas

 * partially apply functions (_ + 1) => partially apply type constructors (!)

## Monad

 * describes run time dependent computation
 
## IO Monad

## Free Monad

