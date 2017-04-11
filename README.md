# Learning Smalltalk

## **Allowable Characters**

- a - z
- A - Z
- 0 - 9
- . + / \ * ~ < > @ % | & ?
- bland, tab, cr, ff, lf

## **Variables**

- Variables must be declared before use.
- Shared variables must begin with uppercase.
- Local variables must begin with lowercase.
- Reserverd names: `nil`, `true`, `false`, `self`,`super`, and `Smalltalk`.

### **Variable Scope:**

- **Global:** defined in `Dictionary Smalltalk` and accessible by all objects in system.
- **Special:** (reserved) `Smalltalk`, `super`, `self`, `true`, `false`, and `nil`.
- **Method Temporary:** local to a method.
- **Block Temporary:** local to a block
- **Pool:** variables in a Dictionary object
- **Method Parameters:** automatic local vars created as a result of  message call with params.
- **Block Parameters:** automatic local vars created as a result of value: message call.
- **Class:** shared with all instances of one class and its subclasses.
- **Class Instance:** unique to each instance of a class.
- **Instance Variables:** unique to each instance.

## **Comments**

```smaltalk
"Comments are enclosed in quotes"
```



## **Statement Separator `.`**

The period `.` is the statement separator.



## **Transcript**

```smalltalk
Transcript clear.                                    "clear to transcript window"
Transcript show: 'Hello World'.                      "output string in transcript window"
Transcript nextPutAll: 'Hello World'.                "output string in transcript window"
Transcript nextPut: $A.                              "output character in transcript window"
Transcript space.                                    "output space character in transcript window"
Transcript tab.                                      "output tab character in transcript window"
Transcript cr.                                       "carriage return / linefeed"
'Hello' printOn: Transcript.                         "append print string into the window"
'Hello' storeOn: Transcript.                         "append store string into the window"
Transcript endEntry.                                 "flush the output buffer"
```

## **Assignment**

```smalltalk
| x y |
x _ 4.                                                "assignment (Squeak) <-"
x := 5.                                               "assignment"
x := y := z := 6.                                     "compound assignment"
x := (y := 6) + 1.
x := Object new.                                      "bind to allocated instance of a class"
x := 123 class.                                       "discover the object class"
x := Integer superclass.                              "discover the superclass of a class"
x := Object allInstances.                             "get an array of all instances of a class"
x := Integer allSuperclasses.                         "get all superclasses of a class"
x := 1.2 hash.                                        "hash value for object"
y := x copy.                                          "copy object"
y := x shallowCopy.                                   "copy object (not overridden)"
y := x deepCopy.                                      "copy object and instance vars"
y := x veryDeepCopy.                                  "complete tree copy using a dictionary"
```

## **Booleans**

```smalltalk
| b x y |
x := 1. y := 2.
b := (x = y).                                          "equals"
b := (x ~= y).                                         "not equals"
b := (x == y).                                         "identical"
b := (x ~~ y).                                         "not identical"
b := (x > y).                                          "greater than"
b := (x < y).                                          "less than"
b := (x >= y).                                         "greater than or equal"
b := (x <= y).                                         "less than or equal"
b := b not.                                            "boolean not"
b := (x < 5) & (y > 1).                                "boolean and"
b := (x < 5) | (y > 1).                                "boolean or"
b := (x < 5) and: [y > 1].                             "boolean and (short-circuit)"
b := (x < 5) or: [y > 1].                              "boolean or (short-circuit)"
b := (x < 5) eqv: (y > 1).                             "test if both true or both false"
b := (x < 5) xor: (y > 1).                             "test if one true and other false"
b := 5 between: 3 and: 12.                             "between (inclusive)"
b := 123 isKindOf: Number.                             "test if object is class or subclass of"
b := 123 isMemberOf: SmallInteger.                     "test if object is type of class"
b := 123 respondsTo: sqrt.                             "test if object responds to message"
b := x isNil.                                          "test if object is nil"
b := x isZero.                                         "test if number is zero"
b := x positive.                                       "test if number is positive"
b := x strictlyPositive.                               "test if number is greater than zero"
b := x negative.                                       "test if number is negative"
b := x even.                                           "test if number is even"
b := x odd.                                            "test if number is odd"
b := x isLiteral.                                      "test if literal constant"
b := x isInteger.                                      "test if object is integer"
b := x isFloat.                                        "test if object is float"
b := x isNumber.                                       "test if object is number"
b := $A isUppercase.                                   "test if upper case character"
b := $A isLowercase.                                   "test if lower case character"
```

## **Arithmetic Expressions**

```smalltalk
| x |
x := 6 + 3.                                          "addition"
x := 6 - 3.                                          "subtraction"
x := 6 * 3.                                          "multiplication"
x := 1 + 2 * 3.                                      "evaluation always left to right (1 + 2) * 3"
x := 5 / 3.                                          "division with fractional result"
x := 5.0 / 3.0.                                      "division with float result"
x := 5.0 // 3.0.                                     "integer divide"
x := 5.0 \\ 3.0.                                     "integer remainder"
x := -5.                                             "unary minus"
x := 5 sign.                                         "numeric sign (1, -1 or 0)"
x := 5 negated.                                      "negate receiver"
x := 1.2 integerPart.                                "integer part of number (1.0)"
x := 1.2 fractionPart.                               "fractional part of number (0.2)"
x := 5 reciprocal.                                   "reciprocal function"
x := 6 * 3.1.                                        "auto convert to float"
x := 5 squared.                                      "square function"
x := 25 sqrt.                                        "square root"
x := 5 raisedTo: 2.                                  "power function"
x := 5 raisedToInteger: 2.                           "power function with integer"
x := 5 exp.                                          "exponential"
x := -5 abs.                                         "absolute value"
x := 3.99 rounded.                                   "round"
x := 3.99 truncated.                                 "truncate"
x := 3.99 roundTo: 1.                                "round to specified decimal places"
x := 3.99 truncateTo: 1.                             "truncate to specified decimal places"
x := 3.99 floor.                                     "truncate"
x := 3.99 ceiling.                                   "round up"
x := 5 factorial.                                    "factorial"
x := -5 quo: 3.                                      "integer divide rounded toward zero"
x := -5 rem: 3.                                      "integer remainder rounded toward zero"
x := 28 gcd: 12.                                     "greatest common denominator"
x := 28 lcm: 12.                                     "least common multiple"
x := 100 ln.                                         "natural logarithm"
x := 100 log.                                        "base 10 logarithm"
x := 100 log: 10.                                    "logarithm with specified base"
x := 100 floorLog: 10.                               "floor of the log"
x := 180 degreesToRadians.                           "convert degrees to radians"
x := 3.14 radiansToDegrees.                          "convert radians to degrees"
x := 0.7 sin.                                        "sine"
x := 0.7 cos.                                        "cosine"
x := 0.7 tan.                                        "tangent"
x := 0.7 arcSin.                                     "arcsine"
x := 0.7 arcCos.                                     "arccosine"
x := 0.7 arcTan.                                     "arctangent"
x := 10 max: 20.                                     "get maximum of two numbers"
x := 10 min: 20.                                     "get minimum of two numbers"
x := Float pi.                                       "pi"
x := Float e.                                        "exp constant"
x := Float infinity.                                 "infinity"
x := Float nan.                                      "not-a-number"
x := Random new next; yourself. x next.              "random number stream (0.0 to 1.0)"
x := 100 atRandom.                                   "quick random number"
```

## **Bitwise Manipulation**

```smalltalk
| b x |
x := 16rFF bitAnd: 16r0F.                         "and bits"
x := 16rF0 bitOr: 16r0F.                          "or bits"
x := 16rFF bitXor: 16r0F.                         "xor bits"
x := 16rFF bitInvert.                             "invert bits"
x := 16r0F bitShift: 4.                           "left shift"
x := 16rF0 bitShift: -4.                          "right shift"
x := 16r80 bitAt: 7.                              "bit at position (0|1) [!Squeak]"
x := 16r80 highbit.                               "position of highest bit set"
b := 16rFF allMask: 16r0F.                        "test if all bits set in mask set in receiver"
b := 16rFF anyMask: 16r0F.                        "test if any bits set in mask set in receiver"
b := 16rFF noMask: 16r0F.                         "test if all bits set in mask clear in receiver"
```

## **Conversion**

```smalltalk
| x |
x := 3.99 asInteger.                             "convert number to integer (truncates in Squeak)"
x := 3.99 asFraction.                            "convert number to fraction"
x := 3 asFloat.                                  "convert number to float"
x := 65 asCharacter.                             "convert integer to character"
x := $A asciiValue.                              "convert character to integer"
x := 3.99 printString.                           "convert object to string via printOn:"
x := 3.99 storeString.                           "convert object to string via storeOn:"
x := 15 radix: 16.                               "convert to string in given base"
x := 15 printStringBase: 16.
x := 15 storeStringBase: 16.
```

## **Blocks**

* Blocks are objects and may be assigned to a variable.
* Value is last expression evaluated unless explicit return.
* Blocks may be nested.
* Specification: `[ arguments | | localvars | expressions ]`
* Max of three arguments allowed.
* `^ expression` terminates block and method (exits all nested blocks).
* Blocks intended for long term storage should not contain `^`

```smalltalk
| x y z |
x := [ y := 1. z := 2. ]. x value.                      "simple block usage"
x := [ :argOne :argTwo |   argOne, ' and ' , argTwo.].  "set up block with argument passing"
Transcript show: (x value: 'First' value: 'Second'); cr."use block with argument passing"
x := [ | z | z := 1.].                                  "localvars not available in squeak blocks"
```

## **Method Calls**

* Unary methods are messages with no arguments.

* Binary methods.

* Keyword methods are messages with selectors including colons `:`

  ​

  ### **Standard categories / protocols:**

  | initialize - release  | methods called for new instances.        |
  | --------------------- | ---------------------------------------- |
  | **accessing**         | **get / set methods**                    |
  | **testing**           | **boolean test - is**                    |
  | **comparing**         | **boolean test with parameter**          |
  | **displaying**        | **GUI related methods**                  |
  | **printing**          | **methods for printing**                 |
  | **updating**          | **receive notifications of changes**     |
  | **private**           | **methods private to a class**           |
  | **instance-creation** | **class methods for creating instances** |

```smalltalk
| x |
x := 2 sqrt.                                     "unary message"
x := 2 raisedTo: 10.                             "keyword message"
x := 194 * 9.                                    "binary message"
Transcript show: (194 * 9) printString; cr.      "combination (chaining)"
x := 2 perform: #sqrt.                           "indirect method invocation"
Transcript                                       "Cascading - send multiple messages to receiver"
   show: 'hello ';
   show: 'world';
   cr.
x := 3 + 2; * 100.                               "result=300. Sends message to same receiver (3)"
```



## **Conditional Statements**

```smalltalk
| x |
x > 10 ifTrue: [Transcript show: 'ifTrue'; cr].         "if then"

x > 10 ifFalse: [Transcript show: 'ifFalse'; cr].       "if else"

x > 10                                                  "if then else"
   ifTrue:  [Transcript show: 'ifTrue'; cr]
   ifFalse: [Transcript show: 'ifFalse'; cr].
   
x > 10                                                  "if else then"
   ifFalse: [Transcript show: 'ifFalse'; cr]
   ifTrue:  [Transcript show: 'ifTrue'; cr].
Transcript
   show:
      (x > 10
         ifTrue:  ['ifTrue']
         ifFalse: ['ifFalse']);
   cr.
   
Transcript                                              "nested if then else"
   show:
      (x > 10
         ifTrue:  [x > 5
            ifTrue:  ['A']
            ifFalse: ['B']]
         ifFalse: ['C']);
   cr.
   
switch := Dictionary new.                               "switch functionality"
switch at: $A put: [Transcript show: 'Case A'; cr].
switch at: $B put: [Transcript show: 'Case B'; cr].
switch at: $C put: [Transcript show: 'Case C'; cr].
result := (switch at: $B) value.
```

## **Iteration Statements**

```smalltalk
| x y |
x := 4. y := 1.
[x > 0] whileTrue: [x := x - 1. y := y * 2].                "while true loop"
[x >= 4] whileFalse: [x := x + 1. y := y * 2].              "while false loop"
x timesRepeat: [y := y * 2].                                "times repear loop (i := 1 to x)"
1 to: x do: [:a | y := y * 2].                              "for loop"
1 to: x by: 2 do: [:a | y := y / 2].                        "for loop with specified increment"
#(5 4 3) do: [:a | x := x + a].                             "iterate over array elements"
```

## **Characters**

```smalltalk
| x y |
x := $A.                                                    "character assignment"
y := x isLowercase.                                         "test if lower case"
y := x isUppercase.                                         "test if upper case"
y := x isLetter.                                            "test if letter"
y := x isDigit.                                             "test if digit"
y := x isAlphaNumeric.                                      "test if alphanumeric"
y := x isSeparator.                                         "test if seperator char"
y := x isVowel.                                             "test if vowel"
y := x digitValue.                                          "convert to numeric digit value"
y := x asLowercase.                                         "convert to lower case"
y := x asUppercase.                                         "convert to upper case"
y := x asciiValue.                                          "convert to numeric ascii value"
y := x asString.                                            "convert to string"
b := $A <= $B.                                              "comparison"
y := $A max: $B.
```

## **Symbol**

```smalltalk
| b x y |
x := #Hello.                                          "symbol assignment"
y := 'String', 'Concatenation'.                       "symbol concatenation (result is string)"
b := x isEmpty.                                       "test if symbol is empty"
y := x size.                                          "string size"
y := x at: 2.                                         "char at location"
y := x copyFrom: 2 to: 4.                             "substring"
y := x indexOf: $e ifAbsent: [0].                     "first position of character within string"
x do: [:a | Transcript show: a printString; cr].      "iterate over the string"
b := x conform: [:a | (a >= $a) & (a <= $z)].         "test if all elements meet condition"
y := x select: [:a | a > $a].                         "return all elements that meet condition"
y := x asString.                                      "convert symbol to string"
y := x asText.                                        "convert symbol to text"
y := x asArray.                                       "convert symbol to array"
y := x asOrderedCollection.                           "convert symbol to ordered collection"
y := x asSortedCollection.                            "convert symbol to sorted collection"
y := x asBag.                                         "convert symbol to bag collection"
y := x asSet.                                         "convert symbol to set collection"
```

## **String**

```smalltalk
| b x y |
x := 'This is a string'.                             "string assignment"
x := 'String', 'Concatenation'.                      "string concatenation"
b := x isEmpty.                                      "test if string is empty"
y := x size.                                         "string size"
y := x at: 2.                                        "char at location"
y := x copyFrom: 2 to: 4.                            "substring"
y := x indexOf: $a ifAbsent: [0].                    "first position of character within string"
x := String new: 4.                                  "allocate string object"

x                                                    "set string elements"
   at: 1 put: $a;
   at: 2 put: $b;
   at: 3 put: $c;
   at: 4 put: $e.
   
x := String with: $a with: $b with: $c with: $d.      "set up to 4 elements at a time"
x do: [:a | Transcript show: a printString; cr].      "iterate over the string"
b := x conform: [:a | (a >= $a) & (a <= $z)].         "test if all elements meet condition"
y := x select: [:a | a > $a].                         "return all elements that meet condition"
y := x asSymbol.                                      "convert string to symbol"
y := x asArray.                                       "convert string to array"
x := 'ABCD' asByteArray.                              "convert string to byte array"
y := x asOrderedCollection.                           "convert string to ordered collection"
y := x asSortedCollection.                            "convert string to sorted collection"
y := x asBag.                                         "convert string to bag collection"
y := x asSet.                                         "convert string to set collection"
y := x shuffled.                                      "randomly shuffle string"
```

## **Arrays**

* **Array**: Fixed length collection.
* **ByteArray**: Array limited to byte elements (0 - 255).
* **WordArray**: Array limited to word elements (0 - 2 ^ 32 )

```smalltalk
| b x y sum max |
x := #(4 3 2 1).                                "constant array"
x := Array with: 5 with: 4 with: 3 with: 2.     "create array with up to 4 elements"
x := Array new: 4.                              "allocate an array with specified size"

x                                               "set array elements"
   at: 1 put: 5;
   at: 2 put: 4;
   at: 3 put: 3;
   at: 4 put: 2.
   
b := x isEmpty.                                  "test if array is empty"
y := x size.                                     "array size"
y := x at: 4.                                    "get array element at index"
b := x includes: 3.                              "test if element is in array"
y := x copyFrom: 2 to: 4.                        "subarray"
y := x indexOf: 3 ifAbsent: [0].                 "first position of element within array"
y := x occurrencesOf: 3.                         "number of times object in collection"
x do: [:a | Transcript show: a printString; cr]. "iterate over the array"
b := x conform: [:a | (a >= 1) & (a <= 4)].      "test if all elements meet condition"
y := x select: [:a | a > 2].                     "return collection of elements that pass test"
y := x reject: [:a | a < 2].                     "return collection of elements that fail test"
y := x collect: [:a | a + a].                    "transform each element for new collection"
y := x detect: [:a | a > 3] ifNone: [].          "find position of first element that passes test"

sum := 0. x do: [:a | sum := sum + a]. sum.      "sum array elements"

sum := 0. 1 to: (x size) do: 					"sum array elements"
			[:a | sum := sum + (x at: a)]. 

sum := x inject: 0 into: [:a :c | a + c].        "sum array elements"

max := x inject: 0 into: [:a :c | (a > c)        "find max element in array"
   ifTrue: [a]
   ifFalse: [c]].
   
y := x shuffled.                                  "randomly shuffle collection"
y := x asArray.                                   "convert to array"
y := x asByteArray.                               "note: this instruction not available on Squeak"
y := x asWordArray.                               "convert to word array"
y := x asOrderedCollection.                       "convert to ordered collection"
y := x asSortedCollection.                        "convert to sorted collection"
y := x asBag.                                     "convert to bag collection"
y := x asSet.                                     "convert to set collection"
```

## **Ordered Collection**

Acts like an expandable array

```smalltalk
| b x y sum max |
x := OrderedCollection	with: 4 			   "create collection with up to 4 elements"
					  with: 3 
					  with: 2 
					  with: 1. 
                      
x := OrderedCollection new.                     "allocate collection"
x add: 3; add: 2; add: 1; add: 4; yourself.     "add element to collection"
y := x addFirst: 5.                             "add element at beginning of collection"
y := x removeFirst.                             "remove first element in collection"
y := x addLast: 6.                              "add element at end of collection"
y := x removeLast.                              "remove last element in collection"
y := x addAll: #(7 8 9).                        "add multiple elements to collection"
y := x removeAll: #(7 8 9).                     "remove multiple elements from collection"
x at: 2 put: 3.                                 "set element at index"
y := x remove: 5 ifAbsent: [].                  "remove element from collection"
b := x isEmpty.                                 "test if empty"
y := x size.                                    "number of elements"
y := x at: 2.                                   "retrieve element at index"
y := x first.                                   "retrieve first element in collection"
y := x last.                                    "retrieve last element in collection"
b := x includes: 5.                             "test if element is in collection"
y := x copyFrom: 2 to: 3.                       "subcollection"
y := x indexOf: 3 ifAbsent: [0].                "first position of element within collection"
y := x occurrencesOf: 3.                        "number of times object in collection"
x do: [:a | Transcript show: a printString; cr]."iterate over the collection"
b := x conform: [:a | (a >= 1) & (a <= 4)].     "test if all elements meet condition"
y := x select: [:a | a > 2].                    "return collection of elements that pass test"
y := x reject: [:a | a < 2].                    "return collection of elements that fail test"
y := x collect: [:a | a + a].                   "transform each element for new collection"
y := x detect: [:a | a > 3] ifNone: [].         "find position of first element that passes test"

sum := 0. x do: [:a | sum := sum + a]. sum.                 "sum elements"
sum := 0. 1 to: (x size) do: [:a | sum := sum + (x at: a)]. "sum elements"
sum := x inject: 0 into: [:a :c | a + c].                   "sum elements"

max := x inject: 0 into: [:a :c | (a > c)                   "find max element in collection"
   ifTrue: [a]
   ifFalse: [c]].
   
y := x shuffled.                                  "randomly shuffle collection"
y := x asArray.                                   "convert to array"
y := x asOrderedCollection.                       "convert to ordered collection"
y := x asSortedCollection.                        "convert to sorted collection"
y := x asBag.                                     "convert to bag collection"
y := x asSet.                                     "convert to set collection"
```

