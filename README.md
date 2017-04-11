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

## Comments

```smaltalk
"Comments are enclosed in quotes"

```



## Statement Separator `.`

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
"x := 16r80 bitAt: 7."                            "bit at position (0|1) [!Squeak]"
x := 16r80 highbit.                               "position of highest bit set"
b := 16rFF allMask: 16r0F.                        "test if all bits set in mask set in receiver"
b := 16rFF anyMask: 16r0F.                        "test if any bits set in mask set in receiver"
b := 16rFF noMask: 16r0F.                         "test if all bits set in mask clear in receiver"
```

