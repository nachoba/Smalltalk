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
b := $A isLowercase.                                    "test if lower case character"
```

