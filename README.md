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



## **Assignement**

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

