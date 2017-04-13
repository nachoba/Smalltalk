![pharo](pharo.png)

# Pharo Snippets

An assortment of useful snippets, *at least for me*.

**How to get and image from the Internet and set it as a wallpaper:**

```smalltalk
"Getting an image from internet and set it as wallpaper"     
  | form | 
  form := ZnEasy getJpeg: 'http://<uri>'. 
 "form := ZnEasy getPng:  'http://<uri>'."
  World backgroundImage: form layout: #scaled 
```

**Get the contents of a web page:**

```smalltalk
"Showing the contents of a Web Page"
| aWebPage |
aWebPage := 'http://www.pharo.org/' asUrl retrieveContents.
Transcript open; show: aWebPage.
```

**Open a System Window:**

```smalltalk
"Opening a SystemWindow in World, using the Morphic UI"
| aWindow|
aWindow := SystemWindow new.
aWindow openInWorld.
aWindow setLabel: 'A System Window'.
aWindow inspect.
```

**Reading a text file named *untitled.txt*:**

```smalltalk
"Reading the whole file at once"
| file string line|
file := FileStream fileNamed: 'untitled.txt'.
string := file contentsOfEntireFile.
Transcript open; show: string.
file close.

"Reading the file line by line"
file := FileStream fileNamed: 'untitled.txt'.
Transcript open.
[ file atEnd ] whileFalse: [ line := file nextLine. Transcript show: line; cr].
file close.
```

**Adding a class and some methods sending messages:**

```smalltalk
"Create the class"
Object subclass: #TestClass 
       instanceVariableNames: 'one'  
       classVariableNames: '' 
       package: 'IS-Test'. 

"Add a comment to the class"
TestClass comment: 'This is a test class'.


"Add some methods"
TestClass compile:
'one 
    ^one' 
          classified: 'accessor'.

TestClass compile: 
'one: aNumber 
   one := aNumber'
          classified: 'accessor'.

TestClass compile: 
'printMe
   "Print in the Transcript"
   Transcript open.
   Transcript show: self one; cr'
          classified: 'printing'.
```

**Command-Line Pharo**

This shows how to execute code from the command line. We want to run:

```smalltalk
# Create a class
Object subclass: #TestClass 
       instanceVariableNames: 'one' 
       classVariableNames: '' 
       package: 'IS-Test'. 

# Wait a little to get the whole class compiled
(Delay forSeconds: 3) wait. 

# Define two methods
TestClass compile:'one ^one' classified: 'accessor'. 
TestClass compile: 'one: aNumber one := aNumber' classified: 'accessor'. 

# Test the class
aTest := TestClass new. 
aTest one: 25. 
aTest one.
```

The output of this should be: `25`.

We have to enter:

`./Pharo --headless imageName.image eval "codeGoesHere"`

So, it will be:

```smalltalk
# For Windows
PharoConsole --headless Pharo-minimal.image eval "Object subclass: #TestClass instanceVariableNames: 'one' classVariableNames: '' package: 'IS-Test'. (Delay forSeconds: 3) wait. TestClass compile:'one ^one' classified: 'accessor'. TestClass compile: 'one: aNumber one := aNumber' classified: 'accessor'. TestClass compile: 'printMe Transcript show: (one printString).' classified: 'printing'. aTest := TestClass new. aTest one: 25. aTest one"

# For *NIX
./Pharo --headless Pharo-minimal.image eval "Object subclass: #TestClass instanceVariableNames: 'one' classVariableNames: '' package: 'IS-Test'. (Delay forSeconds: 3) wait. TestClass compile:'one ^one' classified: 'accessor'. TestClass compile: 'one: aNumber one := aNumber' classified: 'accessor'. TestClass compile: 'printMe Transcript show: (one printString).' classified: 'printing'. aTest := TestClass new. aTest one: 25. aTest one"
```

