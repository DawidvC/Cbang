id: 13
author: slashvar
category: Language
title: C! - system oriented programming - Syntax Explain

> *Following the previous article introducing **C!** I now present the*
> *language itself. I kept presentation as short as possible and present*
> *relation to C syntax when it's relevant.*

## Basic syntax: statement and expressions

Globally **C!** code will look like C code. There're few details due to some
adjustement but you'll find usual operators, functions call, loop and *if*
statements … The global structure of the code will look very familiar.

Among minor differencies are: cast, function pointer usage and types syntax.

## Declarations

The most striking differencies is probably declarations syntax. In C, there's
no clear separtation between the declared entity (variables, functions or type
names) and the type description of the entity. For example, in C, if you
declare an array of characters you'll write something like:

    :::C
    char t[256];

The variable name is <tt>t</tt> and its type is *array of <tt>char</tt>* (the
size being some extra information.)

In **C!**, we choose to break things more clearly, and have in the declaration
a part naming the entity and a part describing its type, the previous
expression becomes:

    :::cbang
    t : char[256];

This clarified the question of the position of the star when declaring a
pointer, for example in C, we shall write:

    :::C
    char *p;

and in **C!**:

    :::C
    p : char*;

The star no longer needs to be attached to <tt>p</tt> and you can't write
ambiguous declarations like:

    :::C
    char* p, c;

Where <tt>c</tt> is character and not a pointer to character. Of course, the
drawback is that we must write two lines for that example:

    :::cbang
    p : char*;
    c : char;

The same logic appears on function declaration, for example the following C
code:

    :::C
    char f(char c)
    {
      if (c < 'a' || c > 'z')
        return c;
      return 'A' + c - 'a';
    }

Will be written in **C!**:

    :::cbang
    f(c : char) : char
    {
      if (c < 'a' || c > 'z')
        return c;
      return 'A' + c - 'a';
    }

We apply the same idea to cast, thus the following code:

    :::C
    void f(void *p, char *c)
    {
      *c = *((char*)p);
    }

becomes:

    :::cbang
    f(p : void*, c : char*) : void
    {
      *c = *(p : char*);
    }

The same logic is shown in type name definitions:

    :::C
    typedef char *string;

becomes:

    :::cbang
    typedef string = char*;

Again, function pointer have a simplified syntax: the name of the variable is
no longer inside the type. So the following C code:

    :::C
    char (*f)(char,char*);

Becomes:

    :::cbang
    f : <(char,char*)> : char;

Of course, you can add initilization expressions:

    :::cbang
    a : char = 'a';

## Integer and floating point numbers

We decide to have explicit size and signedness in integer types. Thus, integer
will be declared as follow:

    :::cbang
    x : int<32>;  // a signed 32bits integer
    y : int<+16>; // an unsigned 16bits integer
    z : int<24>;  // uncommon size declaration

Sizes not belongings to standard sizes are stored using available integer
types in C99 (the ones defined in <tt>stdint.h</tt>) and are masked when
needed to prevent usage of unwanted values.

The same ideas apply to floating point numbers:

    :::cbang
    f : float<64>; // a double float

Of course, you can define some types name (but you can't use <tt>int</tt>,
<tt>char</tt> and <tt>float</tt>):

    :::cbang
    typedef short = int<16>;

Sized integer in structure definition are directly translated as bitfields, so
we have a single syntax.

We extends the language syntax with a notion of *bits arrays*: that is an
integer can be used as an array of bits:

    :::cbang
    x : int<+32> = 41;
    x[31] = 1;           // set the most significant bit to 1
    x[31] = 0;           // set the most significant bit to 0
    x += (x[0] ? 1 : 0); // make x even if not

When setting bit, value other than 0 are transformed into 1.

## Object Oriented Extension

We introduce a classical, but yet simple, OOP extension to our language. So
first, you can define classes with attributes, methods and constructors:

    :::cbang
    class A {
      x : int<32>;
      get() : int<32> { return x; }
      set(y : int<32>) : void { x = y; }
      // A simple constructor
      init(y : int<32>) { x = y; }
    }

We have simple inheritance and methods are true methods (that is virtual
methods):

    :::cbang
    class B : A {
      y : float<32>;
      init(a : int<32>, b : float<32>) {
        A(this, a) // call A constuctor
        y = b;
      }
      get() : int<32> { return x + (y : int<32>); }
    }

We don't have (yet ?) method overloading, only overriding.

Object in **C!** are always pointer and you should allocate them by yourself
(so we don't rely on predefined allocator) but you can create some kind of «
local object » that is an object defined on the stack or as global value.

    :::cbang
    og : A = A(some_pointer, 41); // object creation require pre-allocation
    ol : local A(42);             // object on the stack
    og.set(og.get() + 1);

There's no implicit destructor calls for now, but depending on real nead we
may add it for local objects.

Since we only have pointed-object there's no implicit copy as in C++ nor
there's need for references. Access to content (all is public) is done with
the simple dot syntax.

The constructor for an object is a simple function that take a pointer to the
concrete object (the object pointer) and any needed parameters. It returns the
object pointer. If you're object is "compatible" with the object built by a
given constructor, you safely can pass it to the constructor (as in the
previous example.)

Local object are not automatically initialized, in the following code

    o : local A;

Object <tt>o</tt> is allocated on the local scope but not initiliazed: methods
table is "empty" (a method call will fail … ) In near future we probably be
able to detect that, or at least provide a minimal initialization.

We also provide interface and abstract methods.

I may explain generated code in some future article.

## Typed macro and *Macro Class*

We introduce a simple way to define typed macro constants and macro functions:
you just a <tt>#</tt> at the begining of a declaration:

    :::cbang
    #X : int<32> = 42;
    #square(x : int<32>) : int<32>
    {
      return x * x;
    }

Our macro functions enjoy a real call by value semantics (using some tricks in
the generated code) and (once typed by **C!**) are real <tt>cpp</tt> macro in
the generated code!

The other macro extension is the macro class concept: we syntactically embeded
a value (of any type) in some kind of object with methods. The result produces
special macro but let you use your values just like an object.

    :::cbang
    macro class A : int<32> // storage kind
    {
      get() const  : int<32> // won't modify inner storage
      {
        return this; // this represent the inner storage value
      }
      set(x : int<32>) : void // non const can modify inner storage
      {
        this = x;
      }
    }

For now, all "macro code" generate CPP macro (with a lot of tricks to respect
call by value and return management. It is not excluded to generate inlined
functions in the future as long as we are sure that semantics is preserved.

One of the idea behind macro class is to provide a simple syntax (OO like) for
constructions that do not require functions (or worse the burden of a whole
object.)

## Properties

Properties is an other extension (very young and poorly tested) in the same
spirit than macro class.

The idea is quite simple: it provides a way to overload access to any kind of
value (structured or not) and make it appears as another type (the *virtual
type*.) You just have to provide a getter and a setter and when context
requires the *virtual type* the compiler automatically insert the right
accesser.

For example, you have a 32 bits unsigned integer stored in two different
locations but you want to access it as if it is a plain and simple
integer. Suppose you have a structure <tt>s</tt> storing the two pointer,
you'll have use it that way in plain old C:

    :::C
    unsigned x, y = 70703;
    x = ((*(s.high)) << 16) + *(s.low); // getting the value
    *(s.high) = y >> 16;                // setting the value
    *(s.low) = y & (0xffff);

You can declare a property that way (I included the structure describing our
splitted integer):

    :::cbang
    struct segint {
      high: int<+16>*;
      low: int<+16>*;
    }
    
    property V(segint) : int<+32>
    {
      get() {
        return ((*(this.high)) << 16) + *(this.low);
      }
      set(y : int<+32>) {
        *(this.high) = y >> 16;
        *(this.low) = y & (0xffff);
      }
    }

And then, to use it:

    :::cbang
    s : segint;
    s.high = &high; s.low = &low; // init the struct
    x : V = s; // warning: x is a copy of s
    y : int<+32>;
    y = x + 1; // accessing the value
    x = 70703  // setting it

Since a property can have any real type you want, it can be part of an object
and have its own <tt>this</tt> pointer corresponding to a pointer to the
object (since every thing is public the property have a fool access to the
object.)

As of now, accessors are generated as macro and access to the real value is
done through a *reference* (so it can be modified.)

Support for *op-assign* (operators like <tt>+=</tt>) and other similar
operators (mainly <tt>++</tt>) will probably be added later.
