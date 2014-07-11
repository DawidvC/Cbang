id: 9
author: slashvar
category: Language
title: C! - system oriented programming

> *This is a first article, intended to be an introduction to to*
> * **C!**, more articles presenting syntax and inner parts of the*
> *compiler will follow.*

**C!** is one of our project here at LSE (System Lab of EPITA.) It is a
programming language oriented toward lower-level system programming (kernel or
driver for example.)

We were looking for a modern programming language for kernel programming and
after trying some (D, C++, OCaml … ) it appears that most of them were too
oriented toward userland to be used in kernel programming context.

So we decide to *modify* and *extend* C to fit our need and quickly aim toward
a new programming language: **C!**

# Modern language and kernel programming

Most part of kernel code is rather classical: data structures, algorithms and
a lot of glue. But, some crucial aspects require lower-level programming:
direct management of memory, talking to specific CPU part (interruption
management, MMU … ), complete control over data layout, bit-by-bit data
manipulation …

Thus, to write some kernel code (or a complete kernel) we need a native
language with direct access to this kind of low-level operations. This implies
the ability to include ASM code somehow, to manage function calls from ASM and
to build standard functions (so you can have function pointers for various
interruptions mechanisms.)

And, since you're not in user-land, you can't use user-land facilities
(standard system libs for example.) For most language this means that you must
rewrite memory allocators and tools that come with (especially for managed
memory language using garbage collection.)

An other issue is the binary format: when writing user-land programs, your
compiler build a file suited for kernel binary loader. On actual Unix system
your file will respect the elf format. Of course, you can write an elf loader
in your boot-loader (or any part of your booting process for that matter) but
since you are managing memory and memory mapping, you can't rely on the way a
program is loaded on your system and thus the organization of your elf must
reflect these constraint.

Of course, this issue is not language dependent, even with a pure ASM or C
kernel, you will have to control the way your linker build the final
binary. But, in C (and obviously in ASM) there's no major issues here, the
structure of your program will be sufficiently simple so that the only
important question are: where will I be put in memory ?

***So, what's wrong with modern languages ?***

For the most evolved one such as language with memory transparent management
and garbage collection, one of the most important problem is the to provide a
replacement for all aspects of the standard libs of the system: memory
allocator, threads and locks management … And in that case some aspect just
can't be rewrited the same way it is in user-land.

The C++ situation is somehow better and worse: in theory there's less runtime
needs than most modern languages. The good part is that you can bypass the
most problematic element of C++ (such as RTTI or exception) so you don't have
to fight against them. Once you've deactivated problematic features and found
what can't be used without them, you have to provide runtime elements needed
by your code: start-up code, pure virtual fallback, dynamic stack allocation
code, dynamic memory allocation for new and delete operators (for objects and
array) …

Roaming here and there, you'll find documentation on how you can write your
C++ kernel, but face it: does the involved work is worse the pain ?

# What's wrong with C

So, if you're still reading me, this mean that you're partially convinced that
using C++ (or D, or OCaml, or … ) is not a good idea for your kernel. But, why
not going on with the good old C language ?

Since it was design for that job, it is probably the best one (or one of the
best) for it. But, we want more.

This is a quick lists of what we may find wrong or missing in C:

*   The C syntax contains a lot of ambiguous trap
*   While the type system of C is basically size based, a lot of type have an
    ambiguous size (int for example)
*   Controlling size and signedness of integer is often painful
*   There is no clean way to provide some form of genericity or polymorphism
*   There's no typed macro
*   The type system and most static verification mechanism are too *basic*
    compared to what can be done now
*   C miss a namespace (or module) mechanism
*   While you can do object oriented programming, it is tedious and error
    prone

In fact, the above list can divided in two categories:

*   syntax and base language issue
*   missing modern features

# Genese of C!

Once we stated what was wrong with C, I come up with the idea that we can
write a simple syntactic front-end to C or a kind of preprocessor, where will
shall fix most syntax issues. Since we were playing with syntax, we shall add
some syntactic sugar as well.

We then decided to take a look at object oriented C: smart usage of function
pointers and structures let you build some basic objects. You can even have
fully object oriented code. But while it is quiet simple to use code with
object oriented designed, the code itself is complex, tedious, error prone and
most of the time unreadable. So, all the gain of the OOP on the *outer side*,
is lost on the *inner side*.

***So why not encapsulated object oriented code in our syntax extension
 ?***

But, OOP means typing (Ok, **I wanted** static typing for OOP.) And thus, we
need to write our own types system and type checker.

Finally, from a simple syntax preprocessor, we ended up with a language of its
own.

# *Compiler-to-compiler*

Designing and implementing a programming language implies a lot of work:
parsing, static analysis (mostly type checking), managing syntactic sugar, and
a lot of code transformation in order to produce machine code.

While syntactic parts and typing are unavoidable, code productions is can be
shorten somehow: you write a frontend for an existing compiler or use a
*generic* backend such as llvm. But you still need to produce some kind of
*abstract* ASM, a kind of generic machine code that will be transformed into
target specific machine code.

The fact is that a normal compiler will already have done a lot of
optimization and smart code transformation **before** the backend stage. In
our case, this means that we should do an important part of the job of a
complete C compiler while we are working with code that is mainly C (with a
different concrete syntax.)

The last solution (the one we choose) is to produce code for another compiler:
in that case all the magic is in the target compiler and we can concentrate
our effort on syntax, typing and extensions that can be expressed in the
target language.

Based on our previous discussion, you can deduce that we choose to produce C
code. Presenting all aspect of using C as a target language should the subject
of a future article.

# Syntactic Sugar

An interesting aspect of building a high-level language is that we can add new
shinny syntax extension quite simply. We decide to focus on syntax extension
that offers comfort without introducing hidden-complexity.

## Integer as bit-array

In lower-level code, you often manipulate integer bit per bit, so we decide to
add a syntax to do that without manipulating masks and bit per bit logical
operands.

Thus, any integer value (even signed, but this may change, or trigger a
warning) can be used as array in left and right position (you can test and
assign bit per bit your integer !)

One little example (wait for the next article for full syntax description):

    :::C
    x : int<+32> = 0b001011; // yes binary value
    t : int<+1>;
    t = x[0];
    x[0] = x[5];
    x[5] = t;

## Assembly blocks

When writing kernel code, you need assembly code blocks. The syntax provided
by gcc is annoying, you have to correctly manage the string yourself (adding
newline and so on.)

On the other hand, I don't want to add a full assembly parser (as in D
compiler for example.) Despite the fact that its boring and tedious, it
implies that we use stuck our language to some architectures and we shall have
to rewrite the parser for each new architecture we need …

Finally, I found a way to integrate asm blocks without the noise of gcc but
keeping it close enough to be able to translate it directly. Of course, this
means that you still have to write clobber lists and other stuff of that kind.

A little example (using typed macro function):

    :::C
    #cas(val: volatile void**, cmp: volatile void*, exch: volatile void*) : void*
    {
      old: volatile void*;
      asm ("=a" (old); "r" (val), "a" (cmp), "r" (exch);)
      {
                lock
                cmpxchg %3, (%1)
      }
      return old;
    }

## Macro stuff

Actually, **C!** has no dedicated preprocessing tools but we include some
syntax to provide code that will be macro rather than functions or variables.

First, you can transform any variable or function declaration into a kind of
typed macro by simply adding a sharp in front of the name (see previous
example.) The generated code will be traditional C macro with all the « dirty
» code needed to manage return, call by value and so on.

The other nice syntax extension is *macro class*: a macro class provide method
(in fact macro function) on a non object type. The idea is to define simple
and recurring operations to a value without boxing it (next article will
provide examples.)

# Modules

Another missing features of C is a proper module mechanism. We provide a
simple module infrastructure sharing a lot (but far simple) with C++
namespace. Basically, every **C!** file is a module and referring to symbols
from that module need to refer to the module's name, just like a namespace
name. Of course you can also *open* the module, that is make directly
available (without namespace) every symbols of the module.

Namespaces provide a simple way to avoid name specialization: inside the
module you can refer to it directly and outside you use the module name and
thus no inter-module conflict could happen.

# *What's next*

In the next article of this series I will present you with **C!'s** syntax, the
very basis of the object system and macro stuff.

The compiler is still in a *prototype* state: all features describe here are
working, but some details are still a bit fuzzy and may need you to do some
adjustment in the generated code.

As of now, you can checkout **C!** on LSE git repository, take a look at the
examples in the tests dir and begin writing your own code. Unfortunately, we
don't have yet an automated build procedure, so you have to do it step by
step.
