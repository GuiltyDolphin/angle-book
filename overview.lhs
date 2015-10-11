\documentclass[a4paper,11pt,oneside]{article}

%include lhs2TeX.fmt
%include lhs2TeX.sty
%include polycode.fmt

% \usepackage[nounderscore]{syntax}
% \usepackage{underscore}

% This provides a table of contents that okular can read.
\usepackage{hyperref}
\hypersetup{
  colorlinks,
  citecolor=black,
  filecolor=black,
  linkcolor=black,
  urlcolor=black
}

\title{An implementation of a basic general-purpose programming language}
\author{Ben Moon}
\date{}

\begin{document}

\maketitle

\begin{abstract}
  % TODO: Not sure about the 'design-process' bit.
  This document provides an overview of the design-process, usage and
  implementation of Angle - a general purpose programming language.
  \\
  Additionally, this overview will provide a language reference,
  describing each of Angle's features in detail.
  \\
  The programming language `Haskell', along with standard tools such as
  Cabal and the Glasgow Haskell Compiler have been used for the
  implementation.
  \\
  % FIXME: Not sure about the colon after including.
  Angle takes inspiration from many programming languages, including:
  Perl, Lisp, Python, Ruby, and Haskell itself.
  \\
  The end product is twofold: the language itself, its grammar,
  implementation details and semantics; and the software that can be
  used to execute source either interactively or non-interactively.
  % The end product is a piece of software that can be used to run
  % syntactically correct source files either interactively or
  % non-interactively.

\end{abstract}

\tableofcontents


\section{Background}
\label{sec:background}

% TODO: Better phrasing, need to say that it also introduces some
% terminology.
What follows is an overview of external software and libraries used in
this project.

\subsection{Haskell}
\label{sub:haskell}

I have decided to write my project using the Haskell programming
language. Haskell is a statically-typed, functional, lazy language
that supports type-inference.\footnote{https://www.haskell.org/}

% TODO: Want better phrasing.
Haskell's type system makes it very easy to develop projects quickly
and safely as a project will not compile unless types match up.

\subsection{Software}
\label{sub:software}

In developing Angle, I have used various software to deal with
compiling, documenting, testing and many other aspects.

\subsubsection{Cabal}
\label{ssub:cabal}

Cabal is a package manager for projects written in Haskell.\footnote{https://www.haskell.org/cabal/}
It allows information about the package, including name, description,
library information and test suites to be specified within a
\texttt{.cabal} file, and can then be used to simplify the task of
compiling, running tests and installing packages.


\subsubsection{Haddock}
\label{ssub:haddock}

Haddock is a system for generating documentation from annotated Haskell
source files.\footnote{https://www.haskell.org/haddock/}
I chose Haddock due to it's intuitive syntax, ease of readability and
integration with Cabal.


\subsubsection{Glasgow Haskell Compiler}
\label{ssub:glasgow_haskell_compiler}

The Glasgow Haskell Compiler, or GHC, is the main actively-maintained
compiler for Haskell.\footnote{https://www.haskell.org/ghc/} It is very
stable and provides a lot of additional features that may be used when
required by a programmer.

\subsubsection{Libraries}
\label{ssub:libraries}

Throughout the project I have used many libraries written by other
developers. Some part of the Haskell Platform\footnote{https://www.haskell.org/platform/contents.html},
others standalone. Below are some of the packages I have used.

\begin{description}
  \item[QuickCheck] - a testing library that can generate random tests
    for checking that specified properties hold when applied to
    different data.\footnote{https://hackage.haskell.org/package/QuickCheck}
  \item[Tasty] - a test framework that allows the combination of tests
    into a single test suite for running.\footnote{https://hackage.haskell.org/package/tasty}
  \item[Criterion] - a benchmarking library for measuring the
    performance of code.\footnote{https://hackage.haskell.org/package/criterion}
\end{description}


\subsubsection{Git}
\label{ssub:git}

Git is an open source version control system that allows changes in a
project to be staged, committed and tracked. This means that changes
to the project can easily be undone and reviewed, and allows changes
to be labeled for future reference.\footnote{https://git-scm.com/}


\subsection{A brief overview of language implementation}
\label{sub:a_brief_overview_of_language_implementation}

\subsubsection{Translator Software}
\label{ssub:translator_software}

% TODO: Get a link for this bit.
Translator software is used to translate a program written in one
language to another language, without loosing any functionality.
\\
Although people often refer to languages as `compiled' or
`interpreted', the translator software is distinct from the language
itself, and thus a language could be both interpreted \textit{and}
compiled.\footnote{This is quite common with the more popular
languages, for example: Python and Lisp both have compiler and
interpreter implementations.}
% TODO: Not sure about that previous paragraph!


\paragraph{Compilers}
\label{par:compilers}
A compiler is a piece of software that takes a file containing the
source code for one language, and produces output in another language.
The output language is often in a form that can be executed by the
system's CPU - namely object code.

\paragraph{Interpreters}
\label{par:interpreters}
Interpreters differ from compilers in that instead of programs first
being translated to machine code before being executed, instructions
are translated into pre-compiled subroutines and executed directly at
run-time, thus possibly incurring a speed decrease when compared to a
similar compiled language.\footnote{http://www.vanguardsw.com/dphelp4/dph00296.htm}
\\
One of the main disadvantages of an interpreter is that it is
required every time a program needs to be run. Source code is also
more transparent (which may or may not be a disadvantage), as the
files are run directly by the interpreter.

\subsubsection{Programming Paradigms}
\label{ssub:programming_paradigms}

Programming paradigms are a way of writing programs using a
programming language. Most languages encourage the use of one or more
paradigms (for example, Haskell strongly encourages the use of the
functional paradigm), but will not usually \textit{force} the
programmer to use a single one.

\paragraph{Common paradigms}
\label{par:common_paradigms}

Following is a brief description of some of the more common
paradigms.\footnote{http://cs.lmu.edu/~ray/notes/paradigms/}

\paragraph{Declarative}
\label{par:declarative}

The declarative paradigm is one in which programs describe what
actions should be performed, but don't explicity state \textit{how}
this should be done.
\\
Common examples of declarative languages are database query languages such as SQL.
\\
\\
\textit{The programmer specifies that the required action is retrieving a column from a table in a database. No indication of how to achieve this is given.}
\begin{spec}
SELECT column FROM table WHERE condition
\end{spec}


\paragraph{Imperative}
\label{par:imperative}

In the opposite spectrum of paradigms to declarative is imperative.
In imperative languages the programmer specifies how to perform
computations via a sequence of sep-by-step instructions.\footnote{https://msdn.microsoft.com/en-gb/library/bb669144.aspx}
Contrary to declarative languages, the order of execution in
imperative languages can greatly affect the outcome of the program.


\paragraph{Object-oriented}
\label{par:object_oriented}

In object-oriented languages, structures called objects, which have
state (usually represented by properties) and methods that are used
to change this state.
\\
Objects can call other objects' methods, or potentially directly
modify their properties in order to change this state.
\\
Inheritance is usually present in object-oriented languages, and
allows a parent-child relationship between different objects.
\\
\\
% FIXME: Don't like this... Choose a better example.
\textit{A python3 example of class-based object-oriented programming.}
\begin{spec}
class Animal:
    noise = "roar"

    def __init__(self, name):
        self.name = name


class Duck(Animal):
    noise = "quack"

    def __init__(self, *args):
        super().__init__(*args)


sam = Duck("Sam")
\end{spec}


In the above example, it could be said that @sam@ is a @Duck@ called
@Sam@, and that a @Duck@ is a type of @Animal@.
\\
@noise@ is a class variable, and does not rely on an instance (@sam@)
being created, thus @Duck.noise == 'quack'@, but also
@sam.noise == 'quack'@.


\paragraph{Functional}
\label{par:functional}

In functional languages, computations are treated as the evaluation
of mathematical functions. Side-effects are usually low to
non-existent as in a purely functional language functions should be
referentially transparent (the same result should always arise with
the same arguments).\footnote{https://wiki.haskell.org/Functional\_programming}
\\
Several features are prominent in functional languages:

\begin{description}
  \item[functions as first-class citizens] this means that functions
    are treated equally when used in place of other data structures
    (integers, lists etc...) and can thus be passed around as data.
  \item[higher-order functions] functions that are higher-order can
    take functions as arguments or produce them as results. This
    makes sense when functions are first-class citizens as they should
    be generally indistinguishable from other data.
\end{description}


Haskell is an example of a purely-functional programming language.

% Functions in Angle are treated as first-class citizens; they can be
% passed as values (via lambdas) just like any other data (integers,
% strings etc.). Functions can also be higher-order, meaning that they
% can accept other functions as arguments and call these functions
% with another set of arguments. Due to these capabilities, Angle is
% a functional language, albeit an impure one.\footnote{https://wiki.haskell.org/Functional\_programming}


\subsubsection{Type systems}
\label{ssub:type_systems}

\paragraph{Static and dynamic type checking}
\label{par:static_and_dynamic_type_checking}

% FIXME: Not happy with this paragraph

In a statically-typed language, certain type-related criterion must
be met before a program can be executed. These criterion can ensure
a program will have well-typed operations and that types are not used
incorrectly.\footnote{http://courses.cs.washington.edu/courses/cse341/04wi/lectures/13-dynamic-vs-static-types.html}
\\
Statically-typed languages often require the programmer to annotate
code, stating the types of variables and functions when they are
declared; leading to increased verbosity.
\\
In a dynamically-typed language, types are checked at run-time, this
means that type-correctness is not ensured and errors may occur as a
result of types being used in places for which they are not valid.\footnote{http://c2.com/cgi/wiki?DynamicTyping}


% \subparagraph{Dynamic}
% \label{par:dynamic}
%
% Being dynamically typed means that type-checking in Angle is
% performed at run-time,\footnote{http://c2.com/cgi/wiki?DynamicTyping}
% thus programs are not guaranteed to be type-correct
% (unlike in Haskell, where the compiler ensures that programs do not
% compile if any inconsistencies in types are noticed).




\paragraph{How to read this document}
\label{par:how_to_read_this_document}

% FIXME: There is repetition on '...refer to the source files...'
This document should be read in conjunction with the source files
provided. Examples of code in this document are for illustration
purposes and should be assumed to be non-functional - the source files
should be referred to for working code.
\\
Relevant modules will be stated before any sections describing the
development of code.
\\
Additional documentation is provided in the form of HTML files
generated by Haddock. Any capable browser will be sufficient to view
the documentation interactively.



\part{Introduction}
\label{prt:introduction}

\section{The Language}
\label{sec:the_language}

\paragraph{What is Angle?}
\label{par:what_is_angle_}

Angle is intended to be a weak and dynamically typed, functional,
interpreted, general-purpose programming language.

\subparagraph{Dynamic}
\label{par:dynamic}

Being dynamically typed means that type-checking in Angle is
performed at run-time,\footnote{http://c2.com/cgi/wiki?DynamicTyping}
thus programs are not guaranteed to be type-correct
(unlike in Haskell, where the compiler ensures that programs do not
compile if any inconsistencies in types are noticed).

\subparagraph{Weak}
\label{par:weak}

% TODO: Check this
% TODO: Maybe add small section on operator coercion.
Angle has weak typing as type information is not guaranteed to be
preserved during run-time.\footnote{http://c2.com/cgi/wiki?WeaklyTyped}
One example of this would be Angle's type coercion capabilities
through one of the built-in functions. Using this function allows the
programmer to (for instance) convert an integer to a string or vice
versa.

\subparagraph{Functional}
\label{par:functional}

Functions in Angle are treated as first-class citizens; they can be
passed as values (via lambdas) just like any other data (integers,
strings etc.). Functions can also be higher-order, meaning that they
can accept other functions as arguments and call these functions
with another set of arguments. Due to these capabilities, Angle is
a functional language, albeit an impure one.\footnote{https://wiki.haskell.org/Functional\_programming}

\subparagraph{General-purpose}
\label{par:general_purpose}

There was no specific use-case in mind when developing Angle, as such
it is intended to be able to cope with the implementation of a variety
of different types of software.

\subparagraph{Interpreted}
\label{par:interpreted}

Interpreted languages differ from compiled languages in that instead
of programs first being translated to machine code before being
executed, instructions are translated into pre-compiled subroutines and
executed directly at run-time, thus possibly incurring a speed decrease
when compared to a similar compiled language.\footnote{http://www.vanguardsw.com/dphelp4/dph00296.htm}

Choosing to make Angle an interpreted language rather than compiled
means that a compiler need not be written. It also means that
programs do not need to be compiled before they are run and can be
distributed as source without the need for any additional files,
although the interpreter (`angle') is required in order to run the
programs.

\section{Project Overview}
\label{sec:project_overview}

\subsection{Components}
\label{sub:components}

Angle is contained within a cabal package which consists of the
library, tests, benchmarks and executable.

The library contains the necessary source code to build Angle's
internals, which are then used to build the executable.

The executable is a program that can be used to run scripts written
using Angle's syntax. See section~\ref{sub:using_angle} `Using Angle'
for information on how this is used.


% TODO: Maybe a Readme that has usage information and this links to
% that?
\subsection{Using Angle}
\label{sub:using_angle}

The `angle' program supports two main modes of execution: interactive
and non-interactive.

\paragraph{Interactive Angle}
\label{par:interactive_angle}

When Angle runs in interactive mode, the user is presented with a
prompt at which they may enter one or more lines of code which can then
be executed. This mode allows a programmer to debug code, test short
snippets and ensure code has the correct syntax before using it in a
program.

\paragraph{Non-Interactive}
\label{par:non_interactive}

When Angle runs in non-interactive mode, achieved by passing a file
to be executed, a source file is read and run by the Angle
interpreter. This mode is how programs should usually be run.


% \section{Features}
% \label{sec:features}
%
% Angle supports many features, such as looping and support for
% functional programming, along with some more obscure features such as
% parameter constraints.
%
% % TODO: Update this to reflect the addition of exception handling,
% % file io and the possibility for imports through eval.
% Angle also has a lot of potential for more big features to be
% implemented, some of which are largely essential to a good programming
% environment - exception handling and imports. The way in which Angle
% has been implemented and refined means that adding new features
% requires modifying as little of the existing code as possible, and
% instead promotes constructive modification.
%
% Some features and where to find the respective information is shown
% in the table below.
%
% `Types' refers to a type defined in \texttt{Angle.Types.Lang} that provides
% more information on the feature.
%
% \begin{tabular}{l c r}
%   Feature & Types & Sections \\
%   \hline
%   Conditionals & `LangStruct' & section~\ref{ssub:conditional_constructs} \\
%   Parameter constraints & `ConstrRef' & section~\ref{ssub:function_definitions} \\
%   Parameter annotations & `AnnType' & section~\ref{ssub:function_definitions} \\
%   Looping structures & `LangStruct' & section~\ref{ssub:looping_structures} \\
%   Functions & `Lambda' & section~\ref{ssub:function_definitions} \\
%   Operators & `LangOp' & section~\ref{ssub:operations} \\
% \end{tabular}

\part{Language Reference}
\label{prt:language_reference}

\section{Introduction}
\label{sec:introduction}

% FIXME: Better phrasing please.
The following reference describes the language features, grammar and
methodology of Angle. Each section describes a feature or ideal of
Angle, and sections describing syntax features will include a grammar
in Extended Backus Naur form at the end.

\subsubsection{Notation}
\label{ssub:notation}

\paragraph{Extended Backus Naur Form (EBNF)}
\label{par:extended_backus_naur_form_ebnf_}

EBNF is an extended version of Backus Naur Form, a notation that can be
used to express the grammar of formal languages.\footnote{http://www.garshol.priv.no/download/text/bnf.html\#id1.2.}
\\
BNF can be used to describe context-free grammars,\footnote{http://matt.might.net/articles/grammars-bnf-ebnf/}
which are grammars that consist of names and expansions
(the components), meaning that it may be used to express a grammar for
Angle.


\section{Program structure}
\label{sec:program_structure}

A program written using Angle is made up of statements: statements
may in turn be made up of many more statements, a language structure
or an expression.

\subsection{Comments}
\label{sub:comments}

Comments represent code that will be ignored by Angle. Comments start
with a `\#' character and continue to the end of the line.

\subsection{Identifiers and reserved words}
\label{sub:identifiers_and_reserved_words}

\subsubsection{Reserved words}
\label{ssub:reserved_words}

Reserved words (or keywords) are reserved identifiers that may not be
used for variable names.
\\
The following is a list of the reserved words in Angle:
\\
\begin{tabular}{ l c c c r }
break & catch & continue & defclass & defun \\
do    & else  & false    & for      & if \\
in    & null  & raise    & return   & then \\
true  & try   & unless   & when     & while \\
\end{tabular}


\subsubsection{Reserved identifiers}
\label{ssub:reserved_identifiers}

Certain identifiers have predefined meanings in Angle, some of these
may be overwritten, others may not.

\begin{tabular}{ l c l }
Name & Can be overwritten? & Use \\
main & no & Whether the current program was invoked directly. \\
\_it & yes & Holds the value of the last computation. \\
asClass & no & Whether the current function was called as a constraint. \\
\end{tabular}

\subsubsection{Grammar}
\label{ssub:grammar}

\begin{spec}

alpha          = `a'..`Z'                              ;
digit          = `0'..`9'                              ;

identifier     = simple_ident  | function_ident        ;

simple_ident   = (alpha | `_') { alpha | digit | `_' } ;
function_ident = `\$' simple_ident                     ;
\end{spec}
% FIXME: Another $ that needs escaping in the source code.
% unescape it for final document.



\subsection{Literals}
\label{sub:literals}

Literals allow the specification of constant values for some of
Angle's built-in types.\footnote{https://www.cs.cf.ac.uk/Dave/Multimedia/node71.html}

% Literals allow the programmer to specify exact values in Angle that
% will remain constant through separate runs of the program, provided
% the source code is not modified.\footnote{https://www.cs.cf.ac.uk/Dave/Multimedia/node71.html}


\subsubsection{Strings}
\label{ssub:strings}

Two types of string literal are supported in Angle: backslash-escaped
and non-backslash-escaped.

By default, escape sequences in strings will be recognized (for
example, `\textbackslash n' would be recognized as a newline), but the `e' prefix
can be used to treat all backslashes literally (thus
`\textbackslash n' would become a backslash followed by an `n').

\begin{spec}
string = [ `e' ] `"' { string_char } `"' ;

string_char = <any character allowed in a Haskell String> ;
\end{spec}

Additionally, each character of a string may be represented as a
literal.

\begin{spec}
char = `'' char_char `'' ;

char_char = <any character allowed in a Haskell Char> ;
\end{spec}

Note that Angle uses the same character escaping as Haskell.\footnote{https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Char.html\#t:Char}


\subsubsection{Numeric literals}
\label{ssub:numeric_literals}

Two types of numeric are supported by Angle: floats (which should
have at least the range and precision of the IEEE double-precision
type),\footnote{http://hackage.haskell.org/package/base-4.8.1.0/docs/Prelude.html\#t:Double}
 and integers.


\begin{spec}

number  = integer | float                                         ;

integer = [ `-' ] digit { digit }                                 ;
float   = [ `-' ] digit { digit } `.' digit { digit }             ;

\end{spec}

\begin{spec}
boolean = `true'  | `false'                                       ;

list    = `[' { literal `,' }                                 `]' ;
range   = `('   literal `..' [ [ literal ] [ `..' literal ] ] `)' ;
\end{spec}


Note that although other values exist in the language (namely handles)
and have a show syntax, they have no read syntax and can thus only be
obtained through the use of built-in functions and language features.


\section{Data}
\label{sec:data}

% FIXME: Better phrasing please!
Fundamentally, the goal of a program is to process data in some way,
producing new data, representing data or transforming old data. To
accommodate working with data, Angle supports several types of data.

\begin{tabular}{ l p{7cm} l }
Datatype  & Use & Example \\
String    & Representing sets of Unicode characters & @"string"@ \\
Integer   & Representing arbitrarily large integral values & @42@ \\
Float     & Representing floating-point values & @12.15@ \\
List      & Grouping values, lists in Angle are heterogeneous meaning
that different types of data may be stored within a single list & @[1, "string", true]@ \\
Boolean   & Representing truth values & @true@ \\
Character & Representing individual Unicode characters & @'c'@ \\
Range     & Representing an enumeration across values of a certain type & @(1..7)@ \\
Null      & Special void value when a value must be returned but it doesn't make sense to return anything else & @null@ \\
Lambda    & Representing function bodies & @(() 1;)@ \\
Keyword   & Representing constant names without strings & @:keyword@ \\
Handle    & Referencing file descriptors & @{handle: file}@ \\
\end{tabular}

More complex data structures can be built out of these basic types.
For example, a conventional hash or dictionary could be represented
by a list of lists, each of length two with a keyword as the key
and any other data as the value.

\begin{spec}
hash = [[:key1, "value1"],
        [:key2, 200]
        [:key3, (() null;)]
        ]
\end{spec}

Then, using parameter constraints (see Section~\ref{ssub:parameters})
one could define a predicate that determines whether a list represents
a hash, and then be used as a constraint on any functions that should
only accept a hash.
\\
Of course this would not be nearly as efficient as the hash implementation
in many languages, but the principle of being able to build more
complex data from the standard set of types still holds.


\section{Language Features}
\label{sec:language_features}

As Angle is intended to be used as a general-purpose programming
language, it is important that the features it provides can facilitate
a variety of uses.
\\
Some features are outlined below, with more in-depth explanations in
section~\ref{sec:defining_the_language_grammar} and the documentation.


\subsection{Functions}
\label{sub:functions}


Functions and subroutines (when they don't have an explicit return
value) are essentially blocks of code that can be reused. Angle
supports functions in two forms: as bare lambdas (the actual block
of code) and function-variables. There is no difference between the
call-signature-body of a function and a lambda, but in general use,
when a lambda has been given a name (through either explicit
assignment or the @defun@ statement) it is referred to as a function.

\subsubsection{Parameters}
\label{ssub:parameters}

\paragraph{Types of parameters}
\label{par:types_of_parameters}


Parameters (the variables that take on the values of arguments) in
Angle come in two forms, each with optional modifiers.
\\
The first form is the standard parameter, these are position
dependent and will take on the value of the argument placed at the
equivalent position in a function call.
\\
The second form is a catch parameter; catch parameters, when defined,
will collect any additional additonal arguments into a special list
that can be accessed normally as a list, or expanded within a
function call to pass in the collected arguments. Catch parameters
allow the implementation of variadic functions.

\paragraph{Modifiers}
\label{par:modifiers}

As mentioned, parameters can have certain modifiers applied to them.
These too come in two flavors: annotations and constraints.
\\
Annotations allow the programmer to quickly state whether the
parameter should be a function, literal or any value. This makes it
easier to reason about higher-order functions and quickly see where
functions should be passed in - as well as having the function reject
invalid arguments.
\\
Parameter constraints (also known as classes in Angle) are references
to functions attached to a parameter. When an argument is passed to
the function and the parameter in that position has a constraint, the
argument is passed to the constraint function.
\\
The function then acts as a predicate, and should return true if the
argument satisfies the constraint and false otherwise.
\\
There are two main restrictions on functions when used as parameter
constraints: firstly, they must be able to accept at least one
argument, but may accept more (if there are more positional
parameters, then the constraint must be passed additional arguments).
Secondly, the function \textbf{must} return a boolean value when
passed the arguments. Of course, the function may happen to return a
boolean when passed one value but not others - thus only functions
intended to be used as predicates should be used as constraints.
\\
As there may be times when it would make sense for a function to act
one way when used as a predicate, but in a different way when used
otherwise (for instance, @int@ could determine whether the passed
value is an integer \textit{or} attempt to convert the value to an
integer), a built-in variable @as_class@ is provided that is true
when the current execution context is as a constraint, and false
otherwise; this allows a function to easily handle both use as a
constraint and standard function.
\\
Additionally, when called with a prefix @@\@@, a function will be
called in a class context.


\subsubsection{Closures}
\label{ssub:closures}

Closures are functions that contain a snapshot of the scope at the
time of their creation.
\\
Closures in Angle are produced by returning a function (or lambda)
from within a function.

\begin{spec}
  x = 7;
  defun foo() {
    defun bar(y) {
      return (+ x y);
    }
    return \$bar;
  }

  fun = foo();

  fun(1);
  # > 8

  x = 5;

  fun(1);
  # > 8

  fun2 = foo();

  fun2(1);
  # > 6
\end{spec}

% TODO: Check this!
In the above example, @foo@ returns a closure @bar@ which takes a
single argument and returns the sum of its argument and the value of
@x@ as it was when the closure was produced.
\\
When @fun@ is defined, the global value of @x@ is 7, thus in the
scope of the closure, global @x@ will always equal 7.
\\
When @x@ is redefined later on, this does not affect the value of @x@
that is used by the closure assigned to @fun@. But when @fun2@ is
defined, as the scope that @bar@ captures is different, so will its
value of @x@.

\paragraph{Declaring closures}
\label{par:declaring_closures}

As mentioned above, closures can be obtained by returning a lambda
from a function. There is another method; using the dollar operator
before a bare lambda will declare the lambda to be a closure, and
capture scope appropriately.
\\
Thus:
\begin{spec}
defun foo() {
  return (() x;);
}

bar = foo();
\end{spec}
Is the same as
\begin{spec}
bar = \$(() x;);
\end{spec}


\subsubsection{Accessing lambdas}
\label{ssub:accessing_lambdas}

As every variable in Angle can have both a function \textit{and}
value definition, \textit{and} functions are first-class citizens,
this means that occasionally the lambda associated with a variable
may need to be retrieved.
\\
Angle provides the dollar operator (@\$@) that when used on an
identifier, will return an expression representing the lambda stored
in the variable.


\subsubsection{Producing values from functions}
\label{ssub:producing_values_from_functions}

% FIXME: Fix the phrasing.
By default, when no return value is specified in a function, the last
produced value will be returned. Otherwise, the @return@ statement can
be used to exit early from the function and set the produced value to
that specified.


\begin{spec}
defun foo() {
  1;
}

foo = (() 1;);
# Equivalent to above.

foo = 2;


foo;
# 2

foo();
# 1

\$foo;
# (() 1;)
\end{spec}


@foo@ is defined as both a function (that just returns the integer 1)
and a value (the integer 2).

When using @foo@ as an expression, without calling it, it just
returns the value definition. Likewise, when called as a function it
returns the expected value 1. However, when the dollar operator is
used, the function's definition is returned.

\subsubsection{Grammar}
\label{ssub:grammar}

\paragraph{Function calls}
\label{par:function_calls}

\begin{spec}
  function_call = [ `@'] simple_ident `(' { expr `,' } `)' ;
\end{spec}

The optional @@\@@ sign in front of the identifier indicates whether
to call the function as a constraint or a regular function.

\paragraph{Function definition}
\label{par:function_definition}


\begin{spec}
function_def = simple_ident `(' { parameter `,' } `)' stmt                  ;

parameter    = [ `!' | `\$' | `..' ] simple_ident [ `:@' simple_ident ] ;
\end{spec}


\subsection{Variables}
\label{sub:variables}

Variables reference a location in memory that represents the data
associated with them. In Angle, variables can have two sets of data:
the first is a non-function value, such as an integer, string etc..;
and the second is a lambda that represents the variable as a function
 (see Section~\ref{ssub:accessing_lambdas} on how to access this
 value).
\\
\\
% TODO: Add scope stuff.
Angle is lexically scoped - meaning that the location at which a
variable is defined determines where it can be accessed.
\\
Any non-global region has access to two scopes: the local scope and
the outer scope.

When resolving variables, Angle first checks the current scope, then
recursively searches through the parent scopes until either the
global scope has been checked, and no definition for the identifier
found, or a definition is found.


% TODO: Check the phrasing of this section.
\subsubsection{Assignment}
\label{ssub:assignment}

Angle supports three assignment operators: @=@, @|=@, and @||=@; which
represent local assignment, nonlocal assignment and global assignment
respectively.
\\
When assigning to the local scope, if a definition for the variable
exists then it is overwritten, otherwise the variable is newly
defined.
\\
When assigning to a nonlocal scope, first the given identifier is
resolved in the local-most scope that is a parent of the current
scope, then this variable is overwritten with the given value. If no
nonlocal variable exists for the identifier, then the operation fails
with an exception.
\\
When assigning to the global scope, the same process as for local
assignment is performed, but in the global scope instead of the
current scope.


\subsection{Exceptions and exception handling}
\label{sub:exceptions_and_exception_handling}


\paragraph{Handling exceptions}
\label{par:handling_exceptions}


Exceptions arise as a result of any of a number of things going
unexpectedly in a program; these can range from types being used in
incorrect places and functions being called with the wrong number of
arguments, to a requested file not being found on the filesystem or
attempting to access a closed handle.
\\
The above exceptions are examples of those that can be raised by the
interpreter during run-time; there is an issue with this system of
exceptions however, the program would crash every single time an
error occurred.
\\
To stop these exceptions reaching the user, Angle provides a
@try...catch@ structure that allows the programmer to wrap a body of
code with the @try@, and any exceptions that occur while executing
that code are passed to the @catch@ for processing.
\\
Just after @catch@, the programmer specifies which categories of
exceptions can be caught by the supplied body, allowing only certain
types of exception to be stopped and others to be allowed to
propagate upwards.
\\
An common example of @try..catch@ usage is with handling user input;
this is usually a case that shouldn't crash the program, so it makes
sense to handle it.

\begin{spec}
  try {
    user_input = input("Enter an integer: ");
    res = asType(1, user_input);
  }
  catch :read {
    print("That wasn't an integer!");
    break :try;
  }
\end{spec}

In this scenario, an attempt is being made to convert the user's
input (string) to an integer; when the user's input does not
represent a valid integer string, a @:read@ exception is thrown. This
is then caught by the @catch@ and the user is notified of their
mistake. The @break :try@ statement at the end is a special form of
the @break@ statement (see section~\ref{sub:looping_structures}) that
just repeats the @try..catch@, as this handle-user-input-repeat form
is quite common.


\paragraph{User exceptions and re-raising}
\label{par:user_exceptions_and_re_raising}

Although catching exceptions is very useful itself, sometimes it may
be useful to throw the exceptions.
\\
For example, what about the scenario where the `no such file'
exception \textit{should} be fatal, but some cleanup should be
performed before the program exits? Well, Angle provides the @raise@
statement, which, when used with the currently-handled exception,
will just re-raise the same exception.

\begin{spec}
  try {
    my_handle = open("some_file.txt", "<");
  }
  catch :noSuchFile {
    ... cleanup ...
    raise :noSuchFile;
  }
\end{spec}

Of course, @raise@ isn't just limited to existing exceptions, the
programmer may specify their own exceptions instead, although as the
user-exception system is rather limited, the only information
available to those catching will be the name.

\begin{spec}
  try {
    for i in (1..10) do {
      if (> i 5) then {
        raise :greaterThan5;
      }
    }
  }
  catch :greaterThan5 {
    print("Got a number larger than 5!");
  }
\end{spec}

In this case the user exception is @:greaterThan5@, and is handled
directly.

\subsubsection{Grammar}
\label{ssub:grammar}

\begin{spec}
stmt_raise = `raise ' litKeyword ;
\end{spec}


\subsubsection{Catching exceptions}
\label{ssub:catching_exceptions}

Angle provides a construct for handling exceptions. A statement is
wrapped in @try@ and then any exceptions that occur during the
execution of this statement are passed to the following @catch(s)@.
\\
Each @catch@ specifies either a single keyword or a list of keywords
that represent the exceptions that should be handled by the
accompanying statement. If the exception matches that of any of those
that the @catch@ can handle, then the provided statement is executed
and no further @catch(s)@ will be executed.


\begin{spec}
  stmt_try_catch = `try '   stmt catch_spec { catch_spec } ;

  catch_spec     = `catch ' catch_keyword stmt             ;

  catch_keyword  = litKeyword | `[' { litKeyword `,' } `]' ;
\end{spec}


\subsection{Looping Structures}
\label{sub:looping_structures}

Repeated execution of a block of code can generally be achieved by
two methods: recursion and iteration, both of which are supported in
Angle.

\subsubsection{Recursion}
\label{ssub:recursion}

Recursion is achieved by a function self-calling with reduced
arguments. A base case exists which when satisfied will return a
well-formed value.
\\
\textit{For the @factorial@ function, the base case is when the argument
equals 0, and the value being passed in is reduced by 1 each time.}
\begin{spec}
  defun factorial(n) {
    if (== n 0) then return 1;
    return (* n factorial((- n 1)));
  }
\end{spec}


\subsubsection{Iteration}
\label{ssub:iteration}

Angle supports two constructs for iteration: @for@ loops and @while@
loops.
\\
\paragraph{For loops}
\label{par:for_loops}

For loops, when given a enumerable value such as a list or range,
will pass over the contained values, assigning each element to a
temporary variable for access in the body.
\\
% FIXME: Don't really like this, choose a better example.
\textit{Welcoming several people with the use of a for loop.}
\begin{spec}
names = ["Jannet", "Harry", "Theo"];

for name in names do {
  print("Welcome ", name);
}
\end{spec}

\paragraph{While loops}
\label{par:while_loops}

Unlike their @for@ loop counterparts, which traverse a series of
values, @while@ loops execute until some condition is met.

\begin{spec}
  age = 7;
  while (< age 18) do {
    print("Not an adult yet!");
    age = (+ age 1);
  }
\end{spec}


\subsubsection{Controlling loops}
\label{ssub:controlling_loops}

There are times at which it may be useful to exit a loop before it
would naturally exit, or skip the rest of the current execution.
\\
% TODO: Better wording...
The @break@ and @continue@ statements provide support for these cases
respectively.


\paragraph{Break}
\label{par:break}

The @break [val]@ statement ends the execution of the current looping
structure, and sets the value produced to @val@, when supplied.
% TODO: Maybe add link to the section where break :try is used (or
% mention special cases of :break)


\paragraph{Continue}
\label{par:continue}

@continue@ skips the rest of the current loop iteration, causing the
looping structure to start its next cycle.



\subsection{Input and Output}
\label{sub:input_and_output}

% FIXME: Want better wording for this.
Although the logic of a program can be defined in terms of pure
functions that do not interact with the outside world - a non-library
program must perform some input-output operations in order to be
useful.
\\
Angle provides several functions for IO operations.


\subsubsection{Interactive with the terminal}
\label{ssub:interactive_with_the_terminal}

A common need in scripts written for use in the terminal is to be able
to perform basic interaction with the user. Angle facilitates this
with the built-in @print@ and @input@ functions.
\\
\begin{description}
  \item[@print(string)@] prints the given string to @stdout@.
  \item[@input(string)@] prints @string@ to @stdout@ before returning
  the response from @stdin@.
\end{description}

A great deal of basic user interaction can be achieved just through
the use of these two functions.

\subsubsection{Handles}
\label{ssub:handles}

Angle makes use of handles to perform IO operations. A handle is a
reference to a resource on the system, such as a file descriptor which
provides access to some IO resource for reading and/or writing.

\paragraph{Standard streams}
\label{par:standard_streams}

On Unix systems there exist three standard file descriptors to access
the three standard streams: @stdin@ is the input stream; @stdout@ is
the output stream; and @stderr@ which is used for error messages.
\\
Angle provides handles to these three descriptors by default, in the
form of the variables @stdin@, @stdout@ and @stderr@.

\paragraph{Obtaining handles}
\label{par:obtaining_handles}

Angle provides the built-in @open@ function, which takes the form:
@open(file_name, access_mode)@, and returns a handle providing
access to the file @file_name@ in the specified @access_mode@.
\\
There are two main access modes in general: read and write.
Write is further split into append and write (clobber).
`Write (clobber)' is used when the contents of the shouldn't be
preserved and will overwrite the contents of the file when writing.
`Append' on the other hand will preserve the contents and add the
new text to the end of the file.
\\
Angle provides four possible access modes for handles: read
(represented by @"<"@), write (@">"@), append (@">>"@), and
read-write (@"<>"@).

\paragraph{Reading handles}
\label{par:reading_handles}

Angle's built-in @read@ function provides various means of reading
from a handle's character stream. @read(handle)@ reads the entirety
of the remaining text, @read(handle, int)@ will read @int@ lines,
then the modifier @:char@ can be appended to the call to read
individual characters.

\paragraph{Writing to handles}
\label{par:writing_to_handles}

% TODO: Check the wording.
As mentioned previously, there are two main write-modes that can be
used with handles in Angle; these produced the stated effects when
used with the @write@ function, which takes a handle and some text
and writes the text to the handle.

\paragraph{Closing handles}
\label{par:closing_handles}

When a handle is no longer in use it can be closed for reading and
writing with the @close@ function. It is advisable to explicitly close
handles when they are no longer needed to free up file descriptors and
allow them to be accessed by other operations later on.


% \subsubsection{Basic functions for user interaction}
% \label{ssub:basic_functions_for_user_interaction}
%
% A basic commandline program could easily get away with using just two
% of Angle's IO functions for user interaction: @print@ and @input@.
% \\
% \paragraph{Print}
% \label{par:print}
%
% The @print@ function takes some text and prints it to the @stdout@
% handle

\subsection{Including code from other files}
\label{sub:including_code_from_other_files}

It is essential for code-reusability and the creation of libraries to
be able to load code from other files in a program. The ability to
do this means that sets of functions can be defined in one file, then
used by many other programs such that the functions do not need to be
written again.
\\
There are two main methods of achieving this in Angle:

\subsubsection{Eval}
\label{ssub:eval}

% TODO: Want better phrasing.
The @eval@ built-in function takes a string and attempts to parse it
as code - this can be useful for loading small sections of source
from a trusted location on the fly.
\\
There are a few issues with this - any use of @eval@ with user input
is risky, as there is the potential for malicious code to be injected,
instead the @asType@ built-in should be used for converting strings
to other types.

\subsubsection{Include}
\label{ssub:include}

The best method of loading entire files is the built-in @include@
function. @include@ takes a filename (or handle) and attempts to
execute the contained text. Syntax errors are handled by @include@.
\\
@include@, when passed a filename, will first check the standard Angle
library locations for the specified file, then the relative path. If
the given filename happens to be a relative or absolute path
(starts with @.@, @/@ etc..) then the library locations will not be
checked.


\subsection{Operations}
\label{sub:operations}


Operations (consisting of operators and operands) are the fundamental
means of manipulating data in Angle.
\\
Angle supports three types of operator: infix binary, prefix unary
and prefix variadic. The infix binary operators (such as @=@) take
two operands, one on either side. The prefix unary operators take a
single operand and are placed before this operand. The prefix variadic
operators are used as the first symbol within parentheses, and take
a non-set number of operands, separated by whitespace, until the
closing parenthesis.

\subsubsection{Operator types}
\label{ssub:operator_types}

Operators in Angle mainly come under four categories: arithmetical,
logical, relational and assignment.
\\
Arithmetical operators, which are generally variadic, perform
mathematical arithmetic operations such as addition, multiplication
and division.

\begin{spec}
(+ 1 2 3);
# 6

(- 1 2 3);
# -4
\end{spec}

As the above shows, the grouping is from left to right, thus
@(- 1 2 3)@ becomes @((1 - 2) - 3)@
\\

Logical operators perform logical operations on booleans and are also
mostly variadic.

\textit{Logical OR}
\begin{spec}
(| false false true);
# true
\end{spec}

Relational operators perform comparison between different types, all
the relational operators are variadic.

\begin{spec}
(< 1 2 3);
# true

(>= 1 2 3);
# false
\end{spec}

There is a pair-wise grouping with relational operators, thus
@(< 1 2 3)@ becomes @1 < 2 AND 2 < 3@, or @1 < 2 < 3@.
\\
Assignment operators are all infix binary, and the use-case is always
the same; associate some data with an identifier. See
section~\ref{ssub:assignment} for a more detailed explanation on how
assignment works.
\\
The arithmetical, logical and relational operators are all expression
operators, meaning that they act upon expressions and produce
expressions, without any side-effects. The assignment operators do
produce side-effects however, namely changing the value that an
identifier references, thus the assignment operators are statements,
not expressions.

\subsubsection{Grammar}
\label{ssub:grammar}

\begin{spec}

operation =     unop    expr
          | `(' varop { expr } `)'         ;

unop      = `^' | `-'                      ;

varop     = `+' | `-'  | `/' | `**' | `*'
                | `|'  | `&' | `>='
                | `<=' | `>' | `<'  | `==' ;
\end{spec}


\subsection{Conditionals}
\label{sub:conditionals}

A common feature to almost all programming languages is a structure
for conditionally evaluating code based on the result of an
expression. The canonical example of this is the `if' conditional,
that executes the accompanying body if the given expression evaluates
to `true', an `else' form is also usually present.
\\
Angle is no exception to this trend and implements its own conditional
statements: `if', and its counterpart `unless'.

\subsubsection{Grammar}
\label{ssub:grammar}

\begin{spec}
stmt_condition = cond_if | cond_unless                     ;

cond_if        = `if'     expr `then' stmt [ `else' stmt ] ;
cond_unless    = `unless' expr        stmt [ `else' stmt ] ;
\end{spec}


\paragraph{Relevant modules}
\label{par:relevant_modules}

\texttt{Angle.Types.Lang} and \texttt{Angle.Parse.Parser.Internal} implement the
language grammar in terms of Haskell types and functions.

\paragraph{Overview}
\label{par:overview}

% The following provides a brief outline of a simple language grammar
% intended to represent some of the features of Angle. The grammar is
% not complete and is only intended to give an overview of Angle's
% syntax.

The following provides a brief outline of Angle's language grammar,
written in a slightly modified Extended Backus Naur form, along with
an overview of some of the features Angle provides.

\section{Defining the language grammar}
\label{sec:defining_the_language_grammar}


\subsection{Statements}
\label{sub:statements}

The building blocks of Angle programs are statements; statements
themselves being made of assignments, expressions and language
constructs.

\begin{spec}
stmt        = single_stmt  | multi_stmt                    ;

single_stmt = function_def | stmt_expr      | stmt_control
            | stmt_loop    | stmt_condition | stmt_assign
            | stmt_raise   | stmt_try_catch                ;

multi_stmt  = `{' { stmt } `}'                             ;
\end{spec}

\subsubsection{Assignment}
\label{ssub:assignment}

Assignment binds the result of an expression to a name
(the identifier).
\begin{spec}
stmt_assign = simple_ident `=' expr ;
\end{spec}

\subsubsection{Looping structures}
\label{ssub:looping_structures}

Looping structures allow iterating over certain values or until a
condition is satisfied.

\begin{spec}
stmt_loop  = loop_for | loop_while             ;

loop_for   = `for'   ident `in' expr `do' stmt ;
loop_while = `while'            expr `do' stmt ;
\end{spec}



\subsubsection{Control statements}
\label{ssub:control_statements}

Control statements help the programmer to control the flow of a
program via allowing code to be skipped when it would otherwise
be executed.

\begin{spec}
stmt_control     = control_return
                 | control_break
                 | control_continue    ;

control_return   = `return'   [ expr ] ;
control_break    = `break'    [ expr ] ;
control_continue = `continue'          ;
\end{spec}






\subsection{Expressions}
\label{sub:expressions}

Expressions are blocks of code that can be evaluated
to produce some value.\footnote{https://msdn.microsoft.com/en-us/library/ms173144.aspx}

\begin{spec}
expr = operation  | literal   | function_call
     | identifier | expr_list | expr_range    ;
\end{spec}

Note the inclusion of @expr_list@ and @expr_range@ which are
represented as literals in section~\ref{ssub:literals}.

See section~\ref{ssub:lists_and_ranges_as_expressions} for more
information.



\subsubsection{Lists and ranges as expressions}
\label{ssub:lists_and_ranges_as_expressions}

The reason for having lists and ranges defined as both literals and
expressions is to enable lists and ranges that have indeterminate
contents at run-time. Angle first attempts to parse a literal list
or range, meaning that if the programmer hard-codes a list containing
only literals, it will be treated as a literal when the file is being
lexed. If any part of the list or range is not a literal value, and
thus has no definite value, Angle will treat it as an expression
rather than a literal so that the contents are only known when the
list or range is evaluated.

\begin{spec}
expr_list    = `[' { expr `,' }                           `]' ;
expr_range   = `('   expr `..' [ [ expr ] [ `..' expr ] ] `)' ;
\end{spec}



\part{Creating Angle}
\label{prt:creating_angle}

\section{How it should work}
\label{sec:how_it_should_work}

The basic sequence of executing a program in Angle should be as
follows:

\begin{enumerate}
  \item The programmer writes a program using Angle syntax.
  \item The source file is run using the `angle' program.
    \begin{enumerate}
      \item The source file is checked for syntax errors and read
        into Haskell datatypes.
        \begin{enumerate}
          \item If any malformed structures exist, the program will
            halt and alert the runner.
        \end{enumerate}
      \item The program is executed.
        \begin{enumerate}
          \item Run-time errors may be encountered; if run-time errors
            occur then the program will halt and the runner will be
            notified.
        \end{enumerate}
    \end{enumerate}
  \item The program exits and memory is freed.
\end{enumerate}

To achieve this method of execution the project is split into
three sections.

\begin{description}
  \item[executable] for running programs, the main interface for the
    user. See section~\ref{sub:using_angle} for more information.
  \item[parser] which will deal with translating source code into
    an abstract syntax tree that represents the language.
  \item[interpreter] which executes the AST produced by the parser
    and performs IO actions.
\end{description}

\section{Parser}
\label{sec:parser}

In Angle the parser does the job of converting source text into a
format that can be executed by the interpreter. Part~\ref{prt:grammar}
defines the form this source can take.
\\
`The parser' mainly refers to two things:

\begin{description}
  \item[@Parser a@ monad] - which is a stack of monads which produces
    a value of type @a@ from an input stream. See section~\ref{sub:defining_the_parser}.
  \item[@Angle.Parse@ modules] - a collection of modules which use the
    @Parser a@ monad to convert the input stream into an abstract
    syntax tree representing an Angle program.
\end{description}

\subsection{Scanner}
\label{sub:scanner}

\paragraph{What is a Scanner?}
\label{par:what_is_a_scanner_}

A scanner is the component of a language that reads in characters
from the source file one at a time and hands them to the lexer to be
converted to tokens.\footnote{http://forums.devshed.com/programming-languages-139/interpreter-compiler-312483.html\#post1342279}
The scanner is able to keep track of its current position in order to
make it easier for the lexer to form these tokens.

\paragraph{The basics}
\label{par:the_basics}

There are two main requirements for the scanner:
\begin{itemize}
  \item It is able to read characters from source.
  \item It is able to indicate the position at which these characters
  were parsed.
\end{itemize}

\subsubsection{Scanner type}
\label{ssub:scanner_type}

\paragraph{Relevant modules}
\label{par:relevant_modules}

\texttt{Angle.Scanner} defines the scanner.


\paragraph{Source Position}
\label{par:source_position}

As stated previously, the scanner needs to be able to keep track
of its current position in source. It is convenient to use a newtype
wrapper of a triple of integers to provide a solid type to hold this
information.
\\
The reason for having three integers is that it is more useful
to refer to the line and column (first two values) numbers in
error messages and debugging. For practical reasons, the total
character index is recorded as the third element.
\begin{spec}
newtype SourcePos = SourcePos
    { getSourcePos :: (Int, Int, Int) }
\end{spec}

\paragraph{Tracking position}
\label{par:tracking_position}

This is not enough to be able to advance position whilst yielding
characters however, thus the scanner must have a type similar to:

\begin{spec}
SourcePos -> (v, SourcePos)
\end{spec}

That is, providing it with a new position in source allows it to
determine a new value and advance position. \\
This type signature actually represents a stateful computation -
something that can be represented more usefully by using the
existing State monad.\footnote{https://hackage.haskell.org/package/mtl-1.1.0.2/docs/Control-Monad-State-Lazy.html}\\
The State monad has a type signature\footnote{This is actually
incorrect - the State Monad is infact a StateT using Identity as the
base monad (but for the purpose of this document it is safe to treat
State as a newtype wrapper) https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-State-Lazy.html\#t:State} of:

\begin{spec}
newtype State s a = State { runState :: s -> (a, s) }
\end{spec}

And from the signature for the scanner, we could then replace the `s'
with `SourcePos' and produce:

\begin{spec}
type Scanner = State SourcePos
\end{spec}


\subsection{Defining the parser}
\label{sub:defining_the_parser}

\paragraph{Relevant modules}
\label{par:relevant_modules}

\texttt{Angle.Scanner} defines the parser type. \texttt{Angle.Parse.Parser},
\texttt{Angle.Parse.Helpers} and \texttt{Angle.Parse.Token} implement the parser
functions. \texttt{Angle.Types.Lang} defines language structures in terms
of Haskell types.

\subsubsection{The parser type}
\label{ssub:the_parser_type}

Having defined the scanner type to be synonymous with
@State SourcePos@, and the knowledge that the scanner will be
integrated with the parser to provide positional information, as well
as access to characters from a stream, the parser type can start to be
defined.

\paragraph{Representing the parser}
\label{par:representing_the_scanner}

As the exact types of values that the parser will be expected to
produce may vary, its type has to be polymorphic.
\\ \\ \textit{The parser consisting of just the scanner component.}
\begin{spec}
type Parser a = State SourcePos a
\end{spec}

% TODO: I think we want a Reader in here (to pass string around)
% thus, maybe a monad transformer? Reader and State

There is one major issue with this type; after having created a few
tokenizer functions in the `Parser' module, I noticed that I was
having to pass strings (the source code) to many of the functions.
\\ \\ \textit{Example: the `integer' function for parsing a single
integer requires a string in order to perform its task}
\begin{spec}
integer :: String -> Parser Integer
\end{spec}
The source code being passed in to these functions could be referred
to as the `environment' - a static piece of information which they
require access to in order to evaluate. Luckily, there exists a
standard monad for representing computations to which an environment
is passed - the Reader monad.\footnote{https://hackage.haskell.org/package/mtl-1.1.0.2/docs/Control-Monad-Reader.html}
\\
The Reader monad has a type of:
\\ \\ \textit{`runReader' takes an environment of type `e' and unwraps and
evaluates the computation inside the Reader.}

\begin{spec}
newtype Reader e a = Reader { runReader :: e -> a }
\end{spec}

Which allows us to represent the source code environment by the
type:
\begin{spec}
type SourceEnv = Reader String
\end{spec}
But the problem with the parser is still not solved, the SourceEnv
and Parser need to be somehow merged together in order to allow the
passing of both state and environment.
\\
Haskell provides a convenient means of doing this - via monad
transformers.\footnote{https://hackage.haskell.org/package/transformers}
Monad transformers allow the combination of different monads into
a single monad that can access the functionality of each.\footnote{http://book.realworldhaskell.org/read/monad-transformers.html}\footnote{https://en.wikibooks.org/wiki/Haskell/Monad\_transformers}
\\
This means that the use of a State Transformer with the Reader monad
will allow the combination of persistent environment and state.
\\ \\ \textit{Notice the scanner is still present in the stack, `StateT SourcePos'.}
\begin{spec}
type Parser = StateT SourcePos (Reader String)
\end{spec}
% TODO: Not sure about this bit!
This is good, but leaves the implementation a little exposed, which
can lead to issues later on.\footnote{http://book.realworldhaskell.org/read/programming-with-monads.html\#id646649}
\\
To hide the internals of the type, we can wrap the Parser in a
newtype declaration.
\begin{spec}
newtype Parser a = Parser
    { runParser :: StateT SourcePos (Reader String) a }
\end{spec}
`runParser' can be used to retrieve the monad stack from a Parser.
\\
One last thing to add is error handling, via the ExceptT monad
transformer.\footnote{Initially `ErrorT' was used, but due to
depreciation `ExceptT' was used instead - https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Except.html\#t:ExceptT}
\\
Thus the final type is:
\\ \\ \textit{`ParseError' contains information about exceptions that occur during parsing.}
\begin{spec}
newtype Parser a = Parser
    { runParser :: ExceptT ScanError (StateT SourcePos (Reader String)) a }
\end{spec}

Thus the base-most parser (scanner) can be represented as a
single function with the type @scanChar :: Parser Char@.

\subsubsection{Parsing Angle}
\label{ssub:parsing_angle}

Having a `Parser' type alone doesn't automatically allow the parsing
of Angle syntax. Angle must be described in Haskell's type system
and functions must be defined to parse each of the constructs.

The parsing functions are defined through three modules.
\texttt{Angle.Parse.Helpers} defines the most basic parsers for use in other
modules. \texttt{Angle.Parse.Token} defines parsers that deal with very basic
structures, such as strings and integers. \texttt{Angle.Parse.Parser} defines
the parsers that read Angle syntax into Angle types.

As an example of the process of defining a new Angle feature, I will
use string literals.

Functions from \texttt{Angle.Parse.Helpers}:
\begin{description}
  \item[@char     :: Char     -> Parser Char@] parses the specified character.
  \item[@manyTill :: Parser b -> Parser a -> Parser [a]@] a
    higher-order parser that parses until the first parser is
    satisfied.
  \item[@anyChar  :: Parser Char@] essentially the same as the base
    scanner function - parses any character.
\end{description}

Using these functions means that a parser could be defined in
\texttt{Angle.Parse.Token} for parsing a basic string.\footnote{There are a
lot of edge cases when parsing strings, see
\texttt{Angle.Parse.Token.tokString} for a better representation.}

\begin{spec}
string :: Parser String
string = char `"' *> manyTill (char `"') anyChar
\end{spec}

Which would parse some text surrounded by double quotes.

Then, assuming the type

\begin{spec}
data LangLit = ...
             | LitStr String
             | ...
\end{spec}

defined in \texttt{Angle.Types.Lang}, a parser can be implemented in
\texttt{Angle.Parse.Parser} for wrapping a Haskell string in an Angle string.

\begin{spec}
litStr :: Scanner LangLit
litStr = liftM LitStr tokString
\end{spec}

Then this process is repeated for any other structures that need to
be parsed.

\part{Conclusion}
\label{prt:conclusion}

% TODO: Update conclusion to reflect the addition of basic exception
% handling and file io (also maybe mention import through eval?)
% Also has the shell function.

Angle satisfies its initial design requirement as a general-purpose
programming language. It supports most major language features,
including subroutines, variables, looping and conditional structures,
exception handling and file IO.
\\
% Angle satisfies many of the initial design requirements; it supports
% most major language features (with the exception of imports):
% subroutines, variables, looping and conditional structures, file IO,
% and exception handling.
I believe that Angle is fit for purpose as a general-purpose
programming language. For small projects, the standard structures,
along with the support for input and output to files, as well as direct
calls to shell commands, means that most use-cases should be handled
directly.
\\
For larger projects, the ability to embed code from other files
through built-in functions should encourage the use of multiple source
files in a single project, and the creation of libraries to reduce
code duplication.
\\
Regarding the internal structure: the design of Angle allows for new
syntax, built-in functions, types and language structures to be defined
relatively easily. This extensibility means that creating new language
features in the future is a definite possibility.
% FIXME: ^ not sure about 'definite possibility'
\\
The documentation for the source code is satisfactory, and the use of
Haddock means that this documentation may be displayed in a
user-friendly manner.
\\
The use of Cabal has greatly sped up development of Angle, as changes
to directory structure need only be updated in the cabal file, and
cabal's support for benchmarks and testing aided in the development
work-flow.

\section{Implementation}
\label{sec:implementation_changes}

As is often the case, with hindsight I am aware of areas of the
implementation that may have been improved if certain knowledge was
available at the beginning of the project.
\\
For example, Haskell supports Generalized Algebraic Datatypes - a
system that allows you to explicitly state types signatures of the
constructors of a type.\footnote{https://downloads.haskell.org/~ghc/6.6.1/docs/html/users\_guide/gadt.html}
Knowledge of this when starting the project may have made it easier
and cleaner to come up with a representation for the language
structures.
\\
\\
In the early stages of the project there were some issues with the
testing frameworks - the sizes of the test-cases that QuickCheck was
generating were too large to be completed in a reasonable time. With
\texttt{test-framework}, the initial testing framework, I had great
difficulty controlling the size of individual test-cases. After
switching to \texttt{tasty} these issues were more easily dealt with.
\\
For future projects I shall have to do more research into testing
libraries to ensure they can cope with all my projects' needs before
commencing on the actual project itself.

\subsection{Errors and debugging}
\label{sub:errors_and_debugging}

An important part of any language is its ability to convey error
messages to the programmer, and provide support for debugging the
software.
\\
This was a design consideration from quite early in the project, and
I feel that Angle's error reporting system is satisfactory for the
project level. Angle provides no separate debugging utilities, bar the
interactive mode accessible through the software. In future projects
I would encourage a more ingrained error system, with much more
detailed messages (function calls that lead to the error, etc.,),
especially at the parsing stage.

\subsection{Type system}
\label{sub:type_system}

My choice was to have a dynamic type system for Angle - this was done
intentionally to reduce reliance on Haskell's type system to improve
my understanding of coping with non-static type systems.
\\
In the future, when this is not a goal in mind, I believe it would be
wise to allow Haskell to enforce type-correctness in any small
language implementations. This would not only reduce the required
effort, but also perhaps provide some speed increases and allow me to
better understand how to use existing type-systems to my advantage.


\section{Final comments}
\label{sec:final_comments}

After having now created a basic programming language, I feel I have
gained enough knowledge to study the topic more in-depth and implement
more small languages. I will perhaps revisit Angle in the future to
implement additional features and review how my methods have changed.

\end{document}
