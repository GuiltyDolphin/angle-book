\documentclass{article}

%include lhs2TeX.fmt
%include lhs2TeX.sty
%include polycode.fmt

% \usepackage[nounderscore]{syntax}
% \usepackage{underscore}

\title{An implementation of a basic general-purpose scripting language}
\author{Ben Moon}
\date{}

\begin{document}

\maketitle

\tableofcontents


\paragraph{How to read this document}
\label{par:how_to_read_this_document}

This document should be read in conjunction with the source files
provided. Examples of code presented in this document may not
reflect the final source, and should be assumed to be non-functional.
\\
Relevant modules will be stated before any sections describing the
development of code.

\part{Introduction}
\label{prt:introduction}

\section{The Language}
\label{sec:the_language}

\paragraph{What is Angle?}
\label{par:what_is_angle_}

Angle is intended to be a weak and dynamically typed, functional,
interpreted, general-purpose scripting language.

\subparagraph{Dynamic}
\label{par:dynamic}

Being dynamically typed means that type-checking in Angle is
performed at run-time,\footnote{http://c2.com/cgi/wiki?DynamicTyping}
thus programs are not guaranteed to be type-correct
(unlike in Haskell, where the compiler ensures that programs do not
compile if any inconsistencies in types are noticed).

\subparagraph{Weak}
\label{par:weak}

Angle has weak typing as types are not guaranteed to remain constant
during run-time.\footnote{http://c2.com/cgi/wiki?WeaklyTyped}
One example of this would be Angle's type casting capabilities
through one of the builtin functions. Using this function allows the
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

Angle is intended to be able to cope with the implementation of an
array of different types of programs; there is no specific use case
in mind that dictated the implementation.

\subparagraph{Interpreted}
\label{par:interpreted}

Interpreted languages differ from compiled languages in that instead
of programs first being translated to machine code before being
executed, instructions are translated to machine code and executed
at run-time, thus possibly incurring a speed decrease when compared
to a similar compiled language.\footnote{http://www.vanguardsw.com/dphelp4/dph00296.htm}

Choosing to make Angle an interpreted language rather than compiled
means that a compiler need not be written. It also means that
programs do not need to be compiled before they are run and can be
distributed as source without the need for any additional files.

\subparagraph{Scripting language}
\label{par:scripting_language}

As Angle does not have its own compiler and is interpreted, it
classifies as a scripting language.

\section{Project Overview}
\label{sec:project_overview}

\subsection{Components}
\label{sub:components}

Angle is defined by two components: the package and the executable.

The package contains the source files, and everything required to
configure, test and install the executable.

The executable is a program that can be used to run scripts written
using Angle's syntax. See section~\ref{sub:using_angle} `Using Angle'
for information on how this is used.


\subsection{Using Angle}
\label{sub:using_angle}

The `angle' program supports two main modes of execution: interactive
and non-interactive.

\paragraph{Interactive Angle}
\label{par:interactive_angle}

When Angle runs in interactive mode, the user is presented with a
prompt at which they may enter code line-by-line and have it execute
immediately. This mode allows a programmer to debug code, test short
snippets and ensure that a line consists of the correct syntax before
using it in a program.

\paragraph{Non-Interactive}
\label{par:non_interactive}

When Angle runs in non-interactive mode, achieved by passing a file
to be executed, a source file is read and run by the Angle
interpreter. This mode is how programs should usually be run.


\section{Features}
\label{sec:features}

Angle supports many features, such as looping and support for
functional programming, along with some more obscure features such as
parameter constraints.

Angle also has a lot of potential for more big features to be
implemented, some of which are largely essential to a good programming
environment - exception handling and imports. The way in which Angle
has been implemented and refined means that adding new features
requires modifying as little of the existing code as possible, and
instead promotes constructive modification.

Some features and where to find the respective information is shown
in the table below.

`Types' refers to a type defined in `Angle.Types.Lang' that provides
more information on the feature.

\begin{tabular}{l c r}
  Feature & Types & Sections \\
  \hline
  Conditionals & `LangStruct' & section~\ref{ssub:conditional_constructs} \\
  Parameter constraints & `ConstrRef' & section~\ref{ssub:function_definitions} \\
  Parameter annotations & `AnnType' & section~\ref{ssub:function_definitions} \\
  Looping structures & `LangStruct' & section~\ref{ssub:looping_structures} \\
\end{tabular}


\part{Grammar}
\label{prt:grammar}

\paragraph{Relevant modules}
\label{par:relevant_modules}

`Angle.Types.Lang' and `Angle.Lex.Lexer.Internal' implement the
language grammar in terms of Haskell types and functions.

\paragraph{Overview}
\label{par:overview}

The following provides a brief outline of a simple language grammar
intended to represent some of the features of Angle. The grammar is
not complete and is only intended to give an overview of Angle's
syntax.

\section{Defining the Language Grammar}
\label{sec:language_grammar}

\paragraph{Extended Bakus Noire Form (EBNF)}
\label{par:extended_bakus_noire_form_ebnf_}

EBNF is an extended version of Bakus Noire Form, a notation that can be
used to express the grammar of formal languages.\footnote{http://www.garshol.priv.no/download/text/bnf.html\#id1.2.}

BNF can be used to decribe context-free grammars\footnote{http://matt.might.net/articles/grammars-bnf-ebnf/},
which are grammars that consist of names and expansions
(the components), meaning that it may be used to express a grammar for
Angle.

\subsection{Statements}
\label{sub:statements}

The building blocks of Angle programs are statements; statements
themselves being made of assignments, expressions and language
constructs.

\begin{spec}
stmt        = single_stmt  | multi_stmt                   ;

single_stmt = function_def | stmt_expr      | stmt_control
            | stmt_loop    | stmt_condition | stmt_assign ;

multi_stmt  = `{' { stmt } `}'                            ;
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

\subsubsection{Conditional constructs}
\label{ssub:conditional_constructs}

Conditionals allow the programmer to control which parts of code get
executed based on the boolean result of expressions.

\begin{spec}
stmt_condition = cond_if | cond_unless                  ;

cond_if     = `if'     expr `then' stmt [ `else' stmt ] ;
cond_unless = `unless' expr        stmt                 ;
\end{spec}


\subsubsection{Control statements}
\label{ssub:control_statements}

Control statements are used to control the flow of the program via
returning early from functions and loops.

\begin{spec}
stmt_control     = control_return | control_break | control_continue ;

control_return   = `return'   [ expr ] ;
control_break    = `break'    [ expr ] ;
control_continue = `continue'          ;
\end{spec}


\subsubsection{Function definitions}
\label{ssub:function_definitions}

Function definitions allow the programmer to assign a lambda to an
identifier in a semantically clear manner.

\paragraph{Parameters}
\label{par:parameters}

Angle supports certain annotations to parameters when defining a
function that allow the programmer to restrict the types of values
that a function will accept.

See `Angle.Types.Lang.ConstrRef' and `Angle.Types.Lang.AnnType' for
more information.

% FIXME: '$' character needs escaping to have correct syntax
% highlighting - but this shows up in the pdf.
\begin{spec}
function_def = simple_ident `(' { parameter } `)' stmt ;

parameter = [ `!' | `\$' | `..' ] simple_ident [ `:@' simple_ident ] ;
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

\subsubsection{Operations}
\label{ssub:operations}

Operations consist of operators and operands.
The operator determine the type of operation to perform
and the operands are the values to perform the operation on.
\\
Operators in Angle are split into two types: unary and variadic.
The unary operators are prefix and take a single argument, whereas
the variadic operators are prefix within parentheses and can take
a variable number of arguments.
\begin{spec}

operation = unop expr | `(' varop { expr } `)' ;

unop      = `^' | `-' ;

binOp     = `+' | `-'  | `/' | `*'
                | `|'  | `&' | `>='
                | `<=' | `>' | `<' | `==' ;
\end{spec}


\subsubsection{Literals}
\label{ssub:literals}

Literals allow the programmer to specify exact values in Angle that
will remain constant through separate runs of the program, provided
the source code is not modified.\footnote{https://www.cs.cf.ac.uk/Dave/Multimedia/node71.html}

The following are referenced below but not defined:

% TODO: Might want a citation - or a better way of linking to the
% docs.
\begin{itemize}
  \item @string_char@ - which is any character accepted in a Haskell
    String. A special type of string exists (by prefixing `e') in
    Angle that treats backslashes literally and thus will not
    interpret escape characters.
  \item @char_char@ - which is any character accepted in a Haskell
    Char.\footnote{https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Char.html\#t:Char}
\end{itemize}

\begin{spec}

literal = number | list | string | char | boolean | range     ;

number  = integer | float                              ;

integer = [ `-' ] digit { digit }                      ;
float   = [ `-' ] digit { digit } `.' digit { digit }  ;

boolean = `true' | `false'                             ;

char    =         `''   char_char     `'' ;
string  = [ `e' ] `"' { string_char } `"' ;

list    = `[' { literal `,' }                                 `]' ;
range   = `('   literal `..' [ [ literal ] [ `..' literal ] ] `)' ;
\end{spec}

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
expr_list =    `[' { expr `,' }                           `]' ;
expr_range   = `('   expr `..' [ [ expr ] [ `..' expr ] ] `)' ;
\end{spec}


\subsubsection{Function calls}
\label{ssub:function_calls}

All functions in Angle produce values when executed. This may be the
`null' value, or a well-defined value.

\begin{spec}
function_call = simple_ident `(' { expr } `)' ;
\end{spec}

\subsubsection{Identifiers}
\label{ssub:identifiers}

Identifiers represent names given to functions and values so that
they may be referred to elsewhere within the program.
\\
I refer to `alpha' and `digit' without formally defining them,
they refer to the characters of the roman alphabet (upper and lower; [`a'..`z'], [`A'..`Z'])
and the digits used in the denary system ([`0'..`9']) respectively.

\begin{spec}
identifier     = simple_ident  | function_ident ;

simple_ident   = alpha { alpha | digit }        ;
function_ident = `\$'    simple_ident           ;
\end{spec}

\part{Scanner}
\label{sec:scanner}

\paragraph{Relevant modules}
\label{par:relevant_modules}

`Angle.Scanner'

\section{Creating the Scanner}
\label{prt:creating_the_scanner}

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

\subsection{The types}
\label{sec:the_types}

\paragraph{Source Position}
\label{par:source_position}

As the scanner will need to be able to keep track of its position in
source, it is convenient to use a newtype wrapper of a triple of
integers to provide a solid type to hold this information.
\\
The reason for having three integers is that it is more useful
to refer to the line and column (first two values) numbers in
error messages and debugging. For practical reasons, the total
character index is recorded as the third element.
\begin{spec}
-- Represents a position in source.
newtype SourcePos = SourcePos
    { getSourcePos :: (Int, Int, Int) }
        deriving (Eq)
\end{spec}

\subsubsection{The scanner type}
\label{sub:the_scanner_type}

\paragraph{Representing the scanner}
\label{par:representing_the_scanner}

As it is unknown at this time the exact type of value that will be
returned by functions using the scanner, the scanner type will have
to be polymorphic.
It is known however, that the scanner will be keeping track of its
current position in the source file - I have already defined a type
for the position in the source file 'SourcePos'.
Therefore, the scanner could have a type of:
\begin{spec}
SourcePos -> (v, SourcePos)
\end{spec}
That is, providing it with a new SourcePosition allows it to
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

% TODO: I think we want a Reader in here (to pass string around)
% thus, maybe a monad transformer? Reader and State

There is one major issue with this type; after having created a few
tokenizer functions in the `Lexer' module, I noticed that I was
having to pass strings (the source code) to many of the functions.
\\ \\ \textit{Example: the `integer' function for parsing a single
integer requires a string in order to perform its task}
\begin{spec}
integer :: String -> Scanner Integer
\end{spec}
The source code being passed in to these functions could be referred
to as the `environment' - a static piece of information which they
require access to in order to evaluate. Luckily, there exists a
standard monad for representing computations to which an environment
is passed - the Reader monad.\footnote{https://hackage.haskell.org/package/mtl-1.1.0.2/docs/Control-Monad-Reader.html}
\\
The Reader monad has a type of:
\begin{spec}
-- `runReader' takes an environment of type `e' and unwraps and
-- evaluates the computation inside the Reader.
newtype Reader e a = Reader { runReader :: e -> a }
\end{spec}

Which allows us to represent the source code environment by the
type:
\begin{spec}
type SourceEnv = Reader String
\end{spec}
But the problem with the scanner is still not solved, the SourceEnv
and Scanner need to be somehow merged together in order to allow the
passing of both state and environment.
\\
Haskell provides a convenient means of doing this - via monad
transformers.\footnote{https://hackage.haskell.org/package/transformers}
Monad transformers allow the combination of different monads into
a single monad that can access the functionality of each.\footnote{http://book.realworldhaskell.org/read/monad-transformers.html}\footnote{https://en.wikibooks.org/wiki/Haskell/Monad\_transformers}
\\
This means that the use of a State Transformer with the Reader monad
will allow the combination of persistent environment and state.
\begin{spec}
type Scanner = StateT SourcePos (Reader String)
\end{spec}
% TODO: Not sure about this bit!
This is good, but leaves the implementation a little exposed, which
can lead to issues later on.\footnote{http://book.realworldhaskell.org/read/programming-with-monads.html\#id646649}
\\
To hide the internals of the type, we can wrap the Scanner in a
newtype declaration.
\begin{spec}
newtype Scanner a = Scanner
    { runScanner :: StateT SourcePos (Reader String) a
    } deriving (Functor, Applicative, Monad)
\end{spec}
`runScanner' can be used to retrieve the monad stack from a Scanner.
\\
As this is one of the base types of the system, it will be worth
making a custom datatype for the environment and position state, to
allow for extensibility later on.
\begin{spec}
-- Holds information about the current position in source.
data ScanState = ScanState
    { sourcePos :: SourcePos
    } deriving (Show, Eq)

-- The environment variables that the scanner can access.
data ScanEnv = ScanEnv
    { sourceText :: String
    } deriving (Show, Eq)
\end{spec}
One last thing to add is error handling, via the ExceptT monad
transformer.\footnote{Initially `ErrorT' was used, but due to
depreciation `ExceptT' was used instead - https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Except.html\#t:ExceptT}

Thus the final type is:
\begin{spec}
newtype Scanner a = Scanner
    { runScanner :: ExceptT ScanError (StateT ScanState (Reader ScanEnv)) a
    } deriving ( Functor, Applicative, Alternative, Monad, MonadPlus
           , MonadState ScanState, MonadReader ScanEnv, MonadError ScanError)
\end{spec}

`ScanError' is a type synonym for a string.
% TODO: Not actually using the 'unexpectedMsg' yet - need to implement
% this in the lexer
\begin{spec}
-- type ScanError = String
--
data ScanError = ScanError { expectedMsg :: String -- Human readable statement of an expected value
                           , unexpectedMsg :: String
                           , errMsg :: String    -- A general error message that does not fit
                                                -- into one of the above two categories
                           , errPos :: SourcePos  -- The position in source where the error occurred
                           }
\end{spec}

% Note, need to be able to detect end of string/file
% Maybe no strings, only handles?
Having determined the type signature for the Scanner, we now
need a way of progressing the position of the scanner and retrieving
a character.
\begin{spec}
scanChar :: Scanner Char
scanChar = do
  st <- get
  sourceString <- liftM sourceText ask
  let pos  = sourcePos st
      indx = sourceIndex pos
  if indx >= genericLength sourceString
  then unexpectedErr "end of stream"
  else do
    let chr = sourceString `genericIndex' indx
    put st{sourcePos=if chr == '\n'
                     then incNL pos  -- Error messages more useful
                                     -- if tracking the line number
                     else incCol pos}
    return chr
\end{spec}
% -- This will set the value of the Scanner to the next character
% -- whilst incrementing the position in the file.
% else genericIndex st . sourceIndex $ pos -- This is the character at pos
%   case readChar of
% '\NUL' -> put pos              -- I have chosen '\NUL' to represent the end of the string
% '\n'   -> put . posNewLine $ pos -- Encountered a new line,
% -- reset the column while incrementing line number
% _      -> put . incCol $ pos
%   return readChar

% This is not used in final project.
\end{document}

\part{Lexer}
\label{sec:lexer}

\part{Parser}
\label{sec:parser}





\end{document}
