\documentclass{article}

%include lhs2TeX.fmt
%include lhs2TeX.sty
%include polycode.fmt

\begin{document}

\title{An implementation of a basic general-purpose programming language}
\author{Ben Moon}
\maketitle
\date{}

\paragraph{How to read this document}
\label{par:how_to_read_this_document}

This document should be read in conjunction with the source files
provided. Examples of code presented in this document may not
reflect the final source, and should be assumed to be non-functional.
\\
Relevant modules will be stated before any sections describing the
development of code.




\part{Scanner}
\label{sec:scanner}

\paragraph{Relevant modules}
\label{par:relevant_modules}

`Angle.Scanner`

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
State as a newtype wrapper) https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-State-Lazy.html#t:State} of:
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
depreciation `ExceptT` was used instead - https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Except.html#t:ExceptT}

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
    let chr = sourceString `genericIndex` indx
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
