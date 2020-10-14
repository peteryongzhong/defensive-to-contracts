#lang scribble/manual
@require[@for-label[defensive-to-contracts
                    racket/base]]

@title{defensive-to-contracts}
@author{Peter Zhong}

@defmodule[defensive-to-contracts]

@margin-note{This tool is highly experimental and has not been tested on a lot of codebases.}

This package presents a tool that converts defensive programming 
into equivalent contracts. Currently, the tool is still at its infancy 
and have not been tested on many code bases. The sample folder contains
some example files to get you started. So far I have exposed two functions
that allows you to input a path and either returns the raw result or load up
a GUI that allows you to change a file. 



@defproc[(contract-infos-on-path [path path-string?]) (listof func-contract-info?)]{                                            
Process the file located in  @racket[path] and returns the result as a list of 
@racket[func-contract-info] where each element represents a procedure in the file in
the order that the procedures appeared.}

@defproc[(path-addcontracts-withGUI [path path-string?]) void?]{

Process the file located in  @racket[path] and launch a GUI that allows for the contracts to be viewed and applied.
}

@defproc[(func-contract-info? [v any/c]) boolean?]{
 Returns  @racket[#t] if @racket[v] is a @racket[func-contract-info]. 
         }

@defproc[(func-contract-info [func-name any/c] [path string?] [spanset character-set?]
                             [contract any/c] [define-end integer?] [body-start integer?]
                             [desirability integer?]) func-contract-info?]{
This procedure initialises a struct @racket[func-contract-info]. 
@racketblock[(serializable-struct func-contract-info (func-name path spanset contract define-end body-start desirability)
  #:guard (struct-guard/c any/c path-string? character-set? any/c integer? integer? integer?))]
@racket[func-name] refers to the name of the function. @racket[path] path refers to the path of the file.
@racket[spanset] is a @racket[character-set] denoting which character to delete.
@racket[contract] contains an s expression of the contract generated.
@racket[define-end] and @racket[body-start] refers to the position of the end of the 
define in a procedure and the start of the body. These two location essentially points 
to where the contracts are added. @racket[desirability] is an internal measure of how 
desirable the contract generated is. 
}

@defproc[(character-set? [v any/c]) boolean?]{
 Returns @racket[#t] if @racket[v] is a @racket[character-set].
}

@defproc[(character-set [s set?]) character-set?]{
 Returns a @racket[character-set] from a @racket[set] of integers.
}

@defproc[(charset-size [s character-set?]) integer?]{}
@defproc[(charset-empty? [s character-set?]) boolean?]{}
@defproc[(empty-charset) character-set?]{}
@defproc[(union-charset [c1 character-set?] [c2 character-set?]) character-set?]{}
@defproc[(subtract-charset [c1 character-set?] [c2 character-set?]) character-set?]{ 
This function returns a character set with characters included in @racket[c1] but not in  @racket[c2]}


