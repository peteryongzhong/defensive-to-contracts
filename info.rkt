#lang info
(define collection "defensive-to-contracts")
(define deps '("base" "plai" "gui-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/defensive-to-contracts.scrbl" ())))
(define pkg-desc "This package presents a tool that converts defensive programming 
into equivalent contracts. Currently, the tool is still at its infancy 
and have not been tested on many code bases. The sample folder contains
some example files to get you started. So far I have exposed two functions
that allows you to input a path and either returns the raw result or load up
a GUI that allows you to change a file.")
(define version "0.0.1")
(define pkg-authors '(peterzhong))
