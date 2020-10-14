#lang racket
(require "definitions.rkt")
(require "textedit.rkt")
(require racket/serialize)

(define file-path (vector-ref (current-command-line-arguments) 0))
(define in (open-input-file file-path))
(process-file-with-func-contractinfos (deserialize (read in)))
