#lang racket
(require "chspans.rkt")
(require "definitions.rkt")
(require "syntax-to-contract.rkt")
(require "textedit.rkt")


(define/contract (contract-infos-on-path path)
  (-> path-string? (listof func-contract-info?))
  (define path-str (if (string? path)
                       path
                       (path->string path)))
  (contract-infos&errors-i (path-addcontract path-str)))

(define/contract (path-addcontracts-withGUI path)
  (-> path-string? any)
  (define path-str (if (string? path)
                       path
                       (path->string path)))
  (process-file-with-func-contractinfos (path-addcontract path-str)))

(provide contract-infos-on-path path-addcontracts-withGUI
         (struct-out func-contract-info)
         (struct-out character-set)
         charset-size
         charset-empty?
         empty-charset
         union-charset
         subtract-charset)