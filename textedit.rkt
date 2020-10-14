#lang racket
(require framework)
(require racket/gui)
(require "chspans.rkt")
(require "definitions.rkt")
(require "syntax-to-contract.rkt")
(require racket/format)
(require racket/pretty)

;(define editor-stream 
;  (make-object editor-stream-in%
;    (make-object editor-stream-in-bytes-base%
;      (file->bytes "/Users/peterzhong/Desktop/OneDrive/Northwestern/Research/defensive-anlysis-removal/sample-defensive/simple-defensive-two-arg.rkt"))))

;l(send text read-from-file editor-stream 'start)
;racket:text

(struct field-checkboxpair (f c))

(define racket:text-line%
  (text:line-numbers-mixin racket:text%))

(define (update-frame-with-newinfo contract-info text contract-textfield) 
  (match contract-info
    [(func-contract-info
      func-name
      path
      spanset
      contract 
      define-end
      body-start
      desire) 
     (for ([char (character-set-s spanset)])
       (send text highlight-range (- char 1) char (make-color 255 0 0 ) #:key `contract-removal))
     (send contract-textfield set-value (substring (~v contract) 1))]))


(define (process-file-with-func-contractinfos contract-info-error)
  ;(editor:set-current-preferred-font-size 12)
  (define contract-infos (contract-infos&errors-i contract-info-error))
  (define path (func-contract-info-path (first contract-infos)))
  (define inner-frame (new frame% [label path] [width 1400] [height 1000]))
  (define frame (new vertical-panel% [parent inner-frame] [vert-margin 0] [horiz-margin 0]))
  (define pane (new horizontal-panel%  [parent frame]  [vert-margin 0] [horiz-margin 0]))
  (define text  (new racket:text-line%))
  (define editor (new editor-canvas% [parent pane] [editor text] [vert-margin 0] [horiz-margin 0]))
  (define text1 (new racket:text-line%))
  (define editor1 (new editor-canvas% [parent pane] [editor text1] [vert-margin 0] [horiz-margin 0]))
  (send text load-file
        path
        'text)
  (send text1 load-file
        path
        'text)
  (send text show-line-numbers! #t)
  (send text1 show-line-numbers! #t)
  (define (apply)
    (send text1 begin-edit-sequence #f)
    (send text1 load-file
          path
          'text)
    (define op1 (open-output-string))
    (for ([contract-info contract-infos]
          [contract-fieldcheckbox contract-fieldcheckboxes])
      (if (send (field-checkboxpair-c contract-fieldcheckbox) get-value )
          (begin
            ;(send text1 unhighlight-ranges/key `contract-removal)
            (for ([char (sort (set->list
                               (character-set-s
                                (func-contract-info-spanset
                                 contract-info)))
                              >)])
              (send text1 delete (- char 1) char ))
            (pretty-write (read (open-input-string (send (field-checkboxpair-f contract-fieldcheckbox) get-value))) op1)
            (send text1 insert
                  (string-append (get-output-string op1) "\n     ")
                  (- (func-contract-info-body-start
                      contract-info) 1))
            (send text1 insert "/contract" (- (func-contract-info-define-end
                                              contract-info) 1)))
          (void)))
    (send text1 end-edit-sequence)
    (send text1 tabify-all))
  
  (define contract-fieldcheckboxes
    (reverse (for/list ([contract-info (reverse contract-infos)])
               (match contract-info
                 [(func-contract-info
                   func-name
                   path
                   spanset
                   contract 
                   define-end
                   body-start
                   desire)
                  (for ([char (character-set-s spanset)])
                    (send text highlight-range (- char 1) char (make-color 255 0 0 ) #:key `contract-removal))
                  ;(define sub-pane (new horizontal-panel% [parent frame] [vert-margin 0] [horiz-margin 0]))
                  (field-checkboxpair
                   (new text-field%
                        [label (~v func-name)]
                        [parent frame]
                        [vert-margin 0]
                        [horiz-margin 0]
                        [font (make-object font% 15 'default)]
                        [init-value (substring (~v contract) 1)])
                   (new check-box%
                        [vert-margin 0]
                        [horiz-margin 0]
                        [label "enabled?"]
                        [parent frame]
                        [callback
                         (lambda (c e)
                           (apply))]))]))))
  
;    (define apply-btn (new button%
;                         [parent frame]
;                         [label "Apply"]
;                         ; Callback procedure for a button click:
;                         [callback
;                          (lambda (button event)
;                            (apply))]))

 (define save-btn (new button%
                        [parent frame]
                        [label "Save Changes"]
                        [enabled #t]
                        [callback (lambda (button event)
                                    (send text1 save-file
                                          path)
                                    )]))

  
  (send inner-frame show #t)
  )
;
;
;  (define func-counter 0)
;  
;  (define contract-textfield
;    (new text-field%
;       [label "contract"]
;       [parent frame]
;       [init-value ""]))
;  (update-frame-with-newinfo (list-ref contract-infos func-counter) text contract-textfield)
;  
;  
;  (send frame show #t))

;(define (highlight-from-charset spanset path contract-statement define-end body-start)
;  (define frame (new frame% [label path] [width 1000] [height 600]))
;  (define text  (new racket:text%))
;  (define editor (new editor-canvas% [parent frame] [editor text]))
;  (send text load-file
;        path
;        'text)
;  (define contract-textfield
;    (new text-field%
;       [label "contract"]
;       [parent frame]
;       [init-value (substring (~v contract-statement) 1)]))
;  (define save-btn (new button%
;                        [parent frame]
;                        [label "Save Changes"]
;                        [enabled #f]
;                        [callback (lambda (button event)
;                                    (send text save-file
;                                          path))]))
;  (define apply-btn (new button%
;                         [parent frame]
;                         [label "Apply"]
;                         ; Callback procedure for a button click:
;                         [callback (lambda (button event)
;                                     ; (send text highlight-range 0 1 (make-color 0 255 0))
;                                     (send text unhighlight-ranges/key `contract-removal)
;                                     (for ([char (sort (set->list (character-set-s spanset)) >)])
;                                       (send text delete (- char 1) char ))
;                                     (send text insert (string-append (send contract-textfield get-value) "\n     ") (- body-start 1))
;                                     (send text insert "/contract" (- define-end 1))
;                                     (send button enable #f)
;                                     (send save-btn enable #t)
;                                     )]))
;
;  (for ([char (character-set-s spanset)])
;    (send text highlight-range (- char 1) char (make-color 255 0 0 ) #:key `contract-removal))
;  (send frame show #t))

;(process-file-with-func-contractinfos (path-addcontract "/Users/peterzhong/Library/Racket/7.8/pkgs/aws/aws/s3.rkt"))
;(process-file-with-func-contractinfos (path-addcontract "/Users/peterzhong/Desktop/OneDrive/Northwestern/Research/defensive-anlysis-removal/sample-defensive/racketcon.rkt"))
(provide process-file-with-func-contractinfos)