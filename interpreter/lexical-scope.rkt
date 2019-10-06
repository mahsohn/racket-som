#lang racket
;(require )

(provide (all-defined-out))
; all arguments, locals and internal variables used in a method
(define lexical-scope%
  (class object%
    (init-field frameDescriptor outerScope)
    (field [method #f variables '()])
    (super-new)
    (abstract getReadNode read)
    (define/public (getFrameDescriptor) frameDescriptor)
    (define/public (getOuterScope) outerScope)
    (define/public (propagateLoopCountThroughoutLexicalScope)
      (when (not (null? outerScope))
        (send (send outerScope method) propagateLoopCountThroughoutLexicalScope)
      ))
    (define/public (getMethod) method)
    (define/public (setMethod m) set! method m)
    (define/public (getVariables) variables)
    (define/public (setVariables vars) (set! variables vars))
    ))