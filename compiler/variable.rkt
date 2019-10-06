#lang racket
(define variable%
  (class object%
    (init-field name
                [isRead #f]
                [isReadOutOfContext #f])
    (define-values (isRead isReadOutOfContext) (values #f #f) )
    (super-new)
    (abstract getReadNode read)
    (define/public (isAccessed) isRead)
    (define/public (isAccessedOutOfContext) isReadOutOfContext)
    (define/public (getSuperReadNode contextLevel holderClass classSide?)
      (set! isRead #t)
      (when (> contextLevel 0)
        (set! isReadOutOfContext #t))
      (if (equal? contextLevel 0)
          (LocalSuperReadNode holderClass classSide?)
          (NonLocalSuperReadNode contextLevel holderClass classSide)))
    (define/public (getThisContextNode) (ThisContextNode 0))
    (define/public (isInternal) #f)))

(define argument%
  (class variable%
    (init-field name
                index)
    (super-new [name name])
    (abstract getReadNode read)
    (define/public (getReadNode contextLevel)
      ;(transferToInterpreterAndInvalidate)
      (set! isRead #t)
      (when (> contextLevel 0)
        (set! isReadOutOfContext #t))
      (if (equal? contextLevel 0)
          (LocalArgumentReadNode index)
          (NonLocalArgumentReadNode index contextLevel)))
    (define/public (read frame)
      (list-ref (send frame getArguments) index))))

(define local%
  (class variable%
    (init-field name
                slot)
    (define-values (isWritted isWrittenOutOfContext) (values #f #f) )
    (super-new [name name])
    (define/public (getSlot) slot)
    (define/public (getReadNode contextLevel)
      ;(transferToInterpreterAndInvalidate)
      (set! isRead #t)
      (when (> contextLevel 0)
        (set! isReadOutOfContext #t))
      (UninitializedVariableReadNode this contextLevel))
    (define/public (read frame)
      (list-ref (send frame getArguments) index))
    (define/public (getWriteNode contextLevel valueExpr)
      ;(transferToInterpreterAndInvalidate)
      (set! isWritten #t)
      (when (> contextLevel 0)
        (set! isWrittenOutOfContext #t))
      (UninitializedVariableWriteNode this contextLevel valueExpr))
    (define/public (read frame)
      (list-ref (send frame getValue) slot))
    (define/override (isAccessed) (or (super isAccessed) isWritten))
    (define/override (isAccessedOutOfContext) (or (superisAccessedOutOfContext) isWrittenOutOfContext))
    ))