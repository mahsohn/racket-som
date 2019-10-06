#lang racket
(require racket/lazy-require
         "variable.rkt"
         "../ast/nodes.rkt"
         "../vm/class.rkt"
         "../vm/symbol.rkt"
         "../vm/invokable.rkt"
         "../vm/constants/classes.rkt"
         "../interpreter/lexical-scope.rkt"
         "../interpreter/frame-descriptor.rkt"
         )
(lazy-require  ["constants/classes.rkt" (som:metaclass-class)])
(provide (all-defined-out))
(define (setInstanceInvokables classObj value) #f)
(define (setInstanceFields classObj value) #f)
(define (member? item lst) (sequence-ormap (lambda (i) (equal? item i)) lst))

(define (constructEmptyPrimitive signature)
  (let* ([mgen (new method-generation-context%)]
        [primNode (EmptyPrim #f)]
        [primitive (Primitive (send (send mgen getCurrentLexicalScope) getFrameDescriptor)
                              primNode
                              primNode ;(deepCopy primNode)
                              '())]
        [method (new som-method% [signature signature] [invokable primitive] [embeddedBlocks '()])]
        )
    ;(send primNode setMethod method) fixme
    method))
(define (createNonLocalReturn expr slot)
  (let ([node (new node%)])
    node))
(define class-generation-context%
  (class object%
    (init-field name
                universe
                [superName "Nil"]
                [classSide #f]
                [instanceFields '()]
                [instanceMethods '()]
                [classFields '()]
                [classMethods '()])
    (super-new)
    (define/public (get-name) name)
    (define/public (set-name str) (set! name str))
    (define/public (get-superName) superName)
    (define/public (set-superName str) (set! superName str))
    (define/public (get-universe) universe)
    (define/public (setClassSide bool) (set! classSide bool))
    (define/public (isClassSide?) classSide)
    (define/public (set-instanceFields fields)
      (for ([f fields]) (set! instanceFields (cons f instanceFields))))
    (define/public (set-classFields fields)
      (for ([f fields]) (set! classFields (cons f instanceFields))))
    (define/public (addInstanceField field)
      (set! instanceFields (cons field instanceFields)))
    (define/public (addInstanceMethod method)
      (set! instanceMethods (cons method instanceMethods)))
    (define/public (addClassField field)
      (set! instanceFields (cons field classFields)))
    (define/public (addClassMethod method)
      (set! classMethods (cons method classMethods)))
    (define/public (hasField field)
      (if (isClassSide?)
          (member field classFields)
          (member field instanceFields)))
    (define/public (getFieldIndex field)
      (if (isClassSide?)
          (index-of classFields field)
          (index-of instanceFields field)))
    (define/public (assemble)
      (define ccname (string-append (~a name " class")))
      (define sc (send universe loadClass superName))
      (define resultClass (new som-class%
                               [name (symbol-for ccname)]
                               [superClass som:metaclass-class]
                               [instanceFields classFields]
                               [instanceInvokables classMethods]))
      (define result (new som-class%
                               [name name]
                               [superClass resultClass]
                               [instanceFields instanceFields]
                               [instanceInvokables instanceMethods]))
      result)
    (define/public (assembleSystemClass systemClass)
      (for ([m instanceMethods]) (send m setHolder systemClass))
      (setInstanceInvokables systemClass instanceMethods)
      (setInstanceFields systemClass instanceFields)
      
      (define superMClass (send systemClass getSOMClass))
      (setInstanceInvokables superMClass classMethods)
      (setInstanceFields superMClass classFields)
      )
    )
  )
(define method-generation-context%
  (class object%
    (super-new)
    (init-field [classCtx #f]
                [outerCtx #f]
                [blockMethod #f]
                [signature ""]
                [primitive? #f]
                [needsToCatchNonLocalReturn?  #f]
                [throwsNonLocalReturn #f]
                [accessesVariablesOfOuterScope #f]
                )
    (define-values (arguments locals framOnStackSlot currentScope embeddedBlockMethods)
      (values
       (make-hash)
       (make-hash)
       '()
       '() '()))
    (define outer (send outerCtx getCurrentLexicalScope))
    (set! currentScope (new lexical-scope% [frameDescriptor (new frame-descriptor%)] [outerScope outer] ))
    
    (define/public (getHolder) "holder");fixme
    (define/public (isBlockMethod) blockMethod)
    (define/public (getClassCtx) classCtx)
    (define/public (getCurrentScope) currentScope)
    (define/public (getFrameOnStackMarkerSlot)
      (if (null? outerCtx)
          (send (send currentScope getFrameDescriptor) addFrameSlot "!frameOnStack")
          (send outerCtx getFrameOnStackMarkerSlot)))
    (define/public (makeCatchNonLocalReturn)
      (set! throwsNonLocalReturn #t)
      (let ([ctx (markOuterContextsToRequireContextAndGetRootContext)])
        (send ctx needsToCatchNonLocalReturn #t)))
    (define/public (requiresContext)
      (or throwsNonLocalReturn accessesVariablesOfOuterScope))
    (define (markOuterContextsToRequireContextAndGetRootContext)
      (define (loop ctx)
        (let ([outer (send ctx outerCtx)])
        (if (null? outer)
            ctx
            (begin
              (send ctx throwsNonLocalReturn #t)
              (loop outer)))))
      (loop outerCtx))
    (define/public (needsToCatchNonLocalReturn)
      (and needsToCatchNonLocalReturn? (null? outerCtx)))
    
    (define/public (assemble body)
      (if primitive?
          (constructEmptyPrimitive signature)
          (let ([onlyLocalAccess '()]
                [nonLocalAccess '()])
            (when (needsToCatchNonLocalReturn)
                (set! body (createNonLocalReturn body (getFrameOnStackMarkerSlot))))
            (define node (Method (send currentScope getFrameDescriptor)
                                   body
                                   body ;(deepCopy body)
                                   currentScope))
            (define method (new som-method% [signature signature] [invokable node] [embeddedBlocks embeddedBlockMethods]))
            (set-Invokable-belongsTo! node method)
            method
          )))
    (define/public (getVariable varName)
      (let ([l (hash-ref locals varName)])
        (displayln `(context:getVariable ,varName ,l))
        (when (not (equal? l #f)) l))
      (let ([a (hash-ref arguments varName #f)])
        (when (not (equal? #f)) a))
      (if (not (null? outerCtx))
        (let ([outerVar (send outerCtx getVariable varName)])
          (when (not (null? outerVar)) (set! accessesVariablesOfOuterScope #t))
          outerVar)
        '()))
    (define/public (getLocalReadNode var)
      (UninitializedVariableReadNode 0 var (getContextLevel var)))
    (define/public (getObjectFieldRead field)
      (if (send classCtx hasField field)
          (FieldReadNode 0 (getSelfRead) (send classCtx getFieldIndex))
          '()))
    (define (getSelfRead)
      (send (getVariable "self") getReadNode (getContextLevel "self")))
    (define/public (getNonLocalReturn expr)
      ;(makeCatchNonLocalReturn)
      (ReturnNonLocalNode expr (getFrameOnStackMarkerSlot) (getOuterSelfContextLevel))
      )
    #;(define/public (getFrameOnStackMarkerSlot)
      (if (not (null? outerGenc))
          (send outerGenc getFrameOnsStackMarkerSlot)
          (when (not (null? fromOnStackSlot))
              (begin (set! frameOnStackSlot
                    (send (send currentScope getFrameDescriptor) addFrameSlot (send universe frameOnStackSlotName)))
                     frameOnStackSlot)))
      frameOnStackSlot)
    (define/public (markAsPrimitive) (set! primitive? #t))
    (define/public (setSignature sig) (set! signature sig))
    (define/public (getArgument arg) (hash-ref arguments arg))
    (define/public (addArgument arg)
      ;(displayln `(addargument ,arg ,(length arguments)))
      (if (or (or (equal? (~a arg) "self")  (equal? (~a arg) "$blockSelf")) (> (length arguments) 0))
          (error "The self argument has to be the first argument of a method")
          (let ([argument (new argument% [name arg] [index (length arguments)])])
            (hash-set! arguments arg argument))))
    (define/public (addArgumentIfAbsent arg)
      ;(displayln `(addargumentIfAbsent ,arg ,(length arguments)))
      (when (not (hash-ref arguments arg #f))
        (addArgument arg)))
    (define/public (getNumberOfArguments) (hash-count arguments))
    (define/public (getLocal arg) arg)
    (define/public (addLocal string)
      (displayln `(addLocal ,string ,(hash-count locals)))
      (let ([l (new local%
                    [name string]
                    [slot (send (send currentScope getFrameDescriptor) addFrameSlot string)])])
        (hash-set! locals string l)
        ))
    (define/public (addLocalIfAbsent arg)
      ;(displayln `(addLocalIfAbsent ,arg ,(length arguments)))
      (if (not (hash-ref locals arg #f))
          (addLocal arg)
          #f))
    (define/public (getOuterSelfContextLevel)
      (define (loop ctx lv)
        (let ([outer (send ctx outerCtx)])
        (if (null? outer)
            0
            (loop outer (+ lv 1)))))
      (loop outerCtx 0))
    (define/public (getContextLevel varName)
      (if (or (hash-ref locals varName #f) (hash-ref arguments varName #f))
        0
        (if (not (null? outerCtx))
            (+ 1 (send outerCtx getContextLevel varName))
            0)))
     (define/public (getEmbeddedLocal name)
       (hash-ref locals name))
    (define/public (addEmbeddedBlockMethod method)
       (set! embeddedBlockMethods (cons method embeddedBlockMethods)))
 

    )
  )
            
            