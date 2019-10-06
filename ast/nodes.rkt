#lang racket
(require racket/generic
      ;   "../compiler/context.rkt"
      ;   "frame.rkt"
         )
(provide (all-defined-out))

(define globals '( (true . true) (false . false)))
(define (getGlobal name) (assoc name globals))
(struct frame (arguments [locals #:mutable] tags))
(define (frame-get frame slot val)
  (cdr (assoc slot (frame-locals frame))))
(define (frame-set frame slot val)
  (set-frame-locals! frame (list-set (frame-locals frame) slot `(slot . val))))

(struct somclass (name instance-fields instance-methods class-fields class-methods))
(define NilObject '())
(define-values (ROOT_EXPR LOOP_BODY CONTROL_FLOW_CONDITION PRIMITIVE_ARGUMENT VIRTUAL_INVOKE_RECEIVER)
  (values 1 2 3 4 5)
  )
(define-generics executable
  (execute executable frame))
(define-generics evaluatedExecutable
  (executeEvaluated evaluatedExecutable frame obj))
(define-generics tagged
  (markAsRootExpression tagged)
  (markAsLoopBody tagged)
  (markAsControlFlowCondition tagged)
  (markAsPrimitiveArgument tagged)
  (markAsVirtualInvokeReceiver tagged)
  (hasTag tagged tag)
  )
(define-generics preevaluatedExpression
  (doPreEvaluated preevaluatedExpression frame args))
(define-generics expressionWithReveiver
  (getReceiver expressionWithReveiver)
  (executeWithReceiver expressionWithReveiver frame reciever))
(define-generics inlineable
  (inline inlineable mgctx))

(struct Node ())
(struct ExecutableNode Node ())
(struct RootNode ExecutableNode (callTarget frameDescriptor lock))
(struct Invokable RootNode (expression body belongsTo))
(struct Primitive Invokable ())
(struct Method Invokable (scope))

(struct SOMNode Node ())
(struct ExpressionNode SOMNode ()
  #:methods gen:executable
  [(define (execute executable frame) (error "not implemented"))]
  )
(struct ExpressionWithTagsNode ExpressionNode ([tag #:mutable])
  #:methods gen:tagged
  [(define (markAsRootExpression self) (set-ExpressionWithTagsNode-tag! self ROOT_EXPR))
   (define (markAsLoopBody self) (set-ExpressionWithTagsNode-tag! self LOOP_BODY))
   (define (markAsControlFlowCondition self) (set-ExpressionWithTagsNode-tag! self CONTROL_FLOW_CONDITION))
   (define (markAsPrimitiveArgument self) (set-ExpressionWithTagsNode-tag! self PRIMITIVE_ARGUMENT))
   (define (markAsVirtualInvokeReceiver self) (set-ExpressionWithTagsNode-tag! self VIRTUAL_INVOKE_RECEIVER))
   (define (hasTag self tag) (equal? tag (ExpressionWithTagsNode-tag self)))
   ]
  )



(struct FieldNode ExpressionWithTagsNode ())
(struct FieldReadNode FieldNode (self fieldIndex)
  #:methods gen:evaluatedExecutable
  [(define (executeEvaluated node frame obj) (void))])
(struct FieldWriteNode FieldNode (self value fieldIndex)
  #:methods gen:evaluatedExecutable
  [(define (executeEvaluated node frame obj) (void))])

(struct ContextualNode ExpressionWithTagsNode (contextLevel))
(struct ThisContextNode ExpressionWithTagsNode ()
  #:methods gen:executable
  [(define (execute node frame) (void))])
(struct LocalArgumentReadNode ExpressionWithTagsNode (argumentIndex)
  #:methods gen:executable
  [(define (execute node frame) (list-ref (frame-arguments frame) (LocalArgumentReadNode-argumentIndex node)))])
(struct NonLocalArgumentReadNode ContextualNode (argumentIndex contextLevel)
  #:methods gen:executable
  [(define (execute node frame) (void))])
(struct LocalSuperReadNode LocalArgumentReadNode (argumentIndex)
  #:methods gen:executable
  [(define (execute node frame) (void))])
(struct NonLocalSuperReadNode NonLocalArgumentReadNode (holderClass ClassSide)
  #:methods gen:executable
  [(define (execute node frame) (void))])
 
(struct SequenceNode ExpressionWithTagsNode (expressions)
  #:methods gen:executable;todo - execute all but last then return last execution
  [(define (execute node frame) (for ([expr (SequenceNode-expressions node)]) (execute expr frame)))]
  )
(struct CascadeMessageSendNode ExpressionWithTagsNode (receiver messages)
  #:methods gen:executable;todo - execute all but last then return last execution
  [(define (execute node frame)
     (let ([rcvr (execute (CascadeMessageSendNode-receiver node) frame)])
     (for ([expr (CascadeMessageSendNode-messages node)])
       (executeWithReceiver expr frame rcvr))))]
  )
(struct AbstractMessageSendNode ExpressionWithTagsNode (selector argumentNodes)
  #:methods gen:preevaluatedExpression 
  [(define (doPreEvaluated node frame args) (error "unimplemented"))]
  #:methods gen:expressionWithReveiver
  [(define (getReceiver node) (car (AbstractMessageSendNode-argumentNodes node)))
   (define (executeWithReceiver node frame reciever) (error "implement Me!"))]
  )
(struct AbstractUninitializedMessageSendNode AbstractMessageSendNode ()
#:methods gen:preevaluatedExpression 
  [(define (doPreEvaluated node frame args) (error "unimplemented"))]
  )
(struct UninitializedMessageSendNode AbstractUninitializedMessageSendNode ());implementme



(struct GlobalNode ExpressionWithTagsNode (globalName)
  
  )
(struct AbstractUninitializedGlobalReadNode (universe)
#:methods gen:executable
  [(define (execute self frame)
     (let ([name (GlobalNode-globalName self)])
       (cond [(equal? name "true") (TrueGlobalNode)]
             [(equal? name "false") (FalseGlobalNode)]
             [(equal? name "nil") (NilGlobalNode)]
             [else (let ([g (getGlobal name)])
                      (if (null? g)
                          (CachedGlobalReadNode name g)
                          NilObject
                          ))]
             )
     ))]
  )
(struct UninitializedGlobalReadNode AbstractUninitializedGlobalReadNode ())
(struct CachedGlobalReadNode GlobalNode (global)
  #:methods gen:executable
  [(define (execute self frame) (CachedGlobalReadNode-global self))])
(struct TrueGlobalNode GlobalNode () #:methods gen:executable [(define (execute self frame) #t)])
(struct FalseGlobalNode GlobalNode () #:methods gen:executable [(define (execute self frame) #f)])
(struct NilGlobalNode GlobalNode () #:methods gen:executable [(define (execute self frame) NilObject)])

(struct ReturnLocalNode ExpressionWithTagsNode (frameOnStackMarker)
  #:methods gen:executable
  [(define (execute self frame) (error "returnlocalnode"))])
(struct CatchNonLocalReturnNode ExpressionWithTagsNode (methodBody nonLocalReturnHandler doCatch doPropagate frameOnStackMarker)
  #:methods gen:executable
  [(define (execute self frame)
     (execute (CatchNonLocalReturnNode-methodBody self) frame)
     ;if execution fails (enter (CatchNonLocalReturnNode-nonLocalReturnHandler)) (enter doCatch)
     )]
  )
(struct IfInlinedLiteralNode ExpressionWithTagsNode (conditionNode bodyNode expectedBool inlinedBodyNode originalBodyNode)
  #:methods gen:executable
  [(define (execute self frame)
     (if (equal? (execute (IfInlinedLiteralNode-conditionNode self) frame) (IfInlinedLiteralNode-expectedBool self))
         (execute (IfInlinedLiteralNode-bodyNode self) frame)
         NilObject)
   )]
  )
(struct IfTrueIfFalseInlinedLiteralsNode ExpressionWithTagsNode (conditionNode trueNode falseNode)
  #:methods gen:executable
  [(define (execute self frame)
     (if (execute (IfTrueIfFalseInlinedLiteralsNode-conditionNode self) frame)
         (execute (IfTrueIfFalseInlinedLiteralsNode-trueNode self) frame)
         (execute (IfTrueIfFalseInlinedLiteralsNode-falseNode self) frame))
   )]
  )
(struct WhileInlinedLiteralNode ExpressionWithTagsNode (conditionNode bodyNode inlinedConditionNode inlinedBodyNode expectedBool originalConditionNode originalBodyNode)
  #:methods gen:executable
  [(define (execute self frame)
     (let* ([expectedBool (WhileInlinedLiteralNode-expectedBool self)]
            [loopConditionResult (execute (WhileInlinedLiteralNode-conditionNode self) frame)])
       (if (equal? loopConditionResult expectedBool)
           (do ([result loopConditionResult])
             ((equal? result expectedBool))
             (begin
               (execute (IfInlinedLiteralNode-bodyNode self) frame)
               (set! result (execute (WhileInlinedLiteralNode-conditionNode self) frame))))
           NilObject))
   )]
  )
(struct IntToDoLiteralsInlinedNode ExpressionWithTagsNode (from to body loopIndex originalBody)
  #:methods gen:executable
  [(define (execute self frame)
     (let ([from (IntToDoLiteralsInlinedNode-from self)]
           [to (IntToDoLiteralsInlinedNode-to self)])
       (when (<= from to)
         (begin
           (frame-set (IntToDoLiteralsInlinedNode-loopIndex self) from)
           (execute (IntToDoLiteralsInlinedNode-body self))))
       (for ([i (range (+ from 1) to)])
         (frame-set (IntToDoLiteralsInlinedNode-loopIndex self) i)
         (execute (IntToDoLiteralsInlinedNode-body self)))))]
  )
(struct BooleanInlinedLiteralNode ExpressionWithTagsNode (receiverNode argumentNode)
 
  )
(struct OrInlinedLiteralNode BooleanInlinedLiteralNode (receiverNode argumentNode inlinedArgumentNode originalArgumentNode)
  #:methods gen:executable
  [(define (execute self frame)
     (if (execute (OrInlinedLiteralNode-receiverNode self) frame)
         #t
         (execute (OrInlinedLiteralNode-argumentNode self) frame))
   )]
  )
(struct AndInlinedLiteralNode BooleanInlinedLiteralNode (receiverNode inlinedArgumentNode originalArgumentNode)
  #:methods gen:executable
  [(define (execute self frame)
     (if (execute (AndInlinedLiteralNode-receiverNode self) frame)
         (execute (OrInlinedLiteralNode-argumentNode self) frame)
         #f)
   )]
  )
(struct LiteralNode ExpressionWithTagsNode ()
  #:methods gen:preevaluatedExpression 
  [(define (doPreEvaluated self frame args) (execute self frame))]
  #:methods gen:inlineable
  [(define (inline self node) self)]
  )
(struct IntegerLiteralNode LiteralNode (value)
  #:methods gen:executable [(define (execute self frame) (IntegerLiteralNode-value self))]
  )
(struct StringLiteralNode LiteralNode (value)
  #:methods gen:executable [(define (execute self frame) (StringLiteralNode-value self))]
  )
(struct CharLiteralNode LiteralNode (value)
  #:methods gen:executable [(define (execute self frame) (CharLiteralNode-value self))]
  )
(struct DoubleLiteralNode LiteralNode (value)
  #:methods gen:executable [(define (execute self frame) (DoubleLiteralNode-value self))]
  )
(struct BigIntegerLiteralNode LiteralNode (value)
  #:methods gen:executable [(define (execute self frame) (BigIntegerLiteralNode-value self))]
  )
(struct SymbolLiteralNode LiteralNode (value)
  #:methods gen:executable [(define (execute self frame) (SymbolLiteralNode-value self))]
  )
(struct ArrayLiteralNode LiteralNode (value)
  #:methods gen:executable [(define (execute self frame) (ArrayLiteralNode-value self))]
  )
(struct BlockNode LiteralNode (blockMethod blockClass)
  ;block class is stored in universe.objectMemory ... I think
  #:methods gen:executable [(define (execute self frame) (error "implement me"))]
  )
(struct BlockNodeWithContext BlockNode ()
;block class is stored in universe.objectMemory ... I think
  #:methods gen:executable [(define (execute self frame) (error "implement me"))]
  )

(struct LocalVariableNode ExpressionWithTagsNode (slot)
;#:methods gen:tagged [(define (hasTag self tag) (error "implement me"))]
  )
(struct LocalVariableReadNode LocalVariableNode ())
(struct LocalVariableWriteNode LocalVariableNode ()
 ; #:methods gen:write [(define (write self frame value) (setObject frame (LocalVariableNode-slot self) value))]
  )


(struct UninitializedVariableNode ContextualNode (variable)
  #:methods gen:executable [(define (execute self frame) (error "implement me"))]
  )
(struct ReturnNonLocalNode ContextualNode (expression frame)
  #:methods gen:executable [(define (execute self frame) (error "implement me"))]
  )
(struct UninitializedVariableReadNode UninitializedVariableNode ())
(struct UninitializedVariableWriteNode UninitializedVariableNode (expression))