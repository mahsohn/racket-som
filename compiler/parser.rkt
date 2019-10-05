#lang racket
(require
  parser-tools/lex
  errortrace
  "lexer.rkt"
  "context.rkt"
  "../ast/nodes.rkt"
         )
(define LONG_MAX 9223372036854775807)
(define LONG_MIN -9223372036854775808)
(define singleOps '(Not And Or Star Div Mod Plus Equal More Less Comma Minus At Per NONE))
(define binaryOps '(Or Comma Minus Equal Not And Or Star Div Mod Plus Equal More Less Comma At Per NONE))
(define keywordSelectorOps '(Keyword KeywordSequence))
(define (member? item lst) (sequence-ormap (lambda (i) (equal? item i)) lst))

(define-struct (parse-exception exn:fail:user) ())

(define curr-pos (position-token 'NONE (position 0 0 0) (position 0 0 0)))
(define peek-pos (position-token 'NONE (position 0 0 0) (position 0 0 0)))
(define sym #f)
(define text #f)
(define port (open-input-file "../SOM/Smalltalk/SortedCollection.som"))

(define nxt (lex port))
(define (next)
  (let ([val (nxt)])
    (set! curr-pos val)
    (set! sym (token-name (position-token-token val)))
    (set! text (token-value (position-token-token val)))
    val))
(define (peek)
  (define loc (file-position port))
  (set! peek-pos (nxt))
  (file-position port loc)
  (token-name (position-token-token peek-pos)))


(define (accept token)
  ;(displayln `(accept ,token ,(equal? token sym)))
  (if (equal? token sym)
      (begin (next) #t)
      #f))
(define (acceptOneOf token lst)
  (if (member? token lst)
      (begin (next) #t)
      #f))
      
(define (parser-error [msg "parse error"] [func "unknown"])
  (let ([loc (file-position port)]
          [offset (position-offset (position-token-start-pos curr-pos))]
          [col (position-col (position-token-start-pos curr-pos))]
          [line (position-line (position-token-start-pos curr-pos))])
      (displayln (string-join `("error from" ,func  ,(~a sym) ,(~a text) "on line" ,(~a line) )))
      (file-position port (- offset col))
      (displayln  (read-line port))
      (displayln (string-append (make-string col #\-) "^"))
      (file-position port loc)
       (error msg)
    ;(raise (make-parse-exception msg (current-continuation-marks)) )
    ))
(define (expect token [function "unknown"] [errormsg "parse error"])
  (when (not (accept token))
    (let ([loc (file-position port)]
          [offset (position-offset (position-token-start-pos curr-pos))]
          [col (position-col (position-token-start-pos curr-pos))]
          [line (position-line (position-token-start-pos curr-pos))])
      (displayln (string-join `("error from" ,function "expected" ,(~a token) "got" ,(~a sym) "on line" ,(~a line) ) ))
      (file-position port (- offset col))
      (displayln  (read-line port))
      (displayln (string-append (make-string col #\-) "^"))
      (file-position port loc)
      (error errormsg)
    ;(raise (make-parse-exception msg (current-continuation-marks)) )
    )
    ;(error (string-join `("error from" ,function "expected" ,(~a token) "got" ,(~a sym) "on line" ,(position-line (position-token-start-pos curr-pos)) )) function)
    ;(raise (make-parse-exception (string-join `("error from" ,function "expected" ,(~a token) "got" ,(~a sym) "on line" ,(~a (position-token-start-pos curr-pos)) )) (current-continuation-marks)))
    ))
(define (expectOneOf token lst)
  (when (not (acceptOneOf token lst))
    (parser-error (string-join (string-append "expectOneOf " (~a token)) "expectOneOf"))))
    
(define (classDef cgctx)
  (expect 'Identifier "classDef")
  (send cgctx set-name text)
  (expect 'Equal "classDef")
  (superclass cgctx)
  (expect 'NewTerm "classDef")
  (instanceFields cgctx)
  ;(displayln `("sym" ,sym ,text))
  (do () ((not (or (identifier? sym) (equal? sym 'Keyword) (equal? sym 'OperatorSequence) (member? sym binaryOps))) )
    (let* ([mgctx (new method-generation-context% [classCtx cgctx])]
             [methodBody (method mgctx)]
             [method (send mgctx assemble methodBody)])
      
        (send cgctx addInstanceMethod method)
      ))
  (when (accept 'Separator)
    (send cgctx setClassSide #t)
    (classFields cgctx)
    (do () ((not (or (identifier? sym) (equal? sym 'Keyword) (equal? sym 'OperatorSequence) (member? sym binaryOps))) )
    (let* ([mgctx (new method-generation-context% [classCtx cgctx])]
             [methodBody (method mgctx)]
             [method (send mgctx assemble methodBody)])
      
        (send cgctx addClassMethod method)
      ))
    )
  (expect 'EndTerm "classDef"))

(define (superclass cgctx)
  (let ([supername (if (equal? sym 'Identifier)
                       (begin (accept 'Identifier) text)
                       "Object")])
    (send cgctx set-superName supername)
    #;(when (not (equal? supername "nil"));;fixme
      (let ([superClass (send universe loadClass supername)])
        (when (null? superClass)
          (error (string-join `("super class" ,superclass "could not be found"))))
        (send cgctx set-InstanceFields (send superclass get-InstanceFields))
        (send cgctx set-ClassFields (send superclass get-ClassFields));!!
      ))))
(define (instanceFields cgctx)
  (when (accept 'Or)
    (begin
      (do () ((not (identifier? sym)))
        (send cgctx addInstanceField (variable)))
      (expect 'Or "instanceFields"))))
(define (classFields cgctx)
  (when (accept 'Or)
    (begin
      (do () ((not (identifier? sym)))
        (send cgctx addClassField (variable)))
      (expect 'Or "classFields"))))
(define (method mgctx)
  ;(displayln `(method: ,text))
  (pattern mgctx)
  
  (expect 'Equal "method" (string-join `("method:" ,(~a text))))
  (if (equal? sym 'Primitive)
      (begin
        (send mgctx markAsPrimitive)
        (primitiveBlock)
        '())
      (methodBlock mgctx)))
(define (primitiveBlock) (expect 'Primitive "primitiveBlock"))
(define (pattern mgctx)
  ;(send mgctx addArgumentIfAbsent "self")
  ;(displayln `(pattern: ,sym ,text))
  (send mgctx addArgument "self")
  (match sym
    ['Identifier (unaryPattern mgctx)]
    ['Primitive (unaryPattern mgctx)]
    ['Keyword (keywordPattern mgctx)]
    [_ (binaryPattern mgctx)]))
(define (unaryPattern mgctx)
  ;(displayln `(unaryPattern: ,sym ,text))
  (send mgctx setSignature (unarySelector)))
(define (binaryPattern mgctx)
  ;(displayln `(binaryPattern: ,sym ,text))
  (send mgctx setSignature (binarySelector))
  ;(send mgctx addArgumentIfAbsent (argument))
  (send mgctx addArgument (argument))
  )
(define (keywordPattern mgctx)
  ;(displayln `(keywordPattern: ,sym ,text))
  (define str "")
  (do ()
    ((not (equal? sym 'Keyword)))
    (begin
      (set! str (string-append str (symbol->string (keyword))))
      ;(send mgctx addArgumentIfAbsent (argument))
      (send mgctx addArgument (argument))
      ))
  (send mgctx setSignature str))
(define (methodBlock mgctx)
  (expect 'NewTerm "methodBlock")
  (define methodBody (blockContents mgctx))
  (expect 'EndTerm "methodBlock")
  methodBody)
(define (unarySelector) (identifier))
(define (binarySelector)
  ;(displayln `(binarySelector ,sym ,text ,(member? sym singleOps)))
  (when (and
         (not (accept 'Or))
         (not (accept 'Comma))
         (not (accept 'Minus))
         (not (accept 'Equal))
         (not (acceptOneOf sym singleOps))
         (not (accept 'OperatorSequence)))
    (expect 'NONE "binarySelector"))
  text)
(define (identifier)
  (when (not (accept 'Primitive))
    (expect 'Identifier "identifier"))
  text)
(define (keyword)
  (expect 'Keyword "keyword")
  text)
(define (argument) (variable))
(define (blockContents mgctx)
  (when (accept 'Or)
    (locals mgctx)
    (expect 'Or "blockContents"))
  (blockBody mgctx))
(define (locals mgctx)
  (do () ((not (identifier? sym)))
    (send mgctx addLocalIfAbsent (variable))))
(define (blockBody mgctx)
  (define expressions '())
  (define (loop)
    (if (accept 'Exit)
        (begin
          (set! expressions (cons (result mgctx) expressions))
          (createSequenceNode expressions))
        (if (equal? sym 'EndBlock)
            (createSequenceNode expressions)
            (if (equal? sym 'EndTerm)
              (begin
                (set! expressions (cons (variableRead mgctx "self") expressions))
                (createSequenceNode expressions))
              (begin
                (set! expressions (cons (expression mgctx) expressions))
                (accept 'Period)
                (loop))))))
  (loop))
(define (createSequenceNode expressions)
  (if (empty? expressions)
      ;(createGlobalRead "nil")
      (UninitializedGlobalReadNode "nil")
      (if (equal? (length expressions) 1)
        (car expressions)
        (SequenceNode 0 expressions)
        ;(createSequence expressions)
        )))
(define (result mgctx)
  (define exp (expression mgctx))
  (accept 'Period)
  (if (send mgctx isBlockMethod)
      (send mgctx getNonLocalReturn exp)
      exp))
(define (expression mgctx)
  ;(displayln `(before: ,sym ,text))
  (define p (peek))
  ;(displayln `(expression peek: ,p sym: ,sym text: ,text))
  (if (equal? p 'Assign)
      (assignation mgctx)
      (evaluation mgctx)))
(define (assignation mgctx) (assignments mgctx))
(define (assignments mgctx)
  ;(displayln `(assignments ,sym ,text))
  (when (not (identifier? sym))
    (parser-error (string-append "Assignments should always target variables or fields, but found a " (symbol->string sym)) "assignments"))
  (let* ([variable (assignment)]
         [p (peek)]
         [value (if (equal? p 'Assign) (assignments mgctx) (evaluation mgctx))])
    (variableWrite mgctx variable value)))
(define (assignment)
  (define v (variable))
  (expect 'Assign "assignment")
  v)
(define (cascadeMessages mgctx firstMessage receiver)
  ;(displayln `(cascadeMessages ,sym ,text))
  (define expressions (list firstMessage))
  (do () ((not (accept 'SemiColon)))
    (set expressions (cons (messages mgctx receiver) expressions)))
  ;(createCascadeMessageSend receiver expressions)
  (CascadeMessageSendNode 0 receiver expressions)
  )
(define (evaluation mgctx)
  ;(displayln `(evaluation ,sym ,text))
  (define exp (primary mgctx))
  (when (or (identifier? sym) (equal? sym 'Keyword) (equal? sym 'OperatorSequence) (member? sym binaryOps))
    (let ([receiver exp])
      (set! exp (messages mgctx exp))
      (when (equal? sym 'SemiColon)
        (set! exp (cascadeMessages mgctx exp receiver)))))
  exp)
(define (primary mgctx)
  ;(displayln `(primary ,sym ,text))
  (match sym
    ['Identifier (variableRead mgctx (variable))]
    ['Primitive (variableRead mgctx (variable))]
    ['NewTerm (nestedTerm mgctx)]
    ['NewBlock (let* ([bgcxt (new method-generation-context% [classCtx (send mgctx getHolder)] [outerCtx mgctx])]
                       [blockBody (nestedBlock mgctx)]
                       [blockMethod (send bgcxt assemble blockBody)])
                 (send mgctx addEmbeddedBlockMethod blockMethod)
                 (if (send mgctx requiresContext)
                     (BlockNodeWithContext blockMethod)
                     (BlockNode 0 blockMethod (send (send mgctx getClassCtx) get-name))))]
    [_ (literal)]))
(define (variable) (identifier))
(define (identifier? s)
  (or (equal? s 'Identifier) (equal? s 'Primitive)))
(define (messages mgctx receiver)
  ;(displayln `(messages ,sym ,text))
  (define msg #f)
  (if (identifier? sym)
        (begin
          (set! msg (unaryMessage receiver))
          (do () ((not (identifier? sym)))
            (set! msg (unaryMessage msg)))
          (do () ((not (or (equal? sym 'OperatorSequence) (member? sym binaryOps))))
             (set! msg (binaryMessage mgctx msg)))
          (when (equal? sym 'Keyword)
            (set! msg (keywordMessage mgctx receiver))))
        (if (or (equal? sym 'OperatorSequence) (member? sym binaryOps))
            (begin
              (set! msg (binaryMessage mgctx receiver))
              (do () ((not (or (equal? sym 'OperatorSequence) (member? sym binaryOps))))
                (set! msg (binaryMessage mgctx msg)))
              (when (equal? sym 'Keyword)
                (set! msg (keywordMessage mgctx receiver)))
              )
            (set! msg (keywordMessage mgctx receiver)))

        )
  msg)
(define (unaryMessage receiver)
  ;(createMessageSend (unarySelector) (expression-node receiver))
  (UninitializedMessageSendNode 0 (unarySelector) (list receiver))
  )
(define (binaryMessage mgctx receiver)
  ;(displayln `(binaryMessage ,sym ,text))
  ;(UninitializedMessageSendNode (binarySelector) (ExpressionNode receiver (binaryOperand mgctx)))
  (UninitializedMessageSendNode 0 (binarySelector) (list receiver (binaryOperand mgctx)))
  )
(define (binaryOperand mgctx)
  ;(displayln `(binaryOperand ,sym ,text))
  (define operand (primary mgctx))
  (do () ((not (identifier? sym)))
    (set! operand (unaryMessage operand)))
  operand)
(define (keywordMessage mgctx receiver)
  ;(displayln `(keywordMessage ,sym ,text))
  (let ([arguments (list receiver)]
        [kw ""]
        [return #f])
    (do () ((not (equal? sym 'Keyword)))
      (begin
        (set! kw (string-append (~a (keyword)) kw))
        (set! arguments (cons (formula mgctx) arguments))))
    ;(displayln `(keywordMessage ,sym ,text kw: ,kw length: ,(length arguments)))
    (if (equal? (length arguments) 2)
        (set! return (match kw
          ["ifTrue:" (IfInlinedLiteralNode (car arguments) #t (inline (LiteralNode (cadr arguments))) (cadr arguments))]
          ["ifTrue:" (IfInlinedLiteralNode (car arguments) #f (inline (LiteralNode (cadr arguments))) (cadr arguments))]
          ["whileTrue:" (WhileInlinedLiteralNode (inline (LiteralNode (car arguments))) #t (inline (LiteralNode (cadr arguments))) (cadr arguments))]
          ["whileFalse:" (WhileInlinedLiteralNode (inline (LiteralNode (car arguments))) #t (inline (LiteralNode (cadr arguments))) (cadr arguments))]
          ["or:" (OrInlinedLiteralNode (car arguments) (inline (cadr arguments) mgctx) (cadr arguments))]
                       ["and:" (AndInlinedLiteralNode (car arguments) (inline (cadr arguments) mgctx) (cadr arguments))]
                       [_ #f]))
        (when (equal? (length arguments) 3)
            (if (and (equal? kw "ifTrue:ifFalse") (LiteralNode? (cadr arguments)) (LiteralNode? (caddr arguments)))
                (set! return (IfTrueIfFalseInlinedLiteralsNode
                 (car arguments)
                 (inline (cadr arguments) mgctx)
                 (inline (caddr arguments) mgctx)
                 (cadr arguments)
                 (caddr arguments)))
            (if (and (equal? kw "to:do") (LiteralNode? (caddr arguments)))
              (set! return (IntToDoLiteralsInlinedNode (inline (caddr arguments) mgctx)))
              (set! return (UninitializedMessageSendNode 0 kw arguments))))
            ))
    return))
(define (formula mgctx)
  ;(displayln `(formula ,sym ,text))
  (define operand (binaryOperand mgctx))
  ;(displayln `(formula operand: ,operand ,sym ,text ,(member? sym binaryOps)))
  (do () ((not (or (equal? sym 'OperatorSequence) (member? sym binaryOps))))
    (set! operand (binaryMessage mgctx operand)))
  ;(displayln `(formulaEnd operand: ,operand ,sym ,text ,(member? sym binaryOps)))
  operand)
(define (nestedTerm mgctx)
  (expect 'NewTerm "nestedTerm")
  (define exp (expression mgctx))
  (expect 'EndTerm "nestedTerm")
  exp)
(define (literal)
  ;(displayln `(literal ,sym ,text))
  (match sym
    ['Pound (if (equal? (peek) 'NewTerm)
                                  (begin (expect 'Pound "literal") (ArrayLiteralNode 0 (literalArray)))
                                  (SymbolLiteralNode 0 (literalSymbol)))]
    ['STString (StringLiteralNode 0 (literalString))]
    ['STChar (CharLiteralNode 0 (literalChar))]
    [_ (let* ([isNegative? (isNegativeNumber)])
         ;(displayln `(literal-number ,sym ,text ,value))
         (if (equal? sym 'Integer)
             (let ([value (literalInteger isNegative?)])
               (if (or (< value LONG_MIN) (> value LONG_MAX))
                   (BigIntegerLiteralNode 0 value)
                   (IntegerLiteralNode 0 value)))
           (if (equal? sym 'Double)
               (DoubleLiteralNode 0 (literalDouble isNegative?))
               (parser-error (string-append "invalid literal " (~a sym) " " (~a text)) "literal"))))]))
(define (isNegativeNumber)
  (if (equal? sym 'Minus)
      (begin (expect 'Minus "isNegativeNumber") #t)
      #f))
(define (literalInteger isNegative)
  (let ([i text])
    (when isNegative
      (set! i (- 0 i)))
    (expect 'Integer "literalInteger")
    i))
(define (literalDouble isNegative)
  (displayln `(literalDouble ,sym ,text))
  (let ([d text])
    (when isNegative
      (set! d (- 0.0 d)))
    (expect 'Double "literalDouble")
    d))
(define (literalSymbol)
  ;(displayln `(literalSymbol ,sym ,text))
  (expect 'Pound "literalSymbol")
  (if (equal? sym 'STString)
      (string)
      (selector)))
(define (literalArray)
  (define literals '())
  (expect 'NewTerm "literalArray")
  (do () ((equal? sym 'EndTerm))
    (set! literals (cons (getObjectForCurrentLiteral) literals)))
  
  (expect 'EndTerm "literalArray"))
  
(define (getObjectForCurrentLiteral)
  ;(displayln `(gofc ,sym ,text))
  (match sym
    ['NewTerm (literalArray)]
    ['Pound (if (equal? (peek) 'NewTerm)
                (begin  (expect 'Pound "getObjectForCurrentLiteral")  (literalArray))
                (literalSymbol))]
    ['STString (literalString)]
    ['STChar (literalChar)]
    ['Integer (literalInteger (isNegativeNumber))]
    ['Double (literalDouble (isNegativeNumber))]
    ['Identifier (if (equal? text "nil")
                     (begin (selector) "NIL");;fixme
                     (if (equal? text "true")
                         (begin (selector) (send universe getTrueObject))
                         (if (equal? text "false")
                             (begin (selector) (send universe getTrueObject))
                             (selector))))]
    ['OperatorSequence (selector)]
    ['Keyword (selector)]
    ['KeywordSequence (selector)]
    [_ (parser-error "could not parse literal array value" "getObjectForCurrentLiteral")]))

(define (literalString) (string))
(define (literalChar)
  ;(displayln `(literalChar ,sym ,text))
  (let ([val (string-ref text 0)])
    (expect 'STChar "literalChar")
    val))
(define (selector)
  ;(displayln `(selector ,sym ,text))
    (if (or (equal? sym 'OperatorSequence) (member? sym singleOps))
        (binarySelector)
        (if (or (equal? sym 'Keyword) (equal? sym 'KeywordSequence))
            (keywordSelector)
            (unarySelector))))
(define (keywordSelector)
  (expectOneOf sym keywordSelectorOps)
  text)
(define (string)
  (expect 'STString "string")
  text)
(define (nestedBlock mgctx)
  ;(displayln `(nestedBlock ,sym ,text))
  (expect 'NewBlock "nestedBlock")
  (send mgctx addArgumentIfAbsent "$blockSelf")
  (when (equal? sym 'Colon) (blockPattern mgctx))
  (define blockSig (string-append "$blockMethod@line#@currentcolumnFIXME!"));;fixme
  (define argSize (send mgctx getNumberOfArguments))
  ;(for ([i
  (send mgctx setSignature blockSig)
  (define expressions (blockContents mgctx))
  (expect 'EndBlock "nestedBlock")
  expressions)
(define (blockPattern mgctx)
  (blockArguments mgctx)
  (expect 'Or "blockPattern"))
(define (blockArguments mgctx)
  (do () ((not (equal? sym 'Colon)))
    (begin
      (expect 'Colon "blockArguments")
      (send mgctx addArgumentIfAbsent (argument)))))
(define (variableRead mgctx variableName)
  (if (equal? variableName "super")
      (send mgctx getSuperReadNode)
      (if (equal? variableName "thisContext")
          (send mgctx getThisContextNode)
          (if (not #f);fixme(null? (send mgctx getVariable variableName)))
              (void);(send mgctx getLocalReadNode variableName)
              (let ([fileRead (send mgctx getObjectFieldRead variableName)])
                (if (not (null? fileRead))
                    fileRead
                    (send mgctx getGlobalRead variableName)))))))
(define (variableWrite mgctx variableName exp)
  (define variable (send mgctx getLocal variableName))
  (if (not (null? variable))
      (void);fixme(send mgctx getLocalWriteNode variableName exp)
      (let ([fieldWrite (send mgctx getObjectFieldWrite variableName exp)])
        (if (not (null? fieldWrite))
            fieldWrite
            (parser-error (string-append
                    "neither a variable nor a field found in current scope that is named "
                    variableName ". Arguments are read-only") "variableWrite")))))

(define (test)
  ;(define cgctx (new class-generation-context% [name "Array"] [superName "nil"]))
  ;(next)
  ;(time (classDef cgctx))
  (define files (find-files file-exists? "../SOM/Smalltalk"))
  (for ([f files])
    (when (not (member? (~a f) '(
                                 "../SOM/Smalltalk/AST-Core/RBExplicitVariableParser.som"
                                 ;"../SOM/Smalltalk/AST-Core/RBMessageNode.som"
                                 "../SOM/Smalltalk/AST-Core/RBParseTreeSearcher.som"
                                 "../SOM/Smalltalk/AST-Core/RBParser.som"
                                 "../SOM/Smalltalk/AST-Core/RBPatternParser.som"
                                 "../SOM/Smalltalk/AST-Core/RBPatternVariableNode.som"
                                 ;"../SOM/Smalltalk/AST-Core/RBScanner.som"
                                 ;"../SOM/Smalltalk/AST-Core/TRBProgramNodeVisitor.som"
                                 ;"../SOM/Smalltalk/Collections/Streams/LimitedWriteStream.som"
                                 "../SOM/Smalltalk/Collections/Streams/LimitingLineStreamWrapper.som"
                                 "../SOM/Smalltalk/Collections/Streams/MultiByteBinaryOrTextStream.som"
                                 "../SOM/Smalltalk/Collections/Streams/PositionableStream.som"
                                 "../SOM/Smalltalk/AST-Core/RBPatternBlockNode.som"
                                 "../SOM/Smalltalk/FileSystem/Core/FileLocator.som"
                                 "../SOM/Smalltalk/FileSystem/Core/FileSystemDirectoryEntry.som"
                                 "../SOM/Smalltalk/FileSystem/Core/FileSystemGuide.som"
                                 "../SOM/Smalltalk/FileSystem/Core/PlatformResolver.som"
                                 "../SOM/Smalltalk/FileSystem/Disk/WindowsStore.som"
                                 "../SOM/Smalltalk/FileSystem/Streams/AsyncFile.som"
                                 )))
      (displayln `(parsing ,f))
      (set! port (open-input-file f))
      (set! nxt (lex port))
      (next)
      (with-handlers ([parse-exception? (lambda (err) (displayln `(error with ,f)))])
        (time (classDef (new class-generation-context% [name "Array"] [superName "nil"])))))
  ))
(test)