#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
        ; syntax/readerr
         )
(provide lex lex-all som-lexer)
(define-tokens datum-tokens ( DATUM Identifier Num Keyword KeywordSequence STString STChar Integer Double OperatorSequence))
(define-empty-tokens empty-tokens (EOF Equal Primitive NewTerm EndTerm Or Exit Period
                                       Assign Pound NewBlock EndBlock Colon Not And Star Div
                                       Mod Plus More Less Comma At Per Minus ))

(define som-lexer
  (lexer-src-pos
   ;[whitespace (token-DATUM 'WHITESPACECHARACTER lexeme )]
   [(:or whitespace comment) (return-without-pos (som-lexer input-port))]
   [comment (token-DATUM 'COMMENT lexeme)]
   [(:: identifier ":") (token-Keyword (string->symbol lexeme))]
   [(:: "primitive") 'Primitive]
   [identifier (token-Identifier (string->symbol lexeme))]
   [#\( 'NewTerm]
   [#\) 'EndTerm]
   [#\= 'Equal]
   [#\| 'Or]
   [#\, 'Comma]
   [#\- 'Minus]
   [#\~ 'Not]
   [#\& 'And]
   [#\* 'Star]
   [#\/ 'Div]
   [#\\ 'Mod]
   [#\+ 'Plus]
   [#\> 'More]
   [#\< 'Less]
   [#\@ 'At]
   [#\# 'Pound]
   [#\% 'Per]
   [#\: 'Colon]
   [#\. 'Period]
   [":=" 'Assign]
   [#\^ 'Exit]
   [#\[ 'NewBlock]
   [#\] 'EndBlock]
   ;["''" (token-STString lexeme)]
   [separator 'Separator]
   [decimal-integer-literal (token-Integer (string->number lexeme))]
   [string-literal (token-STString lexeme)]
   [operator-sequence (token-OperatorSequence lexeme)]
   ;[boolean-literal (token-BOOLEAN lexeme)]
   ;[(eof) (return-without-pos 'EOF)]
   [(eof) 'EOF]
   [any-char lexeme]
   ))

(define-lex-abbrevs
  [whitespace (:or " " "\n" "\t" "\r")]
  [line-terminator (:or #\u000A #\u000D #\u2028 #\u2029)]
  [constantReference (:or "nil" "true" "false")]
  [pseudoVariableReference (:or "self" "super" "thisContext")]
  [binarySelectorChar (:or "~" "!" "@" "%" "&" "*" "-" "+" "=" "|" "\\" "<" ">" "," "?" "/")]
  [miscChar (:or "$" ";" ":" "." ":=" "^")]
  [keyword (:or constantReference pseudoVariableReference binarySelectorChar miscChar)]
  [operator (:or "~" "@" "%" "&" "*" "-" "+" "=" "|" "\\" "<" ">" "," "/")]
  [operator-sequence (:+ operator)]
  [reservedIdentifier (:or pseudoVariableReference constantReference)]
  [comment (:: "\"" (complement (:: any-string "\"" any-string)) "\"")]
  [separator (:>= 5 "-")];don't know the exact number
  [identifier (:: identifier-start (:* identifier-part))]
  [identifier-start (:or alphabetic #\$ #\_)]
  [identifier-part (:or identifier-start numeric)]
  [null-literal "null"]
  [boolean-literal (:or "true" "false")]
  [numeric-literal (:or decimal-literal hex-integer-literal)]
  [decimal-literal
   (:: (:or (:: decimal-integer-literal #\. (:* decimal-digit))
            (:: #\. (:+ decimal-digit))
            decimal-integer-literal)
       (:? (:: (:or #\e #\E)
               (:or #\+ #\- nothing)
               (:+ decimal-digit))))]
  [decimal-integer-literal (:or #\0 (:: (:- decimal-digit #\0) (:* decimal-digit)))]
  [decimal-digit (:/ #\0 #\9)]
  [hex-integer-literal (:: (:or "0x" "0X") (:+ (:/ #\0 #\9 #\a #\f #\A #\F)))]
  [string-literal
   ;(:or (:: #\" (:* (:or (:: #\\ any-char) (char-complement #\"))) #\")
        (:: #\' (:* (:or (:: #\\ any-char) (char-complement #\'))) #\')
        ;)
        ]
  [regexp-literal
   (:: #\/ (:+ (:or (:: #\\ any-char) (char-complement #\/))) #\/ (:* identifier-part))])

(define (lex ip)
  (when (not (port? ip)) (set! ip (open-input-string ip)))
  (port-count-lines! ip)
  (lambda () (som-lexer ip)))

(define (lex-all ip)
  (when (not (port? ip)) (set! ip (open-input-string ip)))
  (port-count-lines! ip)
  (define lst '())
  (letrec ((one-line
	    (lambda ()
	      (let* ([result (som-lexer ip)]
                     [token (position-token-token result)])
		(when (not (equal? token 'EOF))
                  ;(set! lst (cons (~a token) lst))
                  (if (token? token)
                      (set! lst (cons (string-append "(" (~a (token-name token)) " " (~a (token-value token)) ")") lst))
                      (set! lst (cons (~a token) lst)))
                  (one-line))))))
    (one-line))
  (display (string-join (reverse lst) " "))
  )

