#lang racket
(require json)
( define (readFile path)
         (with-input-from-file path
           (lambda()
            (define lista (file->string path))
           (with-input-from-string
               lista
             (λ () (read-json)))
             ;;(display lista)
             )
           
   
     )
)


;(readFile "C:\\Users\\Samuel\\Desktop\\IA\\Proyecto 1\\Example.json")
(define variable (readFile "JSON//languages.json"))


(define subList (hash-values variable))


;variable

(hash-iterate-value variable 2)
;(hash-ref-key variable ".af" )


