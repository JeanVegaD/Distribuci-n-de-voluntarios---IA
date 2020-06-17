#lang racket
(require json)
(require racket/gui/base)


(define (readFile path)
  (with-input-from-file path
    (lambda() 
      (define lista (file->string path))
      (with-input-from-string lista
        (lambda()
          (read-json)
         )
       )
     )
   )
)


(readFile "JSON//places_template.json")



#|
(with-handlers ([string? (lambda (n)
                             (display n))])

|#

;(readFile "C:\\Users\\Samuel\\Desktop\\IA\\Proyecto 1\\Example.json")
;
;variable

#|




(define (load_from_json)

(define path (get-file))
(define path_string (path->string path) )
(define list_hash (readFile path_string))

(for ([sub_hash list_hash])
    (display (hash-iterate-value sub_hash 3));id
    ;(display (hash-iterate-value sub_hash 4));nombre
    ;(display (hash-iterate-value sub_hash 0));ocupacion
    ;(display (hash-iterate-value sub_hash 1));nacionalidad
    ;(display (hash-iterate-value sub_hash 2));languagues
    )
   
    )


;(define subList (hash-values variable))
;subList
;(hash-iterate-value variable 2)
;(hash-ref-key variable ".af" )


|#

