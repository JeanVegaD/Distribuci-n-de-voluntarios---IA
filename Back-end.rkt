(require "Objects.rkt")

;lista de los equipos ya distribuidos
(define teams null)
;lista de voluntarios que hay que distribuir
(define volunteers null)
;lista de los lugares a los que se van a realizar los voluntariados
(define places null)

#|
E: un objeto de tipo voluntario
S: lista de voluntario actualizada
R: la entrada debe ser de tipo voluntario
|#
(define (addVolunteer volunteer)
    (set! volunteers (append volunteers (list volunteer)))
)

#|
E: un objeto de tipo place(lugar de voluntariado)
S: lista de places actualizada
R: la entrada debe ser de tipo place
|#
(define (addPlace place)
    (set! places (append places (list place)))
)

#|
E: lista con los voluntarios
S: una lista con los voluntarios con profesion de traductor
R: la lista no puede ser vacia
|#
(define (searchTranslators list_volunteers)
    (let list_translator null)
    (for[ i list_volunteers]
        (let ((mate get-field profession i)))
        (cond ( = mate "translator")
            list_translator (append list_translator (list i))
        )
    )

)