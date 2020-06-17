#lang racket

#|
E: atributos del objeto: nombre, identificacion, nacionalidad, profesion y lenguajes
S: un nuevo objeto de tipo voluntario
R: los lenguajes se presentan en una lista
|#
(define volunteer%
   (class object%
     (super-new)
     (init-field name id nationality profession languages)

     #|
     E: un nuevo lenguaje
     S: lista de lenguajes actualizada
     R: 
     |#
     (define/public (addLanguage language)
       (set! languages (append languages (list language))))
))

#|
E: atributos del objeto: nombre, descripcion y lenguajes
S: un nuevo objeto de tipo lugar
R: los lenguajes se presentan en una lista
|#
(define places%
  (class object%
    (super-new)
    (init-field name description languages)
    
    #|
    E: un nuevo lenguaje
    S: lista de lenguajes actualizada
    R: 
    |#
    (define/public (addLanguage language)
       (set! languages (append languages language)))
    (define/public (getName) (name))
))

#|
E: atributos del objeto: objeto tipo lugar y objeto tipo voluntario
S: un nuevo objeto tipo equipo
R: los atributos deben ser los objetos respectivos
|#
(define %teams
  (class object%
    (field (volunteers null))
    (init-field name place)
    (super-new)
    #|
    E: un nuevo voluntario
    S: lista de los voluntarios en el equipo actualizada
    R: 
    |#
    (define/public (addVolunteer volunteer)
       (set! volunteers (append volunteers (list volunteer))))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;lista de los equipos ya distribuidos
(define teams_list null)
;lista de voluntarios que hay que distribuir
(define volunteers_list null)
;lista de los lugares a los que se van a realizar los voluntariados
(define places_list null)

(define translator_list null)

#|
E: un objeto de tipo voluntario
S: lista de voluntario actualizada
R: la entrada debe ser de tipo voluntario
|#
(define (addVolunteer volunteer)
    (set! volunteers_list (append volunteers_list (list volunteer)))
)


#|
E: un objeto de tipo place(lugar de voluntariado)
S: lista de places actualizada
R: la entrada debe ser de tipo place
|#
(define (addPlace place)
    (set! places_list (append places_list (list place)))
)


#|
E: lista con los voluntarios
S: una lista con los voluntarios con profesion de traductor
R: la lista no puede ser vacia
|#
(define (searchTranslators)
    (for ([volunteer volunteers_list])
      (define mate (get-field profession volunteer))
      (cond
        [( equal? mate "translator");verifica si su profesion es de traductor
        (set! translator_list (append translator_list (list volunteer)));se añade a la lista de traductores
        (set! volunteers_list (remove volunteer volunteers_list))]);se elimina de la lista de voluntarios
     )

)

#|
E: lista con los voluntarios que son traductores, lista de los lugares
S: una lista con los equipos creados por lugar, con cada traductor añadido según encaje en el lugar por los lenguahes
R: la lista no puede ser vacia
|#
(define (addTranslator)
  (define counter 0)
  (for ([p_place places_list])
    (set! counter (+ counter 1))
    (define number (number->string counter))
    (define team (new %teams [name (string-append "Team " number)] [place p_place]));creacion de nuevo equipo
    (define place_languages (get-field languages p_place));obtener los lenguajes del lugar
    (for ([translator translator_list])
      (define translator_languages (get-field languages translator));obtener los lenguajes del voluntario
      (define language_flag (existLanguage place_languages translator_languages));bandera de lenguajes
      (define profession_flag (existProfesion (get-field profession translator) team));bandera de profesion
      (cond [(and (equal? language_flag #t) (equal? profession_flag #t))
             (send team addVolunteer translator);añadir traductor al equipo
             (send p_place addLanguage translator_languages)
             (set! translator_list (remove translator translator_list));eliminar traductor de la lista
             ]))
    (set! teams_list (append teams_list (list team) )));agrega nuevo equipo a la lista
  )



#|
E: lista con los voluntarios,lista de los lenguajes de los lugares
S: una lista con los voluntarios que cumplen las condiciones, funcion auxiliar
R: las listas no puede ser vacia
|#
(define (group)
  (for ([p_place places_list]);recorrido de los lugares
    (define place_languages (get-field languages p_place));obtener los lenguajes del lugar
    (for ([team teams_list])
      (for ([volunteer volunteers_list]);recorrido de los voluntarios
        (define volunteer_languages (get-field languages volunteer));obtener los lenguajes del voluntario
        (define language_flag (existLanguage place_languages volunteer_languages));bandera de lenguajes
        (define v_language_flag (existVolanguage volunteer_languages team));bandera de lenguajes
        (define profession_flag (existProfesion (get-field profession volunteer) team));bandera de profesion
        (define nationality_flag (existNationality (get-field nationality volunteer) team));bandera de nacionalidad
        (displayln (get-field name volunteer))
        (displayln language_flag)
        (displayln (get-field name p_place))
        (cond [(and (equal? language_flag #t) (equal? profession_flag #t) (equal? nationality_flag #t) (equal? v_language_flag #t));verifica si cumple las restricciones
               (send team addVolunteer volunteer);añadir voluntario al equipo
               (set! volunteers_list (remove volunteer volunteers_list));eliminar voluntario de la lista
               ]
           
      ))))
    (fixTeamList)
  )

#|
E: lista con los equipos distribuidos
S: la lista de los equipos sin los lugares que no tienen miembros
R: las listas no puede ser vacia
|#
(define (fixTeamList)
  (for-each (lambda (actual);recorrido de los equipos
              (define volunteers (get-field volunteers actual))
              (cond [(empty? volunteers)
                     (set! teams_list (remove actual teams_list))])
              )teams_list
    ))


(define (trackTeams)
  (for-each (lambda (actual);recorrido de los equipos
              (define team_place (get-field name (get-field place actual)))
              (define team_volunteers (get-field volunteers actual))
              (define name (get-field name actual))
              (displayln name)
              (displayln team_place)
              (for ([volunteer team_volunteers])
                (displayln (get-field name volunteer))
                )(displayln "----\n"))teams_list))


#|
E: lista con los lenguajes de los lugares, lenguajes de los voluntarios
S: verdadero si coinciden lenguajes del lugar con lenguajes del voluntario, falso si no
R: las listas no puede ser vacia
|#
(define (existLanguage p_languages l_languages)
  (let ([flag #f])
    (for-each (lambda (element);recorrido de la lista de lenguajes del lugar
                 (cond [ (equal? (findf (lambda (lang) (equal? element lang) ) l_languages) #f)];verifica si un elemento de la lista de lenguajes pertenece a la otra
                       [else (set! flag #t)]))
    p_languages)
  (cond [(equal? flag #f) #f];devuelve falso en caso de que no haya encontrado algun lenguaje
        [else #t]);devuelve verdadero en caso de que haya encontrado alguno
  ))

#|
E: lista con los lenguajes del voluntario, equipo actual
S: verdadero si coinciden los lenguajes del voluntario con los que pertenecen al equipo, falso si no
R: las listas no puede ser vacia
|#
(define (existVolanguage v_languages actual_team)
  (let ([flag #t])
   (define actual (get-field volunteers actual_team))
    (cond [(not (empty? actual))
           (for-each (lambda (element);recorrido de los voluntarios
          (define actual_languages (get-field languages element));obtiene los lenguajes del voluntario actual
           (set! flag (existLanguage actual_languages v_languages));cambia el estado de la bandera en caso de que un caso no se cumpla
              ) actual)]
          [else #t])
  
    flag;retorno del resultado de la bandera
    )
  )


#|
E: lista con los lenguajes de los lugares, lenguajes de los voluntarios
S: una lista con los voluntarios con agrupados
R: las listas no puede ser vacia
|#
(define (existProfesion profession actual_team)
  (let ([flag #t])
   (define actual (get-field volunteers actual_team))
    (cond [(not (empty? actual))
           (for-each (lambda (element)
          (define prof (get-field profession element))
        (cond [(equal? prof profession) (set! flag #f)];se setea la bandera falso porque no cumple la restriccion
        )
              ) actual)]
          [else #t])
  
    flag
    )
    
  )

#|
E: recibe la nacionalidad de un voluntario
S: valor #t o #f si exite un voluntario con esa nacionalidad en el equipo 
R: las listas no puede ser vacia
|#
(define (existNationality nationality actual_team)
  (let ([flag #t])
    (define actual (get-field volunteers actual_team))
    (cond [(not (empty? actual))
  (for-each (lambda (element)
          (define nat (get-field nationality element))
        (cond [(equal? nat nationality) (set! flag #f)];se setea la bandera falso porque no cumple la restriccion
              [else (set! flag #t)];se setea la bandera verdadero porque cumple la restriccion no existe alguien de la misma profesion en el equipo
        )
              ) actual)]
          [else #t])
    flag
    )
    
  )


(provide (all-defined-out))