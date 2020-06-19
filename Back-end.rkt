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
     (define/public (addLanguage param_language)
       (set! languages (append languages (list param_language))))
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
    (define/public (addLanguage param_language)
       (set! languages (append languages param_language)))
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
    (define/public (addVolunteer param_volunteer)
       (set! volunteers (append volunteers (list param_volunteer))))
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
S: lista de voluntario actualizada, voluntario agregado
R: la entrada debe ser de tipo voluntario
|#
(define (addVolunteer param_volunteer)
    (set! volunteers_list (append volunteers_list (list param_volunteer)))
)

#|
E: un objeto de tipo voluntario
S: lista de voluntario actualizada, voluntario eliminado
R: la entrada debe ser de tipo voluntario
|#
(define (deleteVolunteer param_volunteer)
    (set! volunteers_list (remove param_volunteer volunteers_list))
)

#|
E: un objeto de tipo place(lugar de voluntariado)
S: lista de places actualizada, lugar agregado
R: la entrada debe ser de tipo place
|#
(define (addPlace param_place)
    (set! places_list (append places_list (list param_place)))
)

#|
E: un objeto de tipo place(lugar de voluntariado)
S: lista de places actualizada, lugar eliminado
R: la entrada debe ser de tipo place
|#
(define (deletePlace param_place)
    (set! places_list (remove param_place places_list))
)

#|
E: las listas goblales, voluntarios, lugares, equipos y traductores
S: las listas reinicializadas
R: 
|#
(define (clearLists)
  (set! volunteers_list null)
  (set! translator_list null)
  (set! places_list null)
  (set! teams_list null))


#|
E: lista con los voluntarios
S: una lista con los voluntarios con profesion de traductor
R: la lista no puede ser vacia
|#
(define (searchTranslators)
    (for ([v_volunteer volunteers_list])
      (define v_profession (get-field profession v_volunteer))
      (cond
        [( equal? v_profession "translator");verifica si su profesion es de traductor
        (set! translator_list (append translator_list (list v_volunteer)));se añade a la lista de traductores
        (set! volunteers_list (remove v_volunteer volunteers_list))]);se elimina de la lista de voluntarios
     )

)

#|
E: lista con los voluntarios que son traductores, lista de los lugares
S: una lista con los equipos creados por lugar, con cada traductor añadido según encaje en el lugar por los lenguahes
R: la lista no puede ser vacia
|#
(define (addTranslator)
  (define v_counter 0)
  (for ([p_place places_list])
    (set! v_counter (+ v_counter 1))
    (define v_number (number->string v_counter))
    (define v_team (new %teams [name (string-append "Team " v_number)] [place p_place]));creacion de nuevo equipo
    (define place_languages (get-field languages p_place));obtener los lenguajes del lugar
    (for ([translator translator_list])
      (define translator_languages (get-field languages translator));obtener los lenguajes del voluntario
      (define language_flag (existLanguage place_languages translator_languages));bandera de lenguajes
      (define profession_flag (existProfesion (get-field profession translator) v_team));bandera de profesion
      (cond [(and (equal? language_flag #t) (equal? profession_flag #t))
             (send v_team addVolunteer translator);añadir traductor al equipo
             (send p_place addLanguage translator_languages)
             (set! translator_list (remove translator translator_list));eliminar traductor de la lista
             ]))
    (set! teams_list (append teams_list (list v_team) )));agrega nuevo equipo a la lista
  )



#|
E: lista con los voluntarios,lista de los lenguajes de los lugares
S: una lista con los voluntarios que cumplen las condiciones, funcion auxiliar
R: las listas no puede ser vacia
|#
(define (group)
    (for ([t_team teams_list])
      (define p_place (get-field place t_team))
      (define place_languages (get-field languages p_place));obtener los lenguajes del lugar
      (for ([volunteer volunteers_list]);recorrido de los voluntarios
        (define volunteer_languages (get-field languages volunteer));obtener los lenguajes del voluntario
        (define language_flag (existLanguage place_languages volunteer_languages));bandera de lenguajes
        (define v_language_flag (existVolanguage volunteer_languages t_team));bandera de lenguajes
        (define profession_flag (existProfesion (get-field profession volunteer) t_team));bandera de profesion
        (define nationality_flag (existNationality (get-field nationality volunteer) t_team));bandera de nacionalidad
        (cond [(and (equal? language_flag #t) (equal? profession_flag #t) (equal? nationality_flag #t) (equal? v_language_flag #t));verifica si cumple las restricciones
               (send t_team addVolunteer volunteer);añadir voluntario al equipo
               (set! volunteers_list (remove volunteer volunteers_list));eliminar voluntario de la lista
               ]
           
      )))
    (fixTeamList)
  )

#|
E: lista con los equipos distribuidos
S: eliminacion de equipos que no tienen miembros
R: las listas no puede ser vacias
|#
(define (fixTeamList)
  (for-each (lambda (t_actual);recorrido de los equipos
              (define l_volunteers (get-field volunteers t_actual))
              (cond [(empty? l_volunteers)
                     (set! teams_list (remove t_actual teams_list))])
              )teams_list
    )
  (existTranslator)
  )

#|
E: lista de los traductores y la lista de los equipos distribuidos
S: añade a los traductores que sobran en el equipo que encaje por los lenguajes
R: las listas no puede ser vacia
|#
(define (existTranslator)
  (cond [(not(empty? translator_list))
         (for-each (lambda (t_team);recorrido de los equipos
              (for-each (lambda (t_translator)
                          (define trans_languages (get-field languages t_translator))
                          (define language_flag (existVolanguage trans_languages t_team));bandera de lenguajes     
                          (cond [(equal? language_flag #t)
                                 (send t_team addVolunteer t_translator);añadir voluntario al equipo
                                 (set! translator_list (remove t_translator translator_list))
                                 ]))
                        translator_list)
              )teams_list
    )]))




#|
E: lista con los lenguajes de los lugares, lenguajes de los voluntarios
S: verdadero si coinciden lenguajes del lugar con lenguajes del voluntario, falso si no
R: las listas no puede ser vacia
|#
(define (existLanguage param_v_languages param_p_languages)
  (let ([v_flag #f])
    (for-each (lambda (v_language);recorrido de la lista de lenguajes del lugar
                 (cond [ (equal? (findf (lambda (p_language) (equal? v_language p_language) ) param_p_languages) #f)];verifica si un elemento de la lista de lenguajes pertenece a la otra
                       [else (set! v_flag #t)]))
    param_v_languages)
  (cond [(equal? v_flag #f) #f];devuelve falso en caso de que no haya encontrado algun lenguaje
        [else #t]);devuelve verdadero en caso de que haya encontrado alguno
  ))

#|
E: lista con los lenguajes del voluntario, equipo actual
S: verdadero si coinciden los lenguajes del voluntario con los que pertenecen al equipo, falso si no
R: las listas no puede ser vacia
|#
(define (existVolanguage param_v_languages param_actual_team)
  (let ([v_flag #t])
   (define v_actual (get-field volunteers param_actual_team))
    (cond [(not (empty? v_actual))
           (for-each (lambda (v_volunteers);recorrido de los voluntarios
          (define actual_languages (get-field languages v_volunteers));obtiene los lenguajes del voluntario actual
           (set! v_flag (existLanguage actual_languages param_v_languages));cambia el estado de la bandera en caso de que un caso no se cumpla
              ) v_actual)]
          [else #t])
  
    v_flag;retorno del resultado de la bandera
    )
  )


#|
E: lista con los lenguajes de los lugares, lenguajes de los voluntarios
S: una lista con los voluntarios con agrupados
R: las listas no puede ser vacia
|#
(define (existProfesion param_profession param_actual_team)
  (let ([v_flag #t])
   (define v_actual (get-field volunteers param_actual_team))
    (cond [(not (empty? v_actual))
           (for-each (lambda (v_volunteers)
          (define v_profession (get-field profession v_volunteers))
        (cond [(equal? v_profession param_profession) (set! v_flag #f)];se setea la bandera falso porque no cumple la restriccion
        )
              ) v_actual)]
          [else #t])
  
    v_flag
    )
    
  )

#|
E: recibe la nacionalidad de un voluntario
S: valor #t o #f si exite un voluntario con esa nacionalidad en el equipo 
R: las listas no puede ser vacia
|#
(define (existNationality param_nationality actual_team)
  (let ([v_flag #t])
    (define v_actual (get-field volunteers actual_team))
    (cond [(not (empty? v_actual))
  (for-each (lambda (v_volunteers)
          (define v_nationality (get-field nationality v_volunteers))
        (cond [(equal? v_nationality param_nationality) (set! v_flag #f)];se setea la bandera falso porque no cumple la restriccion
              [else (set! v_flag #t)];se setea la bandera verdadero porque cumple la restriccion no existe alguien de la misma profesion en el equipo
        )
              ) v_actual)]
          [else #t])
    v_flag
    )
    
  )


#|
E: recibe la nacionalidad de un voluntario
S: valor #t o #f si exite un voluntario con esa nacionalidad en el equipo 
R: las listas no puede ser vacia
|#

(define (existId param_id)
  (let ([v_flag #f])
    (for-each (lambda (v_volunteer)
                (define volunteer_id (get-field id v_volunteer))
                (cond [(equal? param_id volunteer_id) (set! v_flag #t)];se setea la bandera verdader si existe alguien con el mismo id
                      )
              ) volunteers_list)
   v_flag)
 )

(provide (all-defined-out))