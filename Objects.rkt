#lang racket

#|
E: atributos del objeto: nombre, identificacion, nacionalidad, profesion y lenguajes
S: un nuevo objeto de tipo voluntario
R: los lenguajes se presentan en una lista
|#
(define volunteer%
   (class object%
     (super-new)
     (init-field name id nationallity profession languages)

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
       (set! languages (append languages (list language))))
))

#|
E: atributos del objeto: objeto tipo lugar y objeto tipo voluntario
S: un nuevo objeto tipo equipo
R: los atributos deben ser los objetos respectivos
|#
(define %teams
  (class object%
    (super-new)
    (init-field place volunteers)

    #|
    E: un nuevo voluntario
    S: lista de los voluntarios en el equipo actualizada
    R: 
    |#
    (define/public (addVolunteer volunteer)
       (set! volunteers (append volunteers (list volunteer))))
))


     
     
     