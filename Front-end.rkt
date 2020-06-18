
#lang racket/gui
(require racket/gui/base)
(require json)
(require "back-end.rkt")


;funciones generales del programa

#|
E: Path de un archivo
S: Lista con la informacion contenida del archivo
R: El formato de la estructura del json debe ser simple 
|#
( define (readFile path)
         (with-input-from-file path
            (lambda()
              (define lista (file->string path))
              (with-input-from-string
               lista
             (λ () (read-json)))
             )
     )
)



#|
UI:frame principal 
|#
(define frame_principal (new frame%
                   [label "Volunteer distribution"]
                   [width 1000]
                   [height 650]))

(send frame_principal show #t)


#|
UI:divide el frame principal en 2 de forma horizontal
|# 
(define panel_row (new horizontal-pane%
                     (parent frame_principal)
                     ))


#|
UI:panel vertical en el lado izquierdo donde se manejan los controles
|#
(define left_column (new vertical-panel%
                     (parent panel_row)
                     (vert-margin 5)
                     (horiz-margin 5)
                     (min-width 450)
                     (stretchable-width #f)
                     ))

#|
UI: panel derecho donde se visualiza la informacion
|#
(define right_column (new vertical-panel%
                     (parent panel_row)
                     (vert-margin 5)
                     (horiz-margin 5)
                     (min-width 550)
                     ))

#|
UI:Panel especifico de voluntarios
|#
 (define pnl_agregar_voluntarios (new group-box-panel%
                             (parent left_column)
                             (label "Volunteers")
                             (spacing 5)
                             ))
#|
UI:Panel especifico de lugares
|#
 (define pnl_agregar_lugares (new group-box-panel%
                             (parent left_column)
                             (label "Places")
                             (spacing 5)
                             ))



#|
UI: campo de texto nombre del voluntarios
|#
(define txt_nombre (new text-field%
                        (label "Name:         ")
                        (parent pnl_agregar_voluntarios)
                        (init-value "")
                        ))
#|
UI: campo de texto identificador del voluntario
|#
(define txt_id (new text-field%
                        (label "Id:                ")
                        (parent pnl_agregar_voluntarios)
                        (init-value "")))

#|
UI: combobox de nacionalidad de voluntarios
|#
(define txt_nacionalidad (new combo-field%
                        (label "Nationality:")
                        (parent pnl_agregar_voluntarios)
                        (choices (list "afghan"))
                        (init-value "afghan")))


#|
E: obtiene la informacion del json
S: Carga el combobox de nacionalidades
R: No posee
|#
(define lista_nacionalidades (readFile "JSON//nationalities.json"))
(map (lambda (nacionality)
         (send txt_nacionalidad append nacionality))
       lista_nacionalidades)



#|
UI: combobox de profesiones 
|#
(define txt_profesion (new combo-field%
                        (label "Profession: ")
                        (parent pnl_agregar_voluntarios)
                        (choices (list "accountant"))
                        (init-value "accountant")))

#|
E: obtiene la informacion del json
S: Carga el combobox de profesiones
R: No posee
|#
(define lista_profesiones (readFile "JSON//occupations.json"))
(map (lambda (occupation)
         (send txt_profesion append occupation))
       lista_profesiones)


#|
UI: panel con informacion de los lenguajes 
|#
(define panel_seleccion_lenguajes (new horizontal-panel%
                     (parent pnl_agregar_voluntarios)
                     ))


#|
UI: combobox de lenguajes 
|#
(define combo_languaje (new combo-field%
                         (label "Language: ")
                         (parent panel_seleccion_lenguajes)
                         
                         (choices (list "abkhaz"))
                         (init-value "abkhaz")))


#|
UI: boton de agregar lenguajes  
|#
(define btn_add_lenguage (new button%
                    (parent panel_seleccion_lenguajes)
                    (label "Add")
                    (callback (lambda (b e)(add_language_list)))
                    ))

#|
E: Campo de valor en el combobox
S: Agregar al listbox el valor obtenido
R: No posee
|#
(define (add_language_list)
  (send list_box_lenguages append (send combo_languaje get-value)))


#|
UI: list box de lenguajes  
|#

(define list_box_lenguages (new list-box%
                      (label "")
                      (parent (new horizontal-panel%
                                   (parent pnl_agregar_voluntarios)
                                   (style (list 'border))))
                      (choices (list "sa") )
                      (style (list 'single
                                   'column-headers))
                      (columns (list "Languages"))))

(send list_box_lenguages delete 0)



#|
UI: boton de agregar voluntarios 
|#
(define btn_add_volunteer (new button%
                    (parent pnl_agregar_voluntarios)
                    (label "Add volunteers")
                    (min-height 35)
                    (stretchable-height #f)
                    (stretchable-width #t)
                    (callback (lambda (b e)(add_volunteer_to_list)))
                    ))


#|
E: Campos acerca de los lugares
S: Agrega un nuevo voluntario a la lista 
R: No posee
|#
(define (add_volunteer_to_list)
  ;variables
  
  (define var_nombre (send txt_nombre get-value))
  (define var_identificacionr (send txt_id get-value))
  (define var_nacionalidad (send txt_nacionalidad get-value))
  (define var_profesion (send txt_profesion get-value))

  (define var_idiomas "")
  (define var_lista_idiomas '())
  
  (define list (build-list (send list_box_lenguages get-number) values))
  (for ([i list])
    (set! var_lista_idiomas (append (string-split (send list_box_lenguages get-string i)) var_lista_idiomas))
    (set! var_idiomas (string-append var_idiomas ","))
    (set! var_idiomas (string-append var_idiomas (send list_box_lenguages get-string i)))
    )

  
  
  ;condiciones
  (define rest1 (not(equal? var_nombre "")))
  (define rest2 (not(equal? var_identificacionr "")))
  (define rest3 (not(equal? var_nacionalidad "")))
  (define rest4 (not(equal? var_profesion "")))
  (define rest5 (not(equal? var_idiomas "")))

  
  (define (add_volunteers_to_list_aux)
    (define v1 (new volunteer% [name var_nombre] [id var_identificacionr] [nationality var_nacionalidad] [profession var_profesion] [languages var_lista_idiomas]))
    (addVolunteer v1)
    (set! var_idiomas (substring var_idiomas 1))
    (send list_box_volunteers append "")
    (define index (- (send list_box_volunteers get-number) 1))
    (send list_box_volunteers set-string index var_nombre 0)
    (send list_box_volunteers set-string index var_identificacionr 1)
    (send list_box_volunteers set-string index var_nacionalidad 2)
    (send list_box_volunteers set-string index var_profesion 3)
    (send list_box_volunteers set-string index var_idiomas 4)
    
  )
  
  (if (and rest1 rest2 rest3 rest4 rest5)(add_volunteers_to_list_aux) (message-box "Error" 	
 	 	"Complete all fields"	 
 	 	frame_principal	 
 	)) 
)



#|
UI: boton para cargar desde un json 
|#
(define btn_load_volunteer  (new button%
                    (parent pnl_agregar_voluntarios)
                    (label "Load volunteers from JSON")
                    (min-height 35)
                    (stretchable-height #f)
                    (stretchable-width #t)
                    (callback (lambda (b e)(load_volunteers_from_json)))
                    ))

(define (load_volunteers_from_json)

(define path (get-file))
(define path_string (path->string path) )
(define list_hash (readFile path_string))

(for ([sub_hash list_hash])

     (define var_nombre (hash-iterate-value sub_hash 4))
     (define var_identificacionr (hash-iterate-value sub_hash 3))
     (define var_nacionalidad (hash-iterate-value sub_hash 1))
     (define var_profesion (hash-iterate-value sub_hash 0))
     (define var_idiomas "")
     (for ([lan (hash-iterate-value sub_hash 2)])
       (set! var_idiomas (string-append var_idiomas " , "))
       (set! var_idiomas (string-append var_idiomas lan))
      
    )

    ;se agrega a la clase
    (define v1 (new volunteer% [name var_nombre] [id var_identificacionr] [nationality var_nacionalidad] [profession var_profesion] [languages (hash-iterate-value sub_hash 2)]))
    (addVolunteer v1)
    ;se muestra en pantalla 

    (set! var_idiomas (substring var_idiomas 3))
    (send list_box_volunteers append "")
    (define index (- (send list_box_volunteers get-number) 1))
    (send list_box_volunteers set-string index var_nombre 0)
    (send list_box_volunteers set-string index var_identificacionr 1)
    (send list_box_volunteers set-string index var_nacionalidad 2)
    (send list_box_volunteers set-string index var_profesion 3)
    (send list_box_volunteers set-string index var_idiomas 4)

    )
)


#|
UI: campo de texto del nombre del lugar 
|#
(define txt_nombre_lugar (new text-field%
                        (label "Name:          ")
                        (parent pnl_agregar_lugares)
                        (init-value "")
                        ))

#|
UI: campo de texto del descripcion del lugar 
|#
(define txt_descripcion_lugar (new text-field%
                        (label "Description: ")
                        (parent pnl_agregar_lugares)
                        (init-value "")))

#|
UI: panel con informacion de los lenguajes 
|#
(define panel_seleccion_lenguajes_lugares (new horizontal-panel%
                     (parent pnl_agregar_lugares)
                     ))



#|
UI: combobox con la infromacion de lenguajes
|#
(define combo_languaje_lugares (new combo-field%
                         (label "Language:   ")
                         (parent panel_seleccion_lenguajes_lugares)
                         
                         (choices (list "abkhaz"))
                         (init-value "abkhaz")))


#|
UI: boton de agregar lenguajes  
|#
(define btn_add_lenguage_places (new button%
                    (parent panel_seleccion_lenguajes_lugares)
                    (label "Add")
                    (callback (lambda (b e)(add_language_list_places)))
                    ))



#|
E: Campo de valor en el combobox
S: Agregar al listbox el valor obtenido
R: No posee
|#
(define (add_language_list_places)
  (send list_box_lenguages_places append (send combo_languaje_lugares get-value)))


#|
UI: list box de lenguajes de los lugares 
|#
(define list_box_lenguages_places (new list-box%
                      (label "")
                      (parent (new horizontal-panel%
                                   (parent pnl_agregar_lugares)
                                   (style (list 'border))))
                      (choices (list "sa") )
                      (style (list 'single
                                   'column-headers))
                      (columns (list "Languages"))))

(send list_box_lenguages_places delete 0)


#|
E: obtiene la informacion del json
S: Carga el combobox de nacionalidades
R: No posee
|#
(define lista_lenguajes (readFile "JSON//languages.json"))
(map (lambda (language)
         (send combo_languaje_lugares append language)
       (send combo_languaje append language))
       lista_lenguajes)



#|
UI: boton de agregar un lugar nuevo
|#
(define btn_add_place (new button%
                    (parent pnl_agregar_lugares)
                    (label "Add place")
                    (min-height 35)
                    (stretchable-height #f)
                    (stretchable-width #t)
                    (callback (lambda (b e)(add_place_to_list)))
                    ))


#|
E: Campos acerca de los lugares
S: Crear un nuevo lugar en donde distribuir los voluntarios
R: No posee
|#
(define (add_place_to_list)
  ;variables
  (define var_nombre_lugar (send txt_nombre_lugar get-value))
  (define var_descripcion_lugar (send txt_descripcion_lugar get-value))
  
  (define var_idiomas "")
  (define var_list_idiomas '())
  (define list (build-list (send list_box_lenguages_places get-number) values))
  
  (for ([i list])
    (define sub_list_lang (string-split (send list_box_lenguages_places get-string i)) )
    (set! var_list_idiomas (append  sub_list_lang var_list_idiomas))
    (set! var_idiomas (string-append var_idiomas ","))
    (set! var_idiomas (string-append var_idiomas (send list_box_lenguages_places get-string i)))
    
    )
  
  
  ;condiciones
  (define rest1 (not(equal? var_nombre_lugar "")))
  (define rest2 (not(equal? var_descripcion_lugar "")))
  (define rest3 (not(equal? var_idiomas "")))


  (define (add_place_to_list_aux)
    (define p1 (new places% [name var_nombre_lugar] [description var_descripcion_lugar] [languages var_list_idiomas]))
    (addPlace p1 )
    (send list_box_places append "")
    (set! var_idiomas (substring var_idiomas 1))
    (define index (- (send list_box_places get-number) 1))
    (send list_box_places set-string index var_nombre_lugar 0)
    (send list_box_places set-string index var_descripcion_lugar 1)
    (send list_box_places set-string index var_idiomas 2)
  )

  
  (if (and rest1 rest2 rest3)(add_place_to_list_aux) 
  (message-box "Error" 	
 	 	"Complete all fields"	 
 	 	frame_principal	 
 	))	 

)

#|
UI: boton de agregar conjunto de lugares a traves de un JSON
|#
(define btn_load_places (new button%
                    (parent pnl_agregar_lugares)
                    (label "Load places from JSON")
                    (min-height 35)
                    (stretchable-height #f)
                    (stretchable-width #t)
                    (callback (lambda (b e)(load_places_from_json)))
                    ))

(define (load_places_from_json)

  (define path (get-file))
  (define path_string (path->string path) )
  (define list_hash (readFile path_string))

  (for ([sub_hash list_hash])
    
    (define var_description (hash-iterate-value sub_hash 0))
    (define var_name (hash-iterate-value sub_hash 2))
    (define var_lang "")
    (for ([lan (hash-iterate-value sub_hash 1)])
       (set! var_lang (string-append var_lang " , "))
       (set! var_lang (string-append var_lang lan))
    )
    (set! var_lang (substring var_lang 3))

    ;se agrega el objeto
    (define p1 (new places% [name var_name] [description var_description] [languages (hash-iterate-value sub_hash 1)]))
    (addPlace p1)
    ;se agrega a la interfaz
    (send list_box_places append "")
    (define index (- (send list_box_places get-number) 1))
    (send list_box_places set-string index var_name 0)
    (send list_box_places set-string index var_description 1)
    (send list_box_places set-string index var_lang 2)
   
    )
)



#|
UI: panel especifico para visualizar voluntarios
|#
 (define pnl_voluntarios_disponibles (new group-box-panel%
                             (parent right_column)
                             (label "Available volunteers")
                             ))
#|
UI: panel especifico para visualizar lugares
|#
 (define pnl_voluntariados_disponibles (new group-box-panel%
                             (parent right_column)
                             (label "Available places")
                             ))

#|
UI: panel especifico para los botones 
|#
(define pnl_botones (new panel%
                             (parent right_column)
                             (min-height 50)
                             (stretchable-height #f)
                             ))

#|
UI: boton para distirbuir los voluntarios
|#
(define btn_distribuir_voluntarios (new button%
                    (parent pnl_botones)
                    (label "Distribute volunteers")
                    (min-height 50)
                    (stretchable-height #f)
                    (stretchable-width #t)
                    (callback (lambda (b e)(distribuir_voluntarios)))
                    ))


(define (distribuir_voluntarios)

  (define rest1 (> (send list_box_places get-number) 0))
  (define rest2 (> (send list_box_volunteers get-number) 0))

  (define (distribuir_voluntarios_aux)
    (searchTranslators)
    (addTranslator)
    (group)
    (display_distribution)
    (send frame_principal show #f)
    (send frame_distirbucion show #t)
    )


  (if (and rest1 rest2)(distribuir_voluntarios_aux)
      (message-box "Error" 	
 	 	"Complete all fields"	 
 	 	frame_principal	 
 	))
)



#|
UI: listbox para visualizar voluntarios 
|#
(define list_box_volunteers (new list-box%
                      (label "")
                      (parent (new horizontal-panel%
                                   (parent pnl_voluntarios_disponibles)
                                   (style (list 'border))))
                      (choices (list ) )
                      (style (list 'single
                                   'column-headers))
                      (columns (list  "Name" "Id" "Nationality" "Profession" "Languages"))))

#|
UI: listbox para visualizar lugares
|#
(define list_box_places (new list-box%
                      (label "")
                      (parent (new horizontal-panel%
                                   (parent pnl_voluntariados_disponibles)
                                   (style (list 'border))))
                      (choices (list ))
                      (style (list 'single
                                   'column-headers))
                      (columns (list  "Name" "Description" "Languages"))))






;============================================================================================00







#|
UI:frame distribucion
|#
(define frame_distirbucion (new frame%
                   [label "distribution results"]
                   [width 800]
                   [height 600]))


(define (display_distribution)
  (for ([team teams_list])
    ;se agrega a la interfaz
    (send list_box_teams append "")
    (define index (- (send list_box_teams get-number) 1))
    (send list_box_teams set-string index (get-field name team) 0)
    (send list_box_teams set-string index (get-field name (get-field place team)) 1)
    (send list_box_teams set-string index (get-field description (get-field place team)) 2)
    (send list_box_teams set-string index (string-join (get-field languages (get-field place team)) ",") 3)
    )
  
  )


#|
UI:panel vertical que muestra los voluntarios
|#
(define panel_visual_team (new horizontal-panel%
                     (parent frame_distirbucion)
                     (vert-margin 2)
                     ;(horiz-margin 5)
                     ;(min-width 450)
                     ;(stretchable-width #f)
                     ))

#|
UI: panel ogranizador de widgetes poscionado a la izquierda
|#
(define left_panel_dist (new horizontal-panel%
                     (parent panel_visual_team)
                     (vert-margin 5)
                     ;(horiz-margin 5)
                     ;(min-width 450)
                     ;(stretchable-width #f)
                     ))


#|
UI: panel especifico para visualizar los equipos 
|#
 (define pnl_teams (new group-box-panel%
                             (parent left_panel_dist)
                             (label "Teams")
                             ))

#|
UI: listbox para visualizar voluntarios 
|#
(define list_box_teams (new list-box%
                      (label "")
                      (parent (new horizontal-panel%
                                   (parent pnl_teams)
                                   (style (list 'border))))
                      (choices (list ) )
                      (style (list 'single
                                   'column-headers))
                      (columns (list  "Name" "Place" "Description" "Languages"))))




(define rigth_panel_dist (new horizontal-panel%
                     (parent panel_visual_team)
                     (vert-margin 5)
                     ;(horiz-margin 5)
                     ;(min-width 450)
                     ;(stretchable-width #f)
                     ))




#|
UI: panel especifico para visualizar los miembros del equipos 
|#
 (define pnl_teams_members (new group-box-panel%
                             (parent rigth_panel_dist)
                             (label "Teams members")
                             ))

(define list_box_teams_members (new list-box%
                      (label "")
                      (parent (new horizontal-panel%
                                   (parent pnl_teams_members)
                                   (style (list 'border))))
                      (choices (list ) )
                      (style (list 'single
                                   'column-headers))
                      (columns (list  "Name" "Id" "Nationality" "Profession" "Languages"))))



(define panel_visual_voluuntrer (new horizontal-panel%
                     (parent frame_distirbucion)
                     (vert-margin 2)
                     ;(horiz-margin 5)
                     ;(min-width 450)
                     ;(stretchable-width #f)
                     ))

#|
UI: panel especifico para visualizar los voluntartios sin equipos
|#
 (define pnl_volunteers_without_team (new group-box-panel%
                             (parent panel_visual_voluuntrer)
                             (label "Volunteers without team")
                             ))





(define list_box_members_without_team (new list-box%
                      (label "")
                      (parent (new horizontal-panel%
                                   (parent pnl_volunteers_without_team)
                                   (style (list 'border))))
                      (choices (list ) )
                      (style (list 'single
                                   'column-headers))
                      (columns (list  "Name" "Id" "Nationality" "Profession" "Languages"))))

