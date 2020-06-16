
#lang racket/gui
(require racket/gui/base)
(require json)





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
                   [label "Distribución de voluntarios"]
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
                         (label "Languages:")
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
  (define list (build-list (send list_box_lenguages get-number) values))
  (for ([i list])
    (set! var_idiomas (string-append var_idiomas (send list_box_lenguages get-string i)))
    (set! var_idiomas (string-append var_idiomas " , "))
    )



  
  
  ;condiciones
  (define rest1 (not(equal? var_nombre "")))
  (define rest2 (not(equal? var_identificacionr "")))
  (define rest3 (not(equal? var_nacionalidad "")))
  (define rest4 (not(equal? var_profesion "")))
  (define rest5 (not(equal? var_idiomas "")))

  
  (define (add_volunteers_to_list_aux)  
    (send list_box_volunteers append "")
    (define index (- (send list_box_volunteers get-number) 1))
    (send list_box_volunteers set-string index var_nombre 0)
    (send list_box_volunteers set-string index var_identificacionr 1)
    (send list_box_volunteers set-string index var_nacionalidad 2)
    (send list_box_volunteers set-string index var_profesion 3)
    (send list_box_volunteers set-string index var_idiomas 4)
  )
  
  (if (and rest1 rest2 rest3 rest4 rest5)(add_volunteers_to_list_aux) (error "complete all fields")) 
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
                    (callback (lambda (b e)(load_from_json)))
                    ))

(define (load_from_json)

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
                         (label "Languages:  ")
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
  (define list (build-list (send list_box_lenguages_places get-number) values))
  (for ([i list])
    (set! var_idiomas (string-append var_idiomas " , "))
    (set! var_idiomas (string-append var_idiomas (send list_box_lenguages_places get-string i)))
    
    )
  (set! var_idiomas (substring var_idiomas 3))
  
  ;condiciones
  (define rest1 (not(equal? var_nombre_lugar "")))
  (define rest2 (not(equal? var_descripcion_lugar "")))
  (define rest3 (not(equal? var_idiomas "")))


  (define (add_place_to_list_aux)  
    (send list_box_places append "")
    (define index (- (send list_box_places get-number) 1))
    (send list_box_places set-string index var_nombre_lugar 0)
    (send list_box_places set-string index var_descripcion_lugar 1)
    (send list_box_places set-string index var_idiomas 2)
  )

  
  (if (and rest1 rest2 rest3)(add_place_to_list_aux) 
  (message-box "Error" 	
 	 	"complete all fields"	 
 	 	frame_principal	 
 	))	 

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
                    ))

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







