
#lang racket/gui
 (require racket/gui/base)

#|
E:
S:
R:
|#

(define frame_principal (new frame%
                   [label "Distribución de voluntarios"]
                   [width 1000]
                   [height 650]))
(send frame_principal show #t)


;Configuracion incial a dos columnas 

(define panel_row (new horizontal-pane%
                     (parent frame_principal)
                     ))


(define left_column (new vertical-panel%
                     (parent panel_row)
                     (vert-margin 5)
                     (horiz-margin 5)
                     (min-width 450)
                     (stretchable-width #f)
                     ))


(define right_column (new vertical-panel%
                     (parent panel_row)
                     (vert-margin 5)
                     (horiz-margin 5)
                     (min-width 550)
                     ))

;Columna izquierda
 (define pnl_agregar_voluntarios (new group-box-panel%
                             (parent left_column)
                             (label "Volunteers")
                             (spacing 5)
                             ))

 (define pnl_agregar_lugares (new group-box-panel%
                             (parent left_column)
                             (label "Places")
                             (spacing 5)
                             ))



;voluntarios
(define txt_nombre (new text-field%
                        (label "Name:         ")
                        (parent pnl_agregar_voluntarios)
                        (init-value "")
                        ))

(define txt_id (new text-field%
                        (label "Id:                ")
                        (parent pnl_agregar_voluntarios)
                        (init-value "")))

(define txt_nacionalidad (new combo-field%
                        (label "Nationality:")
                        (parent pnl_agregar_voluntarios)
                        (choices (list "Spanish" "English"))
                        (init-value "")))

(define txt_profesion (new combo-field%
                        (label "Profession: ")
                        (parent pnl_agregar_voluntarios)
                        (choices (list "Spanish" "English"))
                        (init-value "")))

(define panel_seleccion_lenguajes (new horizontal-panel%
                     (parent pnl_agregar_voluntarios)
                     ))

(define combo_languaje (new combo-field%
                         (label "Languages:")
                         (parent panel_seleccion_lenguajes)
                         
                         (choices (list "Spanish" "English"))
                         (init-value "Spanish")))

(define btn_add_lenguage (new button%
                    (parent panel_seleccion_lenguajes)
                    (label "Add")))



(define list_box_lenguages (new list-box%
                      (label "")
                      (parent (new horizontal-panel%
                                   (parent pnl_agregar_voluntarios)
                                   (style (list 'border))))
                      (choices (list "") )
                      (style (list 'single
                                   'column-headers))
                      (columns (list "Languages"))))

(define btn_add_volunteer (new button%
                    (parent pnl_agregar_voluntarios)
                    (label "Add volunteers")
                    (min-height 35)
                    (stretchable-height #f)
                    (stretchable-width #t)
                    ))

(define btn_load_volunteer  (new button%
                    (parent pnl_agregar_voluntarios)
                    (label "Load volunteers from JSON")
                    (min-height 35)
                    (stretchable-height #f)
                    (stretchable-width #t)
                    ))

;Lugares

(define txt_nombre_lugar (new text-field%
                        (label "Name:          ")
                        (parent pnl_agregar_lugares)
                        (init-value "")
                        ))

(define txt_descripcion_lugar (new text-field%
                        (label "Description: ")
                        (parent pnl_agregar_lugares)
                        (init-value "")))

(define combo_languaje_lugares (new combo-field%
                         (label "Languages:  ")
                         (parent pnl_agregar_lugares)
                         
                         (choices (list "Spanish" "English"))
                         (init-value "Spanish")))

(define btn_add_place (new button%
                    (parent pnl_agregar_lugares)
                    (label "Add place")
                    (min-height 35)
                    (stretchable-height #f)
                    (stretchable-width #t)
                    ))



;columna derecha 

 (define pnl_voluntarios_disponibles (new group-box-panel%
                             (parent right_column)
                             (label "Available volunteers")
                             ))

 (define pnl_voluntariados_disponibles (new group-box-panel%
                             (parent right_column)
                             (label "Available places")
                             ))

(define pnl_botones (new panel%
                             (parent right_column)
                             (min-height 50)
                             (stretchable-height #f)
                             ))


(define btn_distribuir_voluntarios (new button%
                    (parent pnl_botones)
                    (label "Distribute volunteers")
                    (min-height 50)
                    (stretchable-height #f)
                    (stretchable-width #t)
                    ))


(define list_box_volunteers (new list-box%
                      (label "")
                      (parent (new horizontal-panel%
                                   (parent pnl_voluntarios_disponibles)
                                   (style (list 'border))))
                      (choices (list ) )
                      (style (list 'single
                                   'column-headers))
                      (columns (list  "Name" "Id" "Nationality" "Profession" "Languages"))))


(send list_box_volunteers append "hola" 2)
;;(send list_box_volunteers set-string 1 "holaMundo" 2 )

(define list_box_places (new list-box%
                      (label "")
                      (parent (new horizontal-panel%
                                   (parent pnl_voluntariados_disponibles)
                                   (style (list 'border))))
                      (choices (list "Item 0"
                                     "Item 1"
                                     "Item 2"))
                      (style (list 'single
                                   'column-headers))
                      (columns (list  "Name" "Description" "Languages"))))
