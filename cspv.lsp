;======================================================================
; ** Generatore di Profilo Longitudinale Professionale (PROFILO) **
;
; Scopo:    Crea un grafico di profilo longitudinale completo, analizzando una
;           polilinea. Include picchetti, progressive, parziali, pendenze,
;           ettometri, quote terreno e quota di riferimento.
; Autore:   Adattamento da script originale
; Versione: 5.6 - Aggiunte le unità di misura alle etichette delle righe
;                 per una maggiore chiarezza del grafico, come da richiesta.
;======================================================================

(vl-load-com)

;; --- Funzione di utilità per disegnare la colonna dati di un segmento di profilo ---
(defun draw_profile_column (p_start p_end p_start_elev p_end_elev dist_parziale prog_dist_val elev_start elev_end cfg picket_num)
  ;; Calcolo Pendenza
  (if (< (abs dist_parziale) 1e-6)
    (setq slope_str "---")
    (progn
      (setq slope_percent (* (/ (- elev_end elev_start) dist_parziale) 100.0))
      (setq slope_str (strcat (rtos slope_percent 2 2) "%"))
    )
  )

  ;; Disegno delle linee verticali della griglia e del profilo
  (command "._-color" (cadr (assoc 'cfg_color_grid cfg)))
  (command "._line" p_end p_end_elev "")
  (command "._line" p_start_elev p_end_elev "")
  (command "._-color" (cadr (assoc 'cfg_color_profile cfg)))
  (command "._line" p_start p_end "")

  ;; Disegno delle linee di chiusura orizzontali della colonna
  (setq i 1)
  (while (< i 7)
    (command "._-color" (cadr (assoc 'cfg_color_grid cfg)))
    (command "._line" (polar p_start (* 1.5 pi) (* (cadr (assoc 'cfg_row_spacing cfg)) (1+ i))) (polar p_end (* 1.5 pi) (* (cadr (assoc 'cfg_row_spacing cfg)) (1+ i))) "")
    (setq i (1+ i))
  )
  (command "._line" p_end (polar p_end (* 1.5 pi) (* (cadr (assoc 'cfg_row_spacing cfg)) 8)) "")

  ;; Posizionamento e disegno delle etichette
  (setq text_picket_pos (polar p_end (* 1.5 pi) (* (cadr (assoc 'cfg_row_spacing cfg)) 1.5))) ; Riga 2: Picchetto
  (setq text_prog_pos (polar p_end (* 1.5 pi) (* (cadr (assoc 'cfg_row_spacing cfg)) 2.5)))   ; Riga 3: Progressiva
  (setq text_elev_pos (polar p_end (* 1.5 pi) (* (cadr (assoc 'cfg_row_spacing cfg)) 6.5)))   ; Riga 7: Quota terreno

  (setq segment_mid_point (polar p_start 0 (/ (distance p_start p_end) 2.0)))
  (setq text_partial_pos (polar segment_mid_point (* 1.5 pi) (* (cadr (assoc 'cfg_row_spacing cfg)) 3.5))) ; Riga 4: Parziale
  (setq text_slope_pos (polar segment_mid_point (* 1.5 pi) (* (cadr (assoc 'cfg_row_spacing cfg)) 4.5)))   ; Riga 5: Pendenza

  (command "._-color" (cadr (assoc 'cfg_color_text cfg)))
  (command "._text" "_j" "_bc" text_picket_pos (cadr (assoc 'cfg_text_h_small cfg)) "90" (itoa picket_num))
  (command "._text" "_j" "_bc" text_prog_pos (cadr (assoc 'cfg_text_h_small cfg)) "90" (rtos prog_dist_val 2 2))
  (command "._text" "_j" "_bc" text_elev_pos (cadr (assoc 'cfg_text_h_small cfg)) "90" (rtos elev_end 2 2))
  (command "._text" "_j" "_mc" text_partial_pos (cadr (assoc 'cfg_text_h_large cfg)) "0" (rtos dist_parziale 2 2))
  (command "._text" "_j" "_mc" text_slope_pos (cadr (assoc 'cfg_text_h_large cfg)) "0" slope_str)
)

;; --- Funzione principale ---
(defun c:profilo ()
  (princ "\nGenerazione Profilo Longitudinale Professionale v5.6...")
  (setq old_error *error*) (setq *error* profilo_error_handler)
  (setvar "cmdecho" 0)
  (setq old_osmode (getvar "osmode")) (setvar "osmode" 0)

  ;; --- Parametri di Configurazione (modificabili) ---
  (setq config
    (list
      '(cfg_text_h_small 2.5)   ; Altezza testo per picchetti, progressive, quote
      '(cfg_text_h_large 3.0)   ; Altezza testo per parziali, pendenze, etichette
      '(cfg_row_spacing 12.0)   ; Spaziatura verticale tra le righe della griglia
      '(cfg_label_offset 45.0)  ; Distanza delle etichette dal profilo (aumentata per i testi più lunghi)
      '(cfg_color_text "7")     ; Colore per tutto il testo (7=Bianco/Nero)
      '(cfg_color_grid "8")     ; Colore per la griglia (grigio chiaro)
      '(cfg_color_profile "5")  ; Colore per la linea del profilo (Blu)
      '(cfg_color_datum "2")    ; Colore per la linea e simbolo datum (Giallo)
      '(cfg_color_last_row "1") ; Colore per l'ultima linea della griglia (Rosso)
    )
  )

  ;; --- 1. Selezione Input e Parametri ---
  (setq ent_sel (entsel "\nSelezionare la polilinea 2D del profilo del terreno (X=Distanza, Y=Quota): "))

  (if (and ent_sel (setq ent (car ent_sel)) (wcmatch (cdr (assoc 0 (entget ent))) "*POLYLINE"))
    (progn
      (setq slope_tolerance (getreal "\nSpecificare la tolleranza per la variazione di pendenza (%) [2.0]: "))
      (if (null slope_tolerance) (setq slope_tolerance 2.0)) (setq slope_tolerance (abs slope_tolerance))
      
      (setq min_dist_filter (getreal "\nSpecificare la distanza minima tra i picchetti [5.0]: "))
      (if (null min_dist_filter) (setq min_dist_filter 5.0)) (setq min_dist_filter (abs min_dist_filter))

      (setq p0 (getpoint "\nSpecificare il punto iniziale per il disegno del grafico: [0,0]"))
      (if (null p0) (setq p0 (list 0.0 0.0)))
      
      (setq v_scale_input (getreal "\nScala Verticale [1000]: "))
      (if (null v_scale_input) (setq scy 1000.0) (setq scy v_scale_input))
      (setq h_scale_input (getreal "\nScala Orizzontale [1000]: "))
      (if (null h_scale_input) (setq scx 1000.0) (setq scx h_scale_input))
      
      (setq scv (/ 1000 scy) sch (/ 1000 scx))
      (setq eldat (getreal "\nQuota di riferimento (Datum) [0]: "))
      (if (null eldat) (setq eldat 0.0))

      ;; --- 2. Elaborazione e Disegno Intestazione Tabella ---
      (setq start_point (vlax-curve-getStartPoint ent))
      (setq elt1 (cadr start_point))
      (setq p01 (polar p0 (* 0.5 pi) (* (- elt1 eldat) scv)))
      
      (command "._-color" (cadr (assoc 'cfg_color_grid config)))
      (command "._line" p0 p01 "")

      (setq p_header_start (polar p0 pi (cadr (assoc 'cfg_label_offset config))))
      
      ;; *** MODIFICA: Aggiunte unità di misura alle etichette ***
      (setq labels '("QUOTA RIFERIMENTO [m slm]" "PICCHETTI" "DIST. PROGRESSIVE [m]" "DIST. PARZIALI [m]" "PENDENZE [%]" "DIST. ETTOMETRICHE [100 m]" "QUOTE DEL TERRENO [m]"))
      
      (setq y_offset 0.5)
      (foreach label labels
        (setq current_label_pos (polar p_header_start (* 1.5 pi) (* y_offset (cadr (assoc 'cfg_row_spacing config)))))
        (command "._-color" (cadr (assoc 'cfg_color_text config)))
        (command "._text" "_j" "_mr" current_label_pos (cadr (assoc 'cfg_text_h_large config)) "0" label)
        (setq y_offset (+ y_offset 1.0))
      )
      
      (setq total_length (vlax-curve-getDistAtParam ent (vlax-curve-getEndParam ent)))
      (setq p_final (polar p0 0 (* total_length sch)))
      
      (setq i 0)
      (while (< i 8)
        (setq p_start_line (polar p_header_start (* 1.5 pi) (* i (cadr (assoc 'cfg_row_spacing config)))))
        (setq p_end_line (polar p_final (* 1.5 pi) (* i (cadr (assoc 'cfg_row_spacing config)))))
        (cond
          ((= i 0) (command "._-color" (cadr (assoc 'cfg_color_datum config))))
          ((= i 7) (command "._-color" (cadr (assoc 'cfg_color_last_row config))))
          (t (command "._-color" (cadr (assoc 'cfg_color_grid config))))
        )
        (command "._line" p_start_line p_end_line "")
        (setq i (1+ i))
      )
      
      ;; Etichette del primo picchetto
      (draw_profile_column p0 p0 p01 p01 0.0 0.0 elt1 elt1 config 1)
      
      ;; --- 3. Disegno del Simbolo Datum e del suo valore (UNA SOLA VOLTA) ---
      (setq sym_base p0)
      (command "._-color" (cadr (assoc 'cfg_color_datum config)))
      (command "._line" (polar sym_base pi 6.0) (polar sym_base 0 6.0) "")
      (command "._pline" sym_base (polar sym_base (+ (* 0.5 pi) 0.4) 5.0) (polar sym_base (+ (* 0.5 pi) -0.4) 5.0) "_c")
      (command "._-color" (cadr (assoc 'cfg_color_text config)))
      (command "._text" "_j" "_bc" (polar sym_base (* 0.5 pi) 7.0) (cadr (assoc 'cfg_text_h_large config)) "0" (rtos eldat 2 2))

      ;; --- 4. Analisi Polilinea e Disegno Sezioni ---
      (setq num_vertices (1+ (fix (vlax-curve-getEndParam ent))))
      (setq i 0) (setq picket_counter 1) (setq old_slope nil) (setq last_drawn_dist 0.0)
      (setq previous_point_plot p0) (setq previous_line_top p01)
      (setq previous_vertex_point start_point)

      (while (< i (1- num_vertices))
        (setq pt1 (vlax-curve-getPointAtParam ent i))
        (setq pt2 (vlax-curve-getPointAtParam ent (1+ i)))
        (setq dx (- (car pt2) (car pt1))) (setq dy (- (cadr pt2) (cadr pt1)))
        (if (< (abs dx) 1e-6) (setq current_slope nil) (setq current_slope (/ dy dx)))
        
        (setq slope_changed nil)
        (if (or (null old_slope) (null current_slope))
          (setq slope_changed T)
          (if (> (abs (* (- current_slope old_slope) 100.0)) slope_tolerance)
            (setq slope_changed T)
          )
        )
        
        (setq current_prog_dist (vlax-curve-getDistAtParam ent i))
        (setq dist_from_last_drawn (- current_prog_dist last_drawn_dist))
        
        (if (and slope_changed (> i 0) (> dist_from_last_drawn min_dist_filter))
          (progn
            (setq picket_counter (1+ picket_counter))
            (setq dist_parziale (- current_prog_dist last_drawn_dist))
            (setq elt (cadr pt1))
            (setq p2 (polar p0 0 (* current_prog_dist sch)))
            (setq p21 (polar p2 (* 0.5 pi) (* (- elt eldat) scv)))

            (draw_profile_column previous_point_plot p2 previous_line_top p21 dist_parziale current_prog_dist (cadr previous_vertex_point) elt config picket_counter)

            (setq previous_point_plot p2)
            (setq previous_line_top p21)
            (setq previous_vertex_point pt1)
            (setq last_drawn_dist current_prog_dist)
          )
        )
        (setq old_slope current_slope)
        (setq i (1+ i))
      )
      
      ;; --- 5. Disegno del punto finale (sempre) ---
      (if (> (- total_length last_drawn_dist) 1e-4)
        (progn
          (setq picket_counter (1+ picket_counter))
          (setq end_point (vlax-curve-getEndPoint ent))
          (setq dist_parziale (- total_length last_drawn_dist))
          (setq elt (cadr end_point))
          (setq p2 (polar p0 0 (* total_length sch))) 
          (setq p21 (polar p2 (* 0.5 pi) (* (- elt eldat) scv)))
          
          (draw_profile_column previous_point_plot p2 previous_line_top p21 dist_parziale total_length (cadr previous_vertex_point) elt config picket_counter)
          (setq last_drawn_dist total_length)
        )
      )

      ;; --- 6. Disegno Ettometri ---
      (setq hectometer_base_y (cadr (polar p0 (* 1.5 pi) (* (cadr (assoc 'cfg_row_spacing config)) 5))))
      (setq current_hectometer 100.0)
      (while (<= current_hectometer last_drawn_dist)
        (setq x_pos (+ (car p0) (* current_hectometer sch)))
        (setq p1_hect (list x_pos hectometer_base_y))
        (setq p2_hect (polar p1_hect (* 1.5 pi) (* (cadr (assoc 'cfg_row_spacing config)) 0.8)))
        (setq p_text_hect (polar p2_hect (* 1.5 pi) 3.0))
        (command "._-color" (cadr (assoc 'cfg_color_text config)))
        (command "._line" p1_hect p2_hect "")
        (command "._text" "_j" "_tc" p_text_hect (cadr (assoc 'cfg_text_h_large config)) "0" (rtos (/ current_hectometer 100.0) 2 0))
        (setq current_hectometer (+ current_hectometer 100.0))
      )
      (princ "\nProfilo disegnato con successo.")
    )
    (princ "\nNessuna polilinea valida selezionata. Annullato.")
  )

  (if old_osmode (setvar "osmode" old_osmode))
  (setvar "cmdecho" 1)
  (setq *error* old_error)
  (princ)
)

;; Gestore di errori
(defun profilo_error_handler (msg)
  (if old_osmode (setvar "osmode" old_osmode))
  (setvar "cmdecho" 1)
  (princ (strcat "\nErrore durante l'esecuzione di PROFILO: " msg))
  (setq *error* old_error)
  (princ)
)

(princ "\nScript 'profilo.lsp' (v5.6 - Unità di misura) caricato. Digitare PROFILO per avviare.")
(princ)
