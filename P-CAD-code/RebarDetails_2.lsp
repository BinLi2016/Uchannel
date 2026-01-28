; =========================================
; AutoLISP generated from P-CAD
; Parameter Injection: Approach 4 (Global Variables)
; =========================================

; =========================================
; Function: Set Parameters
; Usage: (set-params H1 H3 B1 B2 L2 L3 L4)
; =========================================
(defun set-params (H1_val H3_val B1_val B2_val L2_val L3_val L4_val)
  (setq H1 H1_val)
  (setq H3 H3_val)
  (setq B1 B1_val)
  (setq B2 B2_val)
  (setq L2 L2_val)
  (setq L3 L3_val)
  (setq L4 L4_val)
  (princ (strcat "\nParameters set: " "H1=" (rtos H1) ", " "H3=" (rtos H3) ", " "B1=" (rtos B1) ", " "B2=" (rtos B2) ", " "L2=" (rtos L2) ", " "L3=" (rtos L3) ", " "L4=" (rtos L4) "\n"))
  (princ)
)

; =========================================
; Function: Render P-CAD Geometry
; Usage: (c:PCAD_Render)
; Note: Call (set-params ...) first to set parameters
; =========================================
(defun c:PCAD_Render ()
  (setvar "CMDECHO" 0)

  ; Check and set default parameters if not defined
  (if (not (boundp 'H1)) (setq H1 1500.0))
  (if (not (boundp 'H3)) (setq H3 1000.0))
  (if (not (boundp 'B1)) (setq B1 500.0))
  (if (not (boundp 'B2)) (setq B2 800.0))
  (if (not (boundp 'L2)) (setq L2 600.0))
  (if (not (boundp 'L3)) (setq L3 400.0))
  (if (not (boundp 'L4)) (setq L4 300.0))

  ; Calculate derived values
  (setq N1_H (+ H3 50))
  (setq N3_H (- B2 12))
  (setq N6_H (+ H1 30))
  (setq N4_W (- L2 12))
  (setq N7_H (- (+ L4 L3) 20))
  (setq N7_W (- L2 12))
  (setq N7_1_W (- B2 12))

  ; Setup layers
  (command "._-LAYER" "_M" "outline" "_C" "3" "" "_LW" "0.25" "" "")
  (command "._-LAYER" "_M" "rebar" "_C" "1" "" "_LW" "0.5" "" "")
  (command "._-LAYER" "_M" "text" "_C" "7" "" "_LW" "0.15" "" "")
  (command "._-LAYER" "_M" "dim" "_C" "4" "" "_LW" "0.18" "" "")
  (command "._-LAYER" "_M" "hatch" "_C" "8" "" "_LW" "0.1" "" "")

  ; Bar shapes (rebar detail drawings)
  ; Each barshape generates a function: (draw-barshape-<Name> base_pt scale)
  ; base_pt is a list (x y z), scale is a number (1.0 = full size)
  ; For table cells, use scale 0.05-0.1 to fit shapes in cells

  ; --- Barshape: N1 (type: custom) ---
  ; Dims: {'H': 'N1_H'}
  (defun draw-barshape-N1 (base_pt / pt x y sc)
    (setq sc 0.05)  ; Scale: 0.05 = fit ~1000mm shapes in ~50mm cells
    (command "._-LAYER" "_S" "rebar" "")
    (command "._PLINE")
    (setq x (+ (car base_pt) (* sc 21.6)))
    (setq y (+ (cadr base_pt) (* sc (+ N1_H 45.3))))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc 0)))
    (setq y (+ (cadr base_pt) (* sc (+ N1_H 45.3))))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc 0)))
    (setq y (+ (cadr base_pt) (* sc 45.3)))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc 45.3)))
    (setq y (+ (cadr base_pt) (* sc 0)))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc 75.3)))
    (setq y (+ (cadr base_pt) (* sc 0)))
    (command (list x y))
    (command "")
  )

  ; --- Barshape: N2 (type: custom) ---
  ; Dims: {'H': 'N1_H'}
  (defun draw-barshape-N2 (base_pt / pt x y sc)
    (setq sc 0.05)  ; Scale: 0.05 = fit ~1000mm shapes in ~50mm cells
    (command "._-LAYER" "_S" "rebar" "")
    (command "._PLINE")
    (setq x (+ (car base_pt) (* sc 21.6)))
    (setq y (+ (cadr base_pt) (* sc (+ N1_H 45.3))))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc 0)))
    (setq y (+ (cadr base_pt) (* sc (+ N1_H 45.3))))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc 0)))
    (setq y (+ (cadr base_pt) (* sc 45.3)))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc 45.3)))
    (setq y (+ (cadr base_pt) (* sc 0)))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc 75.3)))
    (setq y (+ (cadr base_pt) (* sc 0)))
    (command (list x y))
    (command "")
  )

  ; --- Barshape: N3 (type: custom) ---
  ; Dims: {'H': 'N3_H'}
  (defun draw-barshape-N3 (base_pt / pt x y sc)
    (setq sc 0.05)  ; Scale: 0.05 = fit ~1000mm shapes in ~50mm cells
    (command "._-LAYER" "_S" "rebar" "")
    (command "._PLINE")
    (setq x (+ (car base_pt) (* sc 16.2)))
    (setq y (+ (cadr base_pt) (* sc N3_H)))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc 0)))
    (setq y (+ (cadr base_pt) (* sc N3_H)))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc 0)))
    (setq y (+ (cadr base_pt) (* sc 0)))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc 16.2)))
    (setq y (+ (cadr base_pt) (* sc 0)))
    (command (list x y))
    (command "")
  )

  ; --- Barshape: N6 (type: custom) ---
  ; Dims: {'H': 'N6_H'}
  (defun draw-barshape-N6 (base_pt / pt x y sc)
    (setq sc 0.05)  ; Scale: 0.05 = fit ~1000mm shapes in ~50mm cells
    (command "._-LAYER" "_S" "rebar" "")
    (command "._PLINE")
    (setq x (+ (car base_pt) (* sc 21.6)))
    (setq y (+ (cadr base_pt) (* sc (+ N6_H 45.3))))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc 0)))
    (setq y (+ (cadr base_pt) (* sc (+ N6_H 45.3))))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc 0)))
    (setq y (+ (cadr base_pt) (* sc 45.3)))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc 45.3)))
    (setq y (+ (cadr base_pt) (* sc 0)))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc 75.3)))
    (setq y (+ (cadr base_pt) (* sc 0)))
    (command (list x y))
    (command "")
  )

  ; --- Barshape: N5 (type: custom) ---
  ; Dims: {'H': 'N6_H'}
  (defun draw-barshape-N5 (base_pt / pt x y sc)
    (setq sc 0.05)  ; Scale: 0.05 = fit ~1000mm shapes in ~50mm cells
    (command "._-LAYER" "_S" "rebar" "")
    (command "._PLINE")
    (setq x (+ (car base_pt) (* sc 21.6)))
    (setq y (+ (cadr base_pt) (* sc (+ N6_H 45.3))))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc 0)))
    (setq y (+ (cadr base_pt) (* sc (+ N6_H 45.3))))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc 0)))
    (setq y (+ (cadr base_pt) (* sc 45.3)))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc 45.3)))
    (setq y (+ (cadr base_pt) (* sc 0)))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc 75.3)))
    (setq y (+ (cadr base_pt) (* sc 0)))
    (command (list x y))
    (command "")
  )

  ; --- Barshape: N8 (type: custom) ---
  ; Dims: {}
  (defun draw-barshape-N8 (base_pt / pt x y sc)
    (setq sc 0.05)  ; Scale: 0.05 = fit ~1000mm shapes in ~50mm cells
    (command "._-LAYER" "_S" "rebar" "")
    (command "._PLINE")
    (setq x (+ (car base_pt) (* sc 16.2)))
    (setq y (+ (cadr base_pt) (* sc 122)))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc 0)))
    (setq y (+ (cadr base_pt) (* sc 122)))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc 0)))
    (setq y (+ (cadr base_pt) (* sc 0)))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc 16.2)))
    (setq y (+ (cadr base_pt) (* sc 0)))
    (command (list x y))
    (command "")
  )

  ; --- Barshape: N4 (type: custom) ---
  ; Dims: {'W': 'N4_W', 'H': 'B1'}
  (defun draw-barshape-N4 (base_pt / pt x y sc)
    (setq sc 0.05)  ; Scale: 0.05 = fit ~1000mm shapes in ~50mm cells
    (command "._-LAYER" "_S" "rebar" "")
    (command "._PLINE")
    (setq x (+ (car base_pt) (* sc 0)))
    (setq y (+ (cadr base_pt) (* sc B1)))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc N4_W)))
    (setq y (+ (cadr base_pt) (* sc (- B1 12))))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc N4_W)))
    (setq y (+ (cadr base_pt) (* sc 0)))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc 0)))
    (setq y (+ (cadr base_pt) (* sc 0)))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc 0)))
    (setq y (+ (cadr base_pt) (* sc B1)))
    (command (list x y))
    (command "")
  )

  ; --- Barshape: N7 (type: custom) ---
  ; Dims: {'W': 'N7_W', 'H': 'N7_H'}
  (defun draw-barshape-N7 (base_pt / pt x y sc)
    (setq sc 0.05)  ; Scale: 0.05 = fit ~1000mm shapes in ~50mm cells
    (command "._-LAYER" "_S" "rebar" "")
    (command "._PLINE")
    (setq x (+ (car base_pt) (* sc (* N7_H 0.176))))
    (setq y (+ (cadr base_pt) (* sc N7_H)))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc (+ (* N7_H 0.176) N7_W))))
    (setq y (+ (cadr base_pt) (* sc N7_H)))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc N7_W)))
    (setq y (+ (cadr base_pt) (* sc 0)))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc 0)))
    (setq y (+ (cadr base_pt) (* sc 0)))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc (* N7_H 0.176))))
    (setq y (+ (cadr base_pt) (* sc N7_H)))
    (command (list x y))
    (command "")
  )

  ; --- Barshape: N7_1 (type: custom) ---
  ; Dims: {'W': 'N7_1_W', 'H': 'N7_H'}
  (defun draw-barshape-N7_1 (base_pt / pt x y sc)
    (setq sc 0.05)  ; Scale: 0.05 = fit ~1000mm shapes in ~50mm cells
    (command "._-LAYER" "_S" "rebar" "")
    (command "._PLINE")
    (setq x (+ (car base_pt) (* sc (* N7_H 0.176))))
    (setq y (+ (cadr base_pt) (* sc N7_H)))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc (+ (* N7_H 0.176) N7_1_W))))
    (setq y (+ (cadr base_pt) (* sc N7_H)))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc N7_1_W)))
    (setq y (+ (cadr base_pt) (* sc 0)))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc 0)))
    (setq y (+ (cadr base_pt) (* sc 0)))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc (* N7_H 0.176))))
    (setq y (+ (cadr base_pt) (* sc N7_H)))
    (command (list x y))
    (command "")
  )


  ; --- Utility Functions for Rendering ---
  (defun draw-table-cell (pt w h txt / p1 p2 p3 p4 cp)
    (setq p1 pt
          p2 (list (+ (car pt) w) (cadr pt) 0.0)
          p3 (list (+ (car pt) w) (- (cadr pt) h) 0.0)
          p4 (list (car pt) (- (cadr pt) h) 0.0)
          cp (list (+ (car pt) (* 0.5 w)) (- (cadr pt) (* 0.5 h)) 0.0)
    )
    (command "._-LAYER" "_S" "outline" "")
    (command "._PLINE" p1 p2 p3 p4 "_C")
    (if (and txt (/= txt ""))
      (progn
        (command "._-LAYER" "_S" "text" "")
        (command "._MTEXT" cp "_J" "_MC" "_W" (* 0.9 w) txt "")
      )
    )
  )

  ; Tables - Generate data structures and drawing functions
  ; --- Table: RebarDetailsTable2 ---
  (setq table_RebarDetailsTable2_cols '("编号" "规格" "大样" "备注"))
  (setq table_RebarDetailsTable2_data '(
    ("N1" "Φ16" "N1" "H3+50")
    ("N2" "Φ16" "N2" "H3+50")
    ("N3" "Φ12" "N3" "28~(B2-12)")
    ("N6" "Φ16" "N6" "(H3+50)~(H1+30)")
    ("N5" "Φ16" "N5" "(H3+50)~(H1+30)")
    ("N8" "Φ12" "N8" "122")
    ("N4" "Φ12" "N4" "Trapezoid")
    ("N7" "Φ12" "N7" "Slanted")
    ("N7_1" "Φ12" "N7_1" "Slanted")
  ))
  (defun draw-table-RebarDetailsTable2 (ins_pt cell_w cell_h / x y col_idx row_data val cell_pt shape_func shape_pt)
    (setq x (car ins_pt) y (cadr ins_pt))
    (setq col_idx 0)
    (foreach col_name table_RebarDetailsTable2_cols
      (setq cell_pt (list (+ x (* col_idx cell_w)) y 0.0))
      (draw-table-cell cell_pt cell_w cell_h col_name)
      (setq col_idx (1+ col_idx))
    )
    (setq y (- y cell_h))
    (foreach row_data table_RebarDetailsTable2_data
      (setq col_idx 0)
      (foreach val row_data
        (setq cell_pt (list (+ x (* col_idx cell_w)) y 0.0))
        (if (member col_idx '(2))
          (progn
            (draw-table-cell cell_pt cell_w cell_h "")
            (setq shape_func (read (strcat "draw-barshape-" val)))
            (if (and val (/= val ""))
              (progn
                (setq shape_pt (list (+ (car cell_pt) (* 0.5 cell_w)) (- (cadr cell_pt) (* 0.5 cell_h)) 0.0))
                (eval (list shape_func (list 'quote shape_pt)))
              )
            )
          )
          (draw-table-cell cell_pt cell_w cell_h val)
        )
        (setq col_idx (1+ col_idx))
      )
      (setq y (- y cell_h))
    )
  )


  ; Sheets (layout setup)
  ; --- Sheet: RebarDetailsSheet2 ---
  (setq sheet_RebarDetailsSheet2_width 420.0)
  (setq sheet_RebarDetailsSheet2_height 297.0)
  (setq sheet_RebarDetailsSheet2_scale 1.0)
  (setq sheet_RebarDetailsSheet2_title "Details of rebars - Set 2")
  (setq sheet_RebarDetailsSheet2_date "2026.01.28")
  ; Render placements for RebarDetailsSheet2
  (draw-table-RebarDetailsTable2 (list 0.0 0.0 0.0) 80.0 60.0)


  (setvar "CMDECHO" 1)
  (princ "\nP-CAD rendering complete.\n")
  (princ)
)

; =========================================
; Usage Examples:
; =========================================
;
; Method 1: Set parameters then render
;   (set-params 1500 1000 500 800 600 400 300)
;   (c:PCAD_Render)
;
; Method 2: Set parameters individually
;   (setq H1 1500.0)
;   (setq H3 1000.0)
;   (setq B1 500.0)
;   (setq B2 800.0)
;   (setq L2 600.0)
;   (setq L3 400.0)
;   (setq L4 300.0)
;   (c:PCAD_Render)
;
; Method 3: Use default parameters
;   (c:PCAD_Render)  ; Uses default values if not set
