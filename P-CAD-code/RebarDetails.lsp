; =========================================
; AutoLISP generated from P-CAD
; Parameter Injection: Approach 4 (Global Variables)
; =========================================

; =========================================
; Function: Set Parameters
; Usage: (set-params )
; =========================================
(defun set-params ()
  (princ (strcat "\nParameters set: "  "\n"))
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

  ; Calculate derived values

  ; Setup layers
  (command "._LAYER" "_M" "outline" "_C" "4" "" "_LW" "0.25" "" "")
  (command "._LAYER" "_M" "rebar" "_C" "1" "" "_LW" "0.2" "" "")
  (command "._LAYER" "_M" "text" "_C" "3" "" "_LW" "0.18" "" "")

  ; Bar shapes (rebar detail drawings)
  ; Each barshape generates a function: (draw-barshape-<Name> base_pt)
  ; base_pt is a list (x y z), parameters are taken from global variables

  ; --- Barshape: N1_N2 (type: custom) ---
  ; Dims: {'L': 'L'}
  (defun draw-barshape-N1_N2 (base_pt / pt x y)
    (command "._LAYER" "_S" "rebar" "")
    (command "._PLINE")
    (setq x (+ (car base_pt) 0))
    (setq y (+ (cadr base_pt) 21.6))
    (command (list x y))
    (setq x (+ (car base_pt) 0))
    (setq y (+ (cadr base_pt) 0))
    (command (list x y))
    (setq x (+ (car base_pt) L))
    (setq y (+ (cadr base_pt) 0))
    (command (list x y))
    (setq x (+ (car base_pt) L))
    (setq y (+ (cadr base_pt) 21.6))
    (command (list x y))
    (command "")
  )

  ; --- Barshape: N3_N4 (type: custom) ---
  ; Dims: {'w': 'w'}
  (defun draw-barshape-N3_N4 (base_pt / pt x y)
    (command "._LAYER" "_S" "rebar" "")
    (command "._PLINE")
    (setq x (+ (car base_pt) -21.6))
    (setq y (+ (cadr base_pt) 48))
    (command (list x y))
    (setq x (+ (car base_pt) 0))
    (setq y (+ (cadr base_pt) 48))
    (command (list x y))
    (setq x (+ (car base_pt) 0))
    (setq y (+ (cadr base_pt) 0))
    (command (list x y))
    (setq x (+ (car base_pt) w))
    (setq y (+ (cadr base_pt) 0))
    (command (list x y))
    (setq x (+ (car base_pt) w))
    (setq y (+ (cadr base_pt) 48))
    (command (list x y))
    (setq x (+ (car base_pt) (+ w 21.6)))
    (setq y (+ (cadr base_pt) 48))
    (command (list x y))
    (command "")
  )

  ; --- Barshape: N5 (type: custom) ---
  ; Dims: {'w': 'w'}
  (defun draw-barshape-N5 (base_pt / pt x y)
    (command "._LAYER" "_S" "rebar" "")
    (command "._PLINE")
    (setq x (+ (car base_pt) -27))
    (setq y (+ (cadr base_pt) 98))
    (command (list x y))
    (setq x (+ (car base_pt) 0))
    (setq y (+ (cadr base_pt) 98))
    (command (list x y))
    (setq x (+ (car base_pt) 0))
    (setq y (+ (cadr base_pt) 0))
    (command (list x y))
    (setq x (+ (car base_pt) w))
    (setq y (+ (cadr base_pt) 0))
    (command (list x y))
    (setq x (+ (car base_pt) w))
    (setq y (+ (cadr base_pt) 98))
    (command (list x y))
    (setq x (+ (car base_pt) (+ w 27)))
    (setq y (+ (cadr base_pt) 98))
    (command (list x y))
    (command "")
  )

  ; --- Barshape: N6 (type: custom) ---
  ; Dims: {'w': 'w'}
  (defun draw-barshape-N6 (base_pt / pt x y)
    (command "._LAYER" "_S" "rebar" "")
    (command "._PLINE")
    (setq x (+ (car base_pt) -21.6))
    (setq y (+ (cadr base_pt) 98))
    (command (list x y))
    (setq x (+ (car base_pt) 0))
    (setq y (+ (cadr base_pt) 98))
    (command (list x y))
    (setq x (+ (car base_pt) 0))
    (setq y (+ (cadr base_pt) 0))
    (command (list x y))
    (setq x (+ (car base_pt) w))
    (setq y (+ (cadr base_pt) 0))
    (command (list x y))
    (setq x (+ (car base_pt) w))
    (setq y (+ (cadr base_pt) 98))
    (command (list x y))
    (setq x (+ (car base_pt) (+ w 21.6)))
    (setq y (+ (cadr base_pt) 98))
    (command (list x y))
    (command "")
  )

  ; --- Barshape: N7 (type: stirrup) ---
  ; Dims: {'a': '2*d2-30', 'b': 'L3-24'}
  (defun draw-barshape-N7 (base_pt / pt x y)
    (command "._LAYER" "_S" "rebar" "")
    (command "._PLINE")
    (setq x (+ (car base_pt) 0))
    (setq y (+ (cadr base_pt) 0))
    (command (list x y))
    (setq x (+ (car base_pt) a))
    (setq y (+ (cadr base_pt) 0))
    (command (list x y))
    (setq x (+ (car base_pt) a))
    (setq y (+ (cadr base_pt) b))
    (command (list x y))
    (setq x (+ (car base_pt) 0))
    (setq y (+ (cadr base_pt) b))
    (command (list x y))
    (setq x (+ (car base_pt) 0))
    (setq y (+ (cadr base_pt) 0))
    (command (list x y))
    (command "")
    ; Drawing hooks for N7
    ; End hook: angle=135, length=16.5
    (setq hook_angle (+ (angle (list 0 0) (list x y)) (* 135 (/ pi 180.0))))
    (setq hook_end_x (+ x (* 16.5 (cos hook_angle))))
    (setq hook_end_y (+ y (* 16.5 (sin hook_angle))))
    (command "._LINE" (list x y) (list hook_end_x hook_end_y) "")
    ; Start hook: angle=135, length=16.5
    (setq fx (+ (car base_pt) 0))
    (setq fy (+ (cadr base_pt) 0))
    (setq hook_angle (+ (angle (list 0 0) (list fx fy)) (* 135 (/ pi 180.0))))
    (setq hook_start_x (+ fx (* 16.5 (cos hook_angle))))
    (setq hook_start_y (+ fy (* 16.5 (sin hook_angle))))
    (command "._LINE" (list fx fy) (list hook_start_x hook_start_y) "")
  )

  ; --- Barshape: N8 (type: custom) ---
  ; Dims: {'top_w': '2B2+216', 'side_l': 'L4+40', 'h': 'side_l * cos(10)', 'side_x': 'side_l * sin(10)'}
  (defun draw-barshape-N8 (base_pt / pt x y)
    (command "._LAYER" "_S" "rebar" "")
    (command "._PLINE")
    (setq x (+ (car base_pt) -16.2))
    (setq y (+ (cadr base_pt) h))
    (command (list x y))
    (setq x (+ (car base_pt) 0))
    (setq y (+ (cadr base_pt) h))
    (command (list x y))
    (setq x (+ (car base_pt) side_x))
    (setq y (+ (cadr base_pt) 0))
    (command (list x y))
    (setq x (+ (car base_pt) (+ side_x top_w)))
    (setq y (+ (cadr base_pt) 0))
    (command (list x y))
    (setq x (+ (car base_pt) (+ (* 2 side_x) top_w)))
    (setq y (+ (cadr base_pt) h))
    (command (list x y))
    (setq x (+ (car base_pt) (+ (+ (* 2 side_x) top_w) 16.2)))
    (setq y (+ (cadr base_pt) h))
    (command (list x y))
    (command "")
  )

  ; --- Barshape: N9_N10 (type: custom) ---
  ; Dims: {'L': 'L'}
  (defun draw-barshape-N9_N10 (base_pt / pt x y)
    (command "._LAYER" "_S" "rebar" "")
    (command "._PLINE")
    (setq x (+ (car base_pt) 0))
    (setq y (+ (cadr base_pt) 16.2))
    (command (list x y))
    (setq x (+ (car base_pt) 0))
    (setq y (+ (cadr base_pt) 0))
    (command (list x y))
    (setq x (+ (car base_pt) L))
    (setq y (+ (cadr base_pt) 0))
    (command (list x y))
    (setq x (+ (car base_pt) L))
    (setq y (+ (cadr base_pt) 16.2))
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
    (command "._LAYER" "_S" "outline" "")
    (command "._PLINE" p1 p2 p3 p4 "_C")
    (if (and txt (/= txt ""))
      (progn
        (command "._LAYER" "_S" "text" "")
        (command "._MTEXT" cp "_J" "_MC" "_W" (* 0.9 w) txt "")
      )
    )
  )

  ; Tables - Generate data structures and drawing functions
  ; --- Table: RebarDetailsTable ---
  (setq table_RebarDetailsTable_cols '("编号" "规格" "大样" "备注"))
  (setq table_RebarDetailsTable_data '(
    ("N1" "Φ16" "N1_N2" "Straight with hooks")
    ("N2" "Φ16" "N1_N2" "Straight with hooks")
    ("N3" "Φ16" "N3_N4" "U-bend")
    ("N4" "Φ16" "N3_N4" "U-bend")
    ("N5" "Φ20" "N5" "Large U-bend")
    ("N6" "Φ16" "N6" "U-bend")
    ("N7" "Φ16" "N7" "Stirrup 2d2-30 x L3-24")
    ("N8" "Φ12" "N8" "Trapezoidal")
    ("N9" "Φ12" "N9_N10" "Straight with hooks")
    ("N10" "Φ12" "N9_N10" "Straight with hooks")
  ))
  (defun draw-table-RebarDetailsTable (ins_pt cell_w cell_h / x y col_idx row_data val cell_pt shape_func shape_pt)
    (setq x (car ins_pt) y (cadr ins_pt))
    (setq col_idx 0)
    (foreach col_name table_RebarDetailsTable_cols
      (setq cell_pt (list (+ x (* col_idx cell_w)) y 0.0))
      (draw-table-cell cell_pt cell_w cell_h col_name)
      (setq col_idx (1+ col_idx))
    )
    (setq y (- y cell_h))
    (foreach row_data table_RebarDetailsTable_data
      (setq col_idx 0)
      (foreach val row_data
        (setq cell_pt (list (+ x (* col_idx cell_w)) y 0.0))
        (if (member col_idx '(2))
          (progn
            (draw-table-cell cell_pt cell_w cell_h "")
            (setq shape_func (read (strcat "draw-barshape-" val)))
            (if (and val (/= val "") (fboundp shape_func))
              (progn
                (setq shape_pt (list (+ (car cell_pt) (* 0.5 cell_w)) (- (cadr cell_pt) (* 0.5 cell_h)) 0.0))
                (apply shape_func (list shape_pt))
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
  ; --- Sheet: RebarDetailsSheet ---
  (setq sheet_RebarDetailsSheet_width 420.0)
  (setq sheet_RebarDetailsSheet_height 297.0)
  (setq sheet_RebarDetailsSheet_scale 1.0)
  (setq sheet_RebarDetailsSheet_title "Details of Rebars")
  (setq sheet_RebarDetailsSheet_date "2026.01")
  ; Render placements for RebarDetailsSheet
  (draw-table-RebarDetailsTable (list 0.0 0.0 0.0) 40.0 10.0)


  (setvar "CMDECHO" 1)
  (princ "\nP-CAD rendering complete.\n")
  (princ)
)

; =========================================
; Usage Examples:
; =========================================
;
; Method 1: Set parameters then render
;   (set-params )
;   (c:PCAD_Render)
;
; Method 2: Set parameters individually
;   (c:PCAD_Render)
;
; Method 3: Use default parameters
;   (c:PCAD_Render)  ; Uses default values if not set
