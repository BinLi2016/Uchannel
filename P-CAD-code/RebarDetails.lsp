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
    ; Note: Hooks not yet visualized: {'start': {'angle': '135', 'length': '16.5'}, 'end': {'angle': '135', 'length': '16.5'}}
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


  ; Tables - Generate data structures for table rendering
  ; --- Table: RebarDetailsTable ---
  ; Type: schedule, Columns: 4, Rows: 10
  (setq table_RebarDetailsTable_cols '("编号" "规格" "大样" "备注"))
  (setq table_RebarDetailsTable_data '(
    ("N1" "Φ16" "N1_N2" "Straight with hooks")  ; Row 1
    ("N2" "Φ16" "N1_N2" "Straight with hooks")  ; Row 2
    ("N3" "Φ16" "N3_N4" "U-bend")  ; Row 3
    ("N4" "Φ16" "N3_N4" "U-bend")  ; Row 4
    ("N5" "Φ20" "N5" "Large U-bend")  ; Row 5
    ("N6" "Φ16" "N6" "U-bend")  ; Row 6
    ("N7" "Φ16" "N7" "Stirrup 2d2-30 x L3-24")  ; Row 7
    ("N8" "Φ12" "N8" "Trapezoidal")  ; Row 8
    ("N9" "Φ12" "N9_N10" "Straight with hooks")  ; Row 9
    ("N10" "Φ12" "N9_N10" "Straight with hooks")  ; Row 10
  ))
  ; To draw: (draw-table-RebarDetailsTable base_x base_y cell_width cell_height)


  ; Sheets (layout setup)
  ; --- Sheet: RebarDetailsSheet ---
  (setq sheet_RebarDetailsSheet_width 420.0)
  (setq sheet_RebarDetailsSheet_height 297.0)
  (setq sheet_RebarDetailsSheet_scale 1.0)
  (setq sheet_RebarDetailsSheet_title "Details of Rebars")
  (setq sheet_RebarDetailsSheet_date "2026.01")
  ; Placements for RebarDetailsSheet:
  ;   - table: RebarDetailsTable


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
