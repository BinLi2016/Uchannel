; =========================================
; AutoLISP generated from P-CAD
; Parameter Injection: Approach 4 (Global Variables)
; =========================================

; =========================================
; Function: Set Parameters
; Usage: (set-params L_flange H W_slope L_bottom R1 R2)
; =========================================
(defun set-params (L_flange_val H_val W_slope_val L_bottom_val R1_val R2_val)
  (setq L_flange L_flange_val)
  (setq H H_val)
  (setq W_slope W_slope_val)
  (setq L_bottom L_bottom_val)
  (setq R1 R1_val)
  (setq R2 R2_val)
  (princ (strcat "\nParameters set: " "L_flange=" (rtos L_flange) ", " "H=" (rtos H) ", " "W_slope=" (rtos W_slope) ", " "L_bottom=" (rtos L_bottom) ", " "R1=" (rtos R1) ", " "R2=" (rtos R2) "\n"))
  (princ)
)

; =========================================
; Function: Render P-CAD Geometry
; Usage: (c:PCAD_Render)
; Note: Call (set-params ...) first to set parameters
; =========================================
(defun c:PCAD_Render ()
  (setvar "CMDECHO" 0)
  (setvar "DIMSCALE" 25.0)
  (setvar "DIMTXT" 40)
  (setvar "TEXTSIZE" 40)

  ; Check and set default parameters if not defined
  (if (not (boundp 'L_flange)) (setq L_flange 250.0))
  (if (not (boundp 'H)) (setq H 400.0))
  (if (not (boundp 'W_slope)) (setq W_slope 150.0))
  (if (not (boundp 'L_bottom)) (setq L_bottom 600.0))
  (if (not (boundp 'R1)) (setq R1 125.0))
  (if (not (boundp 'R2)) (setq R2 160.0))

  ; Calculate derived values
  (setq total_w (+ (+ L_bottom (* 2 W_slope)) (* 2 L_flange)))
  (setq x0 (- 0 (/ total_w 2.0)))
  (setq x1 (+ x0 L_flange))
  (setq x2 (+ x1 W_slope))
  (setq x3 (+ x2 L_bottom))
  (setq x4 (+ x3 W_slope))
  (setq x5 (+ x4 L_flange))

  ; Setup layers
  (command "._-LAYER" "_M" "outline" "_C" "4" "" "_LW" "0.25" "" "")
  (command "._-LAYER" "_M" "rebar" "_C" "1" "" "_LW" "0.2" "" "")
  (command "._-LAYER" "_M" "text" "_C" "3" "" "_LW" "0.18" "" "")
  (command "._-LAYER" "_M" "dim" "_C" "4" "" "_LW" "0.18" "" "")
  (command "._-LAYER" "_M" "hatch" "_C" "8" "" "_LW" "0.1" "" "")

  ; Setup text style: fsdb_e.shx,cadhzf.shx, width factor 0.7
  (command "._-STYLE" "Standard" "fsdb_e.shx,cadhzf.shx" 0 0.7 0 "_N" "_N" "")

  ; Bar shapes (rebar detail drawings)
  ; Each barshape generates a function: (draw-barshape-<Name> base_pt scale)
  ; base_pt is a list (x y z), scale is a number (1.0 = full size)
  ; For table cells, use scale 0.05-0.1 to fit shapes in cells

  ; --- Barshape: N_U_Bar (type: custom) ---
  ; Dims: {'Flange': 'L_flange', 'Height': 'H', 'SlopeW': 'W_slope', 'Bottom': 'L_bottom', 'R_Top': 'R1', 'R_Bottom': 'R2'}
  (defun draw-barshape-N_U_Bar (base_pt / pt x y sc prev_x prev_y r Flange Height SlopeW Bottom R_Top R_Bottom)
    (setq sc 0.05)  ; Scale: 0.05 = fit ~1000mm shapes in ~50mm cells
    ; Initialize local dims
    (setq Flange L_flange)
    (setq Height H)
    (setq SlopeW W_slope)
    (setq Bottom L_bottom)
    (setq R_Top R1)
    (setq R_Bottom R2)
    (command "._-LAYER" "_S" "rebar" "")
    (command "._PLINE")
    (setq x (+ (car base_pt) (* sc x0)))
    (setq y (+ (cadr base_pt) (* sc H)))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc x1)))
    (setq y (+ (cadr base_pt) (* sc H)))
    ; Fillet corner: r=R1
    (setq r (* sc R1))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc x2)))
    (setq y (+ (cadr base_pt) (* sc 0)))
    ; Fillet corner: r=R2
    (setq r (* sc R2))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc x3)))
    (setq y (+ (cadr base_pt) (* sc 0)))
    ; Fillet corner: r=R2
    (setq r (* sc R2))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc x4)))
    (setq y (+ (cadr base_pt) (* sc H)))
    ; Fillet corner: r=R1
    (setq r (* sc R1))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc x5)))
    (setq y (+ (cadr base_pt) (* sc H)))
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
        (command "._MTEXT" cp "_J" "_MC" "_H" 40 "_W" (* 0.9 w) txt "")
      )
    )
  )

  ; Sheets (layout setup)
  ; --- Sheet: U_Bar_Sheet ---
  (setq sheet_U_Bar_Sheet_width 420.0)
  (setq sheet_U_Bar_Sheet_height 297.0)
  (setq sheet_U_Bar_Sheet_scale 1.0)
  ; Render placements for U_Bar_Sheet


  ; Barshape Layouts (freeform grid placement)
  ; --- Barshape Layout: U_Bar_Detail ---
  (command "._-LAYER" "_S" "text" "")
  (command "._MTEXT" (list 600.0 20.0 0.0) "_J" "_MC" "_H" 16 "_W" 1200.0 "U-Bar Rebar Detail" "")
  (defun draw-layout-U_Bar_Detail (/ x y cell_w cell_h base_pt label_pt note_pt)
    (setq cell_w 1200.0)
    (setq cell_h 800.0)

    ; Placement: N_U_Bar at (0, 0)
    (setq x (+ 0.0 (* 0 cell_w) (* 0.5 cell_w)))
    (setq y (- 0.0 (* 0 cell_h) (* 0.5 cell_h)))
    (setq base_pt (list x (- y 5) 0.0))
    ; Label: N1-U-Bar
    (setq label_pt (list x (+ y 50) 0.0))
    (command "._-LAYER" "_S" "text" "")
    (command "._MTEXT" label_pt "_J" "_MC" "_H" 10 "_W" (- cell_w 10) "N1-U-Bar" "")
    ; Draw shape: N_U_Bar
    (draw-barshape-N_U_Bar base_pt)
    ; Note: Radii: R125, R160
    (setq note_pt (list x (- y 60) 0.0))
    (command "._-LAYER" "_S" "text" "")
    (command "._MTEXT" note_pt "_J" "_MC" "_H" 7 "_W" (- cell_w 10) "Radii: R125, R160" "")

  )
  (draw-layout-U_Bar_Detail)


  (setvar "CMDECHO" 1)
  (princ "\nP-CAD rendering complete.\n")
  (princ)
)

; =========================================
; Usage Examples:
; =========================================
;
; Method 1: Set parameters then render
;   (set-params 250 400 150 600 125 160)
;   (c:PCAD_Render)
;
; Method 2: Set parameters individually
;   (setq L_flange 250.0)
;   (setq H 400.0)
;   (setq W_slope 150.0)
;   (setq L_bottom 600.0)
;   (setq R1 125.0)
;   (setq R2 160.0)
;   (c:PCAD_Render)
;
; Method 3: Use default parameters
;   (c:PCAD_Render)  ; Uses default values if not set
