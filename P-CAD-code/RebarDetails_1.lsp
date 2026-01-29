; =========================================
; AutoLISP generated from P-CAD
; Parameter Injection: Approach 4 (Global Variables)
; =========================================

; =========================================
; Function: Set Parameters
; Usage: (set-params L)
; =========================================
(defun set-params (L_val)
  (setq L L_val)
  (princ (strcat "\nParameters set: " "L=" (rtos L) "\n"))
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
  (if (not (boundp 'L)) (setq L 500.0))

  ; Calculate derived values

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

  ; --- Barshape: N1 (type: straight) ---
  ; Dims: {}
  (defun draw-barshape-N1 (base_pt / pt x y sc)
    (setq sc 0.05)  ; Scale: 0.05 = fit ~1000mm shapes in ~50mm cells
    (command "._-LAYER" "_S" "rebar" "")
    (command "._PLINE")
    (setq x (+ (car base_pt) (* sc 0)))
    (setq y (+ (cadr base_pt) (* sc 0)))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc 500)))
    (setq y (+ (cadr base_pt) (* sc 0)))
    (command (list x y))
    (command "")
  )

  ; --- Barshape: N2 (type: custom) ---
  ; Dims: {}
  (defun draw-barshape-N2 (base_pt / pt x y sc)
    (setq sc 0.05)  ; Scale: 0.05 = fit ~1000mm shapes in ~50mm cells
    (command "._-LAYER" "_S" "rebar" "")
    (command "._PLINE")
    (setq x (+ (car base_pt) (* sc 0)))
    (setq y (+ (cadr base_pt) (* sc 100)))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc 0)))
    (setq y (+ (cadr base_pt) (* sc 0)))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc 200)))
    (setq y (+ (cadr base_pt) (* sc 0)))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc 200)))
    (setq y (+ (cadr base_pt) (* sc 100)))
    (command (list x y))
    (command "")
    ; Drawing hooks for N2
    ; End hook: angle=135, length=50
    (setq hook_angle (+ (angle (list 0 0) (list x y)) (* 135 (/ pi 180.0))))
    (setq hook_end_x (+ x (* sc 50 (cos hook_angle))))
    (setq hook_end_y (+ y (* sc 50 (sin hook_angle))))
    (command "._LINE" (list x y) (list hook_end_x hook_end_y) "")
    ; Start hook: angle=135, length=50
    (setq fx (+ (car base_pt) (* sc 0)))
    (setq fy (+ (cadr base_pt) (* sc 100)))
    (setq hook_angle (+ (angle (list 0 0) (list fx fy)) (* 135 (/ pi 180.0))))
    (setq hook_start_x (+ fx (* sc 50 (cos hook_angle))))
    (setq hook_start_y (+ fy (* sc 50 (sin hook_angle))))
    (command "._LINE" (list fx fy) (list hook_start_x hook_start_y) "")
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

  ; Barshape Layouts (freeform grid placement)
  ; --- Barshape Layout: TestLayout ---
  (command "._-LAYER" "_S" "text" "")
  (command "._MTEXT" (list 150.0 20.0 0.0) "_J" "_MC" "_H" 8 "_W" 300.0 "Rebar Details Test" "")
  (defun draw-layout-TestLayout (/ x y cell_w cell_h base_pt label_pt note_pt)
    (setq cell_w 150.0)
    (setq cell_h 150.0)

    ; Placement: N1 at (0, 0)
    (setq x (+ 0.0 (* 0 cell_w) (* 0.5 cell_w)))
    (setq y (- 0.0 (* 0 cell_h) (* 0.5 cell_h)))
    (setq base_pt (list x (- y 5) 0.0))
    ; Draw shape: N1
    (draw-barshape-N1 base_pt)
    ; Custom Annotations
    (command "._-LAYER" "_S" "text" "")
    (setq ann_pt (list (+ (car base_pt) -30.0) (+ (cadr base_pt) 20.0) 0.0))
    (command "._MTEXT" ann_pt "_J" "_MC" "_H" 3.5 "_W" 0 "N1" "")
    (command "._-LAYER" "_S" "text" "")
    (setq ann_pt (list (+ (car base_pt) 0.0) (+ (cadr base_pt) -20.0) 0.0))
    (command "._MTEXT" ann_pt "_R" 45.0 "_J" "_MC" "_H" 3.5 "_W" 0 "2d2-30" "")
    (command "._-LAYER" "_S" "text" "")
    (setq ann_pt (list (+ (car base_pt) 30.0) (+ (cadr base_pt) 20.0) 0.0))
    (command "._MTEXT" ann_pt "_J" "_MC" "_H" 3.5 "_W" 0 "2.6" "")

    ; Placement: N2 at (1, 0)
    (setq x (+ 0.0 (* 1 cell_w) (* 0.5 cell_w)))
    (setq y (- 0.0 (* 0 cell_h) (* 0.5 cell_h)))
    (setq base_pt (list x (- y 5) 0.0))
    ; Label: N2 Φ12
    (setq label_pt (list x (+ y 50) 0.0))
    (command "._-LAYER" "_S" "text" "")
    (command "._MTEXT" label_pt "_J" "_MC" "_H" 5 "_W" (- cell_w 10) "N2 Φ12" "")
    ; Draw shape: N2
    (draw-barshape-N2 base_pt)
    ; Note: Standard Note
    (setq note_pt (list x (- y 60) 0.0))
    (command "._-LAYER" "_S" "text" "")
    (command "._MTEXT" note_pt "_J" "_MC" "_H" 3.5 "_W" (- cell_w 10) "Standard Note" "")
    ; Custom Annotations
    (command "._-LAYER" "_S" "text" "")
    (setq ann_pt (list (+ (car base_pt) 50.0) (+ (cadr base_pt) 50.0) 0.0))
    (command "._MTEXT" ann_pt "_J" "_MC" "_H" 3.5 "_W" 0 "Extra Info" "")

  )
  (draw-layout-TestLayout)


  (setvar "CMDECHO" 1)
  (princ "\nP-CAD rendering complete.\n")
  (princ)
)

; =========================================
; Usage Examples:
; =========================================
;
; Method 1: Set parameters then render
;   (set-params 500)
;   (c:PCAD_Render)
;
; Method 2: Set parameters individually
;   (setq L 500.0)
;   (c:PCAD_Render)
;
; Method 3: Use default parameters
;   (c:PCAD_Render)  ; Uses default values if not set
