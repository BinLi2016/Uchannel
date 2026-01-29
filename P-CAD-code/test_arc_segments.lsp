; =========================================
; AutoLISP generated from P-CAD
; Parameter Injection: Approach 4 (Global Variables)
; =========================================

; =========================================
; Function: Set Parameters
; Usage: (set-params W H R)
; =========================================
(defun set-params (W_val H_val R_val)
  (setq W W_val)
  (setq H H_val)
  (setq R R_val)
  (princ (strcat "\nParameters set: " "W=" (rtos W) ", " "H=" (rtos H) ", " "R=" (rtos R) "\n"))
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
  (if (not (boundp 'W)) (setq W 770.0))
  (if (not (boundp 'H)) (setq H 220.0))
  (if (not (boundp 'R)) (setq R 110.0))

  ; Calculate derived values

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

  ; --- Barshape: StadiumStirrup (type: stirrup) ---
  ; Dims: {'W': 'W', 'H': 'H', 'R': 'R'}
  (defun draw-barshape-StadiumStirrup (base_pt / pt x y sc prev_x prev_y r W H R)
    (setq sc 0.05)  ; Scale: 0.05 = fit ~1000mm shapes in ~50mm cells
    ; Initialize local dims
    (setq W W)
    (setq H H)
    (setq R R)
    (command "._-LAYER" "_S" "rebar" "")
    (command "._PLINE")
    (setq x (+ (car base_pt) (* sc 0)))
    (setq y (+ (cadr base_pt) (* sc 0)))
    ; Fillet corner: r=R
    (setq r (* sc R))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc W)))
    (setq y (+ (cadr base_pt) (* sc 0)))
    ; Fillet corner: r=R
    (setq r (* sc R))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc W)))
    (setq y (+ (cadr base_pt) (* sc H)))
    ; Fillet corner: r=R
    (setq r (* sc R))
    (command (list x y))
    (setq x (+ (car base_pt) (* sc 0)))
    (setq y (+ (cadr base_pt) (* sc H)))
    ; Fillet corner: r=R
    (setq r (* sc R))
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

  (setvar "CMDECHO" 1)
  (princ "\nP-CAD rendering complete.\n")
  (princ)
)

; =========================================
; Usage Examples:
; =========================================
;
; Method 1: Set parameters then render
;   (set-params 770 220 110)
;   (c:PCAD_Render)
;
; Method 2: Set parameters individually
;   (setq W 770.0)
;   (setq H 220.0)
;   (setq R 110.0)
;   (c:PCAD_Render)
;
; Method 3: Use default parameters
;   (c:PCAD_Render)  ; Uses default values if not set
