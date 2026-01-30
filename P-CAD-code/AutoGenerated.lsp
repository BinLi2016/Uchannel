; =========================================
; AutoLISP generated from P-CAD
; Parameter Injection: Approach 4 (Global Variables)
; =========================================

; =========================================
; Function: Set Parameters
; Usage: (set-params H B a1 C R)
; =========================================
(defun set-params (H_val B_val a1_val C_val R_val)
  (setq H H_val)
  (setq B B_val)
  (setq a1 a1_val)
  (setq C C_val)
  (setq R R_val)
  (princ (strcat "\nParameters set: " "H=" (rtos H) ", " "B=" (rtos B) ", " "a1=" (rtos a1) ", " "C=" (rtos C) ", " "R=" (rtos R) "\n"))
  (princ)
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
        (command "._MTEXT" cp "_J" "_MC" "_H" (* (getvar "DIMSCALE") 10) "_W" (* 0.9 w) txt "")
      )
    )
  )

  (defun pcad-smart-dim-arc (ent / obj midPt)
    (vl-load-com)
    (if (and ent (wcmatch (cdr (assoc 0 (entget ent))) "ARC,CIRCLE"))
      (progn
        (command "._-LAYER" "_S" "dim" "")
        (setq obj (vlax-ename->vla-object ent))
        (setq midPt (vlax-curve-getPointAtParam obj 
                      (/ (+ (vlax-curve-getStartParam obj) 
                            (vlax-curve-getEndParam obj)) 
                         2.0)
                    )
        )
        (command "._DIMRADIUS" (list ent midPt) midPt)
      )
    )
  )

  ; pcad-draw-pline-with-fillets: Draws rebar shape with fillets using FILLET command
  ; pts: list of (x y) points
  ; radii: list of radii (same length as pts, use 0 for no fillet)
  ; closed: T or nil
  ; Approach: Draw LINE segments, store entities, then FILLET using entity-point pairs
  (defun pcad-draw-pline-with-fillets (pts radii closed / i n r p0 p1 p2 lines ent1 ent2 sel1 sel2)
    (command "._-LAYER" "_S" "rebar" "")
    (setq n (length pts))
    (setq lines '())
    
    ; Step 1: Draw all LINE segments and store entity names
    (setq i 0)
    (while (< i (1- n))
      (setq p0 (nth i pts))
      (setq p1 (nth (1+ i) pts))
      (command "._LINE" p0 p1 "")
      (setq lines (append lines (list (entlast))))
      (setq i (1+ i))
    )
    
    ; Step 2: Apply FILLET at each corner with non-zero radius
    (setq i 1)
    (while (< i (1- n))
      (setq r (nth i radii))
      (if (and r (> r 0.0))
        (progn
          ; Get the two LINE entities adjacent to this corner
          (setq ent1 (nth (1- i) lines))  ; Line before corner
          (setq ent2 (nth i lines))       ; Line after corner
          ; Get the adjacent points for calculating midpoints
          (setq p0 (nth (1- i) pts))  ; Point before corner
          (setq p1 (nth i pts))       ; The corner point
          (setq p2 (nth (1+ i) pts))  ; Point after corner
          ; Calculate midpoints of each segment for pick points
          (setq mid1 (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p0 p1))
          (setq mid2 (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p1 p2))
          ; Set fillet radius first
          (command "._FILLET" "_R" r)
          ; Apply fillet using entity-point pairs with midpoints
          (command "._FILLET" (list ent1 mid1) (list ent2 mid2))
          (pcad-smart-dim-arc (entlast))
        )
      )
      (setq i (1+ i))
    )
    
    ; Step 3: Handle closing corner if closed
    (if closed
      (progn
        (setq r (nth (1- n) radii)) ; Radius at the last point (which is same as first)
        (if (and r (> r 0.0))
          (progn
            ; Fillet between last segment and first segment
            (setq ent1 (last lines))      ; Last drawn line
            (setq ent2 (car lines))       ; First drawn line
            
            ; Points regarding the closing corner (last pt = first pt)
            (setq p0 (nth (- n 2) pts))   ; Penultimate point
            (setq p1 (nth (1- n) pts))    ; Last point (corner)
            (setq p2 (nth 1 pts))         ; Second point (after corner)
            
            ; Calculate midpoints
            (setq mid1 (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p0 p1))
            (setq mid2 (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p1 p2))
            
            (command "._FILLET" "_R" r)
            (command "._FILLET" (list ent1 mid1) (list ent2 mid2))
            (pcad-smart-dim-arc (entlast))
          )
        )
      )
    )
  )

  ; pcad-calc-bulges: Calculate bulge values for LWPOLYLINE arc segments
  ; pts: list of (x y) points
  ; radii: list of radii (same length as pts, 0 for no arc)
  ; closed: T or nil
  ;
  ; CRITICAL: radius at vertex i means arc FROM vertex (i-1) TO vertex i
  ; The bulge value must be assigned to vertex (i-1), the ARC START
  ;
  ; Bulge = sagitta / half_chord, where:
  ;   chord = distance between arc start and arc end
  ;   sagitta = R - sqrt(R² - half_chord²)
  ;
  (defun pcad-calc-bulges (pts radii closed / i n bulges r next_r p0 p1 chord half_chord sagitta bulge)
    (setq n (length pts))
    ; Initialize all bulges to 0
    (setq bulges '())
    (repeat n (setq bulges (append bulges (list 0.0))))
    
    ; For each vertex i, check if the NEXT vertex (i+1) has a radius
    ; If yes, that defines an arc from i to i+1, so bulge goes at vertex i
    (setq i 0)
    (while (< i n)
      (setq next_idx (1+ i))
      (if (>= next_idx n)
        (if closed (setq next_idx 0) (setq next_idx nil))
      )
      (if next_idx
        (progn
          (setq next_r (nth next_idx radii))
          ; Check if radius is non-zero (can be negative for CW arc)
          (if (and next_r (/= next_r 0.0))
            (progn
              (setq abs_r (abs next_r))
              (setq is_cw (< next_r 0))
              
              ; Arc from vertex i to vertex next_idx
              (setq p0 (nth i pts))
              (setq p1 (nth next_idx pts))
              (setq chord (distance p0 p1))
              (if (> chord 0.0)
                (progn
                  (setq half_chord (/ chord 2.0))
                  (if (>= abs_r half_chord)
                    (progn
                      (setq sagitta (- abs_r (sqrt (- (* abs_r abs_r) (* half_chord half_chord)))))
                      (setq bulge (/ sagitta half_chord))
                      ; If neg radius (CW), negate the bulge
                      (if is_cw (setq bulge (- bulge)))
                      
                      ; Assign bulge to vertex i (arc start)
                      (setq bulges (subst-nth i bulge bulges))
                    )
                  )
                )
              )
            )
          )
        )
      )
      (setq i (1+ i))
    )
    bulges
  )

  ; Helper: substitute nth element in list
  (defun subst-nth (idx val lst / i result)
    (setq result '())
    (setq i 0)
    (foreach item lst
      (if (= i idx)
        (setq result (append result (list val)))
        (setq result (append result (list item)))
      )
      (setq i (1+ i))
    )
    result
  )

; =========================================
; Function: Render P-CAD Geometry
; Usage: (c:PCAD_Render)
; Note: Call (set-params ...) first to set parameters
; =========================================
(defun c:PCAD_Render ()
  (setvar "CMDECHO" 0)
  (setvar "DIMSCALE" 10.0)
  (setvar "DIMTXT" 10)
  (setvar "TEXTSIZE" 10)

  ; Check and set default parameters if not defined
  (if (not (boundp 'H)) (setq H 3000.0))
  (if (not (boundp 'B)) (setq B 3000.0))
  (if (not (boundp 'a1)) (setq a1 500.0))
  (if (not (boundp 'C)) (setq C 1200.0))
  (if (not (boundp 'R)) (setq R 2000.0))

  ; Calculate derived values
  (setq W_inner (* 2 R))
  (setq W_top (+ W_inner (* 2 a1)))
  (setq slope_w (/ (- W_top B) 2.0))
  (setq y_bot (- H R))

  ; Setup layers
  (command "._-LAYER" "_M" "outline" "_C" "4" "" "_LW" "0.25" "" "")
  (command "._-LAYER" "_M" "rebar" "_C" "1" "" "_LW" "0.2" "" "")
  (command "._-LAYER" "_M" "text" "_C" "3" "" "_LW" "0.18" "" "")
  (command "._-LAYER" "_M" "dim" "_C" "4" "" "_LW" "0.18" "" "")
  (command "._-LAYER" "_M" "hatch" "_C" "8" "" "_LW" "0.1" "" "")

  ; Setup text style: fsdb_e.shx,cadhzf.shx, width factor 0.7
  (command "._-STYLE" "Standard" "fsdb_e.shx,cadhzf.shx" 0 0.7 0 "_N" "_N" "")

  ; Sketch: U_Section
  (command "._-LAYER" "_S" "outline" "")
  ; Create LWPOLYLINE for U_Section_boundary with arc support
  (setq pts_U_Section_boundary (list
    (list (- 0 (/ B 2.0)) 0)
    (list (/ B 2.0) 0)
    (list (/ B 2.0) C)
    (list (/ W_top 2.0) H)
    (list (/ W_inner 2.0) H)
    (list 0 y_bot)
    (list (- 0 (/ W_inner 2.0)) H)
    (list (- 0 (/ W_top 2.0)) H)
    (list (- 0 (/ B 2.0)) C)
  ))
  (setq bulges_U_Section_boundary (list
    0.0
    0.0
    0.0
    0.0
    'CALC  ; Arc to vertex 5
    'CALC  ; Arc to vertex 6
    0.0
    0.0
    0.0
  ))
  (setq bulges_U_Section_boundary (pcad-calc-bulges pts_U_Section_boundary (list
    0.0
    0.0
    0.0
    0.0
    0.0
    (- 0 R)
    (- 0 R)
    0.0
    0.0
  ) T))
  (setq ent_data_U_Section_boundary (list
    '(0 . "LWPOLYLINE")
    '(100 . "AcDbEntity")
    (cons 8 "outline")
    '(100 . "AcDbPolyline")
    (cons 90 9)
    (cons 70 1)
  ))
  (setq i 0)
  (foreach pt pts_U_Section_boundary
    (setq b (nth i bulges_U_Section_boundary))
    (setq ent_data_U_Section_boundary
      (append ent_data_U_Section_boundary
        (list (cons 10 (list (car pt) (cadr pt) 0.0))
              (cons 42 b))
      )
    )
    (setq i (1+ i))
  )
  (entmake ent_data_U_Section_boundary)
  (setq sketch_U_Section_boundary (entlast))

  ; Region: ConcreteFill
  (command "._-LAYER" "_S" "hatch" "")
  ; Create hatch from sketch: U_Section
  (if (boundp 'sketch_U_Section_boundary)
    (progn
      (setvar "HPNAME" "ANSI31")
      (setvar "HPSCALE" 10.0)
      (setvar "HPANG" (* 0 (/ pi 180.0)))
      (command "._-BHATCH" "_S" sketch_U_Section_boundary "" "")
    )
    (princ "\nWarning: Boundary entity sketch_U_Section_boundary not found for region ConcreteFill\n")
  )

  (command "._-LAYER" "_S" "dim" "")
  ; Dimension: B
  (setq p1 (list (- 0 (/ B 2.0)) -400 0.0))
  (setq p2 (list (/ B 2.0) -400 0.0))
  (setq p3 (list (+ (- 0 (/ B 2.0)) 50) (+ -400 50) 0.0))
  (command "._DIMALIGNED" p1 p2 p3)
  (setq ent (entlast))
  (command "._DIMEDIT" "_N" "B" ent "")

  (command "._-LAYER" "_S" "dim" "")
  ; Dimension: H
  (setq p1 (list (- 0 (- (/ W_top 2.0) 500)) 0 0.0))
  (setq p2 (list (- 0 (- (/ W_top 2.0) 500)) H 0.0))
  (setq mid_x (/ (+ (- 0 (- (/ W_top 2.0) 500)) (- 0 (- (/ W_top 2.0) 500))) 2.0))
  (setq mid_y (/ (+ 0 H) 2.0))
  (setq p3 (list (+ mid_x 100) mid_y 0.0))
  (command "._DIMLINEAR" p1 p2 p3)
  (setq ent (entlast))
  (command "._DIMEDIT" "_N" "H" ent "")

  (command "._-LAYER" "_S" "dim" "")
  ; Dimension: C
  (setq p1 (list (+ (/ B 2.0) 500) 0 0.0))
  (setq p2 (list (+ (/ B 2.0) 500) C 0.0))
  (setq mid_x (/ (+ (+ (/ B 2.0) 500) (+ (/ B 2.0) 500)) 2.0))
  (setq mid_y (/ (+ 0 C) 2.0))
  (setq p3 (list (+ mid_x 100) mid_y 0.0))
  (command "._DIMLINEAR" p1 p2 p3)
  (setq ent (entlast))
  (command "._DIMEDIT" "_N" "C" ent "")

  (command "._-LAYER" "_S" "dim" "")
  ; Dimension: a1
  (setq p1 (list (- 0 (/ W_top 2.0)) (+ H 400) 0.0))
  (setq p2 (list (- 0 (/ W_inner 2.0)) (+ H 400) 0.0))
  (setq p3 (list (+ (- 0 (/ W_top 2.0)) 50) (+ (+ H 400) 50) 0.0))
  (command "._DIMALIGNED" p1 p2 p3)
  (setq ent (entlast))
  (command "._DIMEDIT" "_N" "a1" ent "")

  (command "._-LAYER" "_S" "dim" "")
  ; Dimension: a1
  (setq p1 (list (/ W_inner 2.0) (+ H 400) 0.0))
  (setq p2 (list (/ W_top 2.0) (+ H 400) 0.0))
  (setq p3 (list (+ (/ W_inner 2.0) 50) (+ (+ H 400) 50) 0.0))
  (command "._DIMALIGNED" p1 p2 p3)
  (setq ent (entlast))
  (command "._DIMEDIT" "_N" "a1" ent "")

  (command "._-LAYER" "_S" "dim" "")
  ; Dimension: 2.0
  (setq p1 (list 0 H 0.0))
  (setq p2 (list 0 y_bot 0.0))
  (setq mid_x (/ (+ 0 0) 2.0))
  (setq mid_y (/ (+ H y_bot) 2.0))
  (setq p3 (list (+ mid_x 100) mid_y 0.0))
  (command "._DIMLINEAR" p1 p2 p3)
  (setq ent (entlast))
  (command "._DIMEDIT" "_N" "2.0" ent "")

  ; --- Labels ---
  ; Label: 单位：米
  (command "._-LAYER" "_S" "text" "")
  (setq lbl_pt (list 0 (+ H 1000) 0.0))
  (command "._MTEXT" lbl_pt "_J" "_MC" "_H" (* (getvar "DIMSCALE") 10) "_W" 0 "单位：米" "")

  (setvar "CMDECHO" 1)
  (princ "\nP-CAD rendering complete.\n")
  (princ)
)

; =========================================
; Usage Examples:
; =========================================
;
; Method 1: Set parameters then render
;   (set-params 3000 3000 500 1200 2000)
;   (c:PCAD_Render)
;
; Method 2: Set parameters individually
;   (setq H 3000.0)
;   (setq B 3000.0)
;   (setq a1 500.0)
;   (setq C 1200.0)
;   (setq R 2000.0)
;   (c:PCAD_Render)
;
; Method 3: Use default parameters
;   (c:PCAD_Render)  ; Uses default values if not set
