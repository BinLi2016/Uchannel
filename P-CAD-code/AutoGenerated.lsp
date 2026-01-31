; =========================================
; AutoLISP generated from P-CAD
; Parameter Injection: Approach 4 (Global Variables)
; =========================================

; =========================================
; Function: Set Parameters
; Usage: (set-params W H S D)
; =========================================
(defun set-params (W_val H_val S_val D_val)
  (setq W W_val)
  (setq H H_val)
  (setq S S_val)
  (setq D D_val)
  (princ (strcat "\nParameters set: " "W=" (rtos W) ", " "H=" (rtos H) ", " "S=" (rtos S) ", " "D=" (rtos D) "\n"))
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
  (setvar "DIMSCALE" 15.0)
  (setvar "DIMTXT" 10)
  (setvar "TEXTSIZE" 10)

  ; Check and set default parameters if not defined
  (if (not (boundp 'W)) (setq W 4000.0))
  (if (not (boundp 'H)) (setq H 2000.0))
  (if (not (boundp 'S)) (setq S 2400.0))
  (if (not (boundp 'D)) (setq D 800.0))

  ; Calculate derived values
  (setq R (/ D 2.0))
  (setq cx1 (/ (- W S) 2.0))
  (setq cx2 (- W cx1))
  (setq cy (/ H 2.0))

  ; Setup layers
  (command "._-LAYER" "_M" "outline" "_C" "4" "" "_LW" "0.25" "" "")
  (command "._-LAYER" "_M" "hatch" "_C" "8" "" "_LW" "0.15" "" "")
  (command "._-LAYER" "_M" "dim" "_C" "4" "" "_LW" "0.18" "" "")
  (command "._-LAYER" "_M" "rebar" "_C" "1" "" "_LW" "0.2" "" "")
  (command "._-LAYER" "_M" "text" "_C" "3" "" "_LW" "0.18" "" "")

  ; Setup text style: fsdb_e.shx,cadhzf.shx, width factor 0.7
  (command "._-STYLE" "Standard" "fsdb_e.shx,cadhzf.shx" 0 0.7 0 "_N" "_N" "")

  ; Sketch: Outlines
  (command "._-LAYER" "_S" "outline" "")
  ; Create LWPOLYLINE for Outlines_Boundary with arc support
  (setq pts_Outlines_Boundary (list
    (list 0 0)
    (list W 0)
    (list W H)
    (list 0 H)
  ))
  (setq bulges_Outlines_Boundary (list
    0.0
    0.0
    0.0
    0.0
  ))
  (setq bulges_Outlines_Boundary (pcad-calc-bulges pts_Outlines_Boundary (list
    0.0
    0.0
    0.0
    0.0
  ) T))
  (setq ent_data_Outlines_Boundary (list
    '(0 . "LWPOLYLINE")
    '(100 . "AcDbEntity")
    (cons 8 "outline")
    '(100 . "AcDbPolyline")
    (cons 90 4)
    (cons 70 1)
  ))
  (setq i 0)
  (foreach pt pts_Outlines_Boundary
    (setq b (nth i bulges_Outlines_Boundary))
    (setq ent_data_Outlines_Boundary
      (append ent_data_Outlines_Boundary
        (list (cons 10 (list (car pt) (cadr pt) 0.0))
              (cons 42 b))
      )
    )
    (setq i (1+ i))
  )
  (entmake ent_data_Outlines_Boundary)
  (setq sketch_Outlines_Boundary (entlast))
  ; Circle: Hole1
  (command "._CIRCLE" (list cx1 cy 0.0) R)
  (setq sketch_Outlines_Hole1 (entlast))
  ; Circle: Hole2
  (command "._CIRCLE" (list cx2 cy 0.0) R)
  (setq sketch_Outlines_Hole2 (entlast))

  ; Region: TwinCellSection
  (command "._-LAYER" "_S" "hatch" "")
  ; Create hatch from sketch: Outlines
  (if (boundp 'sketch_Outlines_Boundary)
    (progn
      (setvar "HPNAME" "ANSI31")
      (setvar "HPSCALE" 15.0)
      (setvar "HPANG" (* 0 (/ pi 180.0)))
      (setq ss (ssadd))
      (ssadd sketch_Outlines_Boundary ss)
      (if (boundp 'sketch_Outlines_Hole1) (ssadd sketch_Outlines_Hole1 ss))
      (if (boundp 'sketch_Outlines_Hole2) (ssadd sketch_Outlines_Hole2 ss))
      (command "._-BHATCH" "_S" ss "" "")
    )
    (princ "\nWarning: Boundary entity sketch_Outlines_Boundary not found for region TwinCellSection\n")
  )

  (command "._-LAYER" "_S" "dim" "")
  ; Dimension: 4000
  (setq p1 (list 0 H 0.0))
  (setq p2 (list W H 0.0))
  (setq mid_x (/ (+ 0 W) 2.0))
  (setq mid_y (/ (+ H H) 2.0))
  (setq p3 (list mid_x (+ mid_y 300) 0.0))
  (command "._DIMLINEAR" p1 p2 p3)
  (setq ent (entlast))
  (command "._DIMEDIT" "_N" "4000" ent "")

  (command "._-LAYER" "_S" "dim" "")
  ; Dimension: 2000
  (setq p1 (list W 0 0.0))
  (setq p2 (list W H 0.0))
  (setq mid_x (/ (+ W W) 2.0))
  (setq mid_y (/ (+ 0 H) 2.0))
  (setq p3 (list (+ mid_x 300) mid_y 0.0))
  (command "._DIMLINEAR" p1 p2 p3)
  (setq ent (entlast))
  (command "._DIMEDIT" "_N" "2000" ent "")

  (command "._-LAYER" "_S" "dim" "")
  ; Dimension: 2400
  (setq p1 (list cx1 0 0.0))
  (setq p2 (list cx2 0 0.0))
  (setq mid_x (/ (+ cx1 cx2) 2.0))
  (setq mid_y (/ (+ 0 0) 2.0))
  (setq p3 (list mid_x (+ mid_y -300) 0.0))
  (command "._DIMLINEAR" p1 p2 p3)
  (setq ent (entlast))
  (command "._DIMEDIT" "_N" "2400" ent "")

  (command "._-LAYER" "_S" "dim" "")
  ; Radial Dimension: φ800
  (setq center_pt (list cx2 cy 0.0))
  (setq rad_val R)
  (setq ang_rad (* 45 (/ pi 180.0)))
  ; Calculate arc point on the circle at specified angle
  (setq arc_pt (list (+ cx2 (* rad_val (cos ang_rad)))
                     (+ cy (* rad_val (sin ang_rad))) 0.0))
  ; Calculate leader endpoint (outside the arc)
  (setq leader_pt (list (+ cx2 (* (* rad_val 1.3) (cos ang_rad)))
                        (+ cy (* (* rad_val 1.3) (sin ang_rad))) 0.0))
  ; Draw leader line from arc point to text location
  (command "._LINE" arc_pt leader_pt "")
  (command "._MTEXT" leader_pt "_J" "_ML" "_H" (* (getvar "DIMSCALE") 10) "_W" 0 "φ800" "")

  (setvar "CMDECHO" 1)
  (princ "\nP-CAD rendering complete.\n")
  (princ)
)

; =========================================
; Usage Examples:
; =========================================
;
; Method 1: Set parameters then render
;   (set-params 4000 2000 2400 800)
;   (c:PCAD_Render)
;
; Method 2: Set parameters individually
;   (setq W 4000.0)
;   (setq H 2000.0)
;   (setq S 2400.0)
;   (setq D 800.0)
;   (c:PCAD_Render)
;
; Method 3: Use default parameters
;   (c:PCAD_Render)  ; Uses default values if not set
