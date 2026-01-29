; =========================================
; AutoLISP generated from P-CAD
; Parameter Injection: Approach 4 (Global Variables)
; =========================================

; =========================================
; Function: Set Parameters
; Usage: (set-params W_cm H_cm a1_cm a2_cm a3_cm d1_cm d2_cm d3_cm BaseH_cm BaseW_cm)
; =========================================
(defun set-params (W_cm_val H_cm_val a1_cm_val a2_cm_val a3_cm_val d1_cm_val d2_cm_val d3_cm_val BaseH_cm_val BaseW_cm_val)
  (setq W_cm W_cm_val)
  (setq H_cm H_cm_val)
  (setq a1_cm a1_cm_val)
  (setq a2_cm a2_cm_val)
  (setq a3_cm a3_cm_val)
  (setq d1_cm d1_cm_val)
  (setq d2_cm d2_cm_val)
  (setq d3_cm d3_cm_val)
  (setq BaseH_cm BaseH_cm_val)
  (setq BaseW_cm BaseW_cm_val)
  (princ (strcat "\nParameters set: " "W_cm=" (rtos W_cm) ", " "H_cm=" (rtos H_cm) ", " "a1_cm=" (rtos a1_cm) ", " "a2_cm=" (rtos a2_cm) ", " "a3_cm=" (rtos a3_cm) ", " "d1_cm=" (rtos d1_cm) ", " "d2_cm=" (rtos d2_cm) ", " "d3_cm=" (rtos d3_cm) ", " "BaseH_cm=" (rtos BaseH_cm) ", " "BaseW_cm=" (rtos BaseW_cm) "\n"))
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
  (if (not (boundp 'W_cm)) (setq W_cm 400.0))
  (if (not (boundp 'H_cm)) (setq H_cm 300.0))
  (if (not (boundp 'a1_cm)) (setq a1_cm 30.0))
  (if (not (boundp 'a2_cm)) (setq a2_cm 30.0))
  (if (not (boundp 'a3_cm)) (setq a3_cm 30.0))
  (if (not (boundp 'd1_cm)) (setq d1_cm 15.0))
  (if (not (boundp 'd2_cm)) (setq d2_cm 15.0))
  (if (not (boundp 'd3_cm)) (setq d3_cm 15.0))
  (if (not (boundp 'BaseH_cm)) (setq BaseH_cm 40.0))
  (if (not (boundp 'BaseW_cm)) (setq BaseW_cm 450.0))

  ; Calculate derived values
  (setq W (* W_cm 10))
  (setq H (* H_cm 10))
  (setq a1 (* a1_cm 10))
  (setq a2 (* a2_cm 10))
  (setq a3 (* a3_cm 10))
  (setq d1 (* d1_cm 10))
  (setq d2 (* d2_cm 10))
  (setq d3 (* d3_cm 10))
  (setq BaseH (* BaseH_cm 10))
  (setq BaseW (* BaseW_cm 10))
  (setq CulvertX (/ (- BaseW W) 2.0))
  (setq InnerL (+ CulvertX a2))
  (setq InnerR (- (+ CulvertX W) a2))
  (setq InnerB (+ BaseH a3))
  (setq InnerT (- (+ BaseH H) a1))

  ; Setup layers
  (command "._-LAYER" "_M" "concrete" "_C" "4" "" "_LW" "0.5" "" "")
  (command "._-LAYER" "_M" "dim" "_C" "7" "" "_LW" "0.18" "" "")
  (command "._-LAYER" "_M" "hatch" "_C" "8" "" "_LW" "0.1" "" "")
  (command "._-LAYER" "_M" "text" "_C" "3" "" "_LW" "0.25" "" "")
  (command "._-LAYER" "_M" "outline" "_C" "4" "" "_LW" "0.25" "" "")
  (command "._-LAYER" "_M" "rebar" "_C" "1" "" "_LW" "0.2" "" "")

  ; Sketch: BaseSlab
  (command "._-LAYER" "_S" "concrete" "")
  ; Create LWPOLYLINE for BaseSlab_outer
  (setq pts_BaseSlab_outer (list
    (list 0 0)
    (list BaseW 0)
    (list BaseW BaseH)
    (list 0 BaseH)
  ))
  (setq ent_data_BaseSlab_outer (list
    '(0 . "LWPOLYLINE")
    '(100 . "AcDbEntity")
    (cons 8 "concrete")
    '(100 . "AcDbPolyline")
    (cons 90 4)
    (cons 70 1)
  ))
  (foreach pt pts_BaseSlab_outer
    (setq ent_data_BaseSlab_outer
      (append ent_data_BaseSlab_outer
        (list (cons 10 (list (car pt) (cadr pt) 0.0)))
      )
    )
  )
  (entmake ent_data_BaseSlab_outer)
  (setq sketch_BaseSlab_outer (entlast))

  ; Sketch: CulvertBody
  (command "._-LAYER" "_S" "concrete" "")
  ; Create LWPOLYLINE for CulvertBody_outer
  (setq pts_CulvertBody_outer (list
    (list CulvertX BaseH)
    (list (+ CulvertX W) BaseH)
    (list (+ CulvertX W) (+ BaseH H))
    (list CulvertX (+ BaseH H))
  ))
  (setq ent_data_CulvertBody_outer (list
    '(0 . "LWPOLYLINE")
    '(100 . "AcDbEntity")
    (cons 8 "concrete")
    '(100 . "AcDbPolyline")
    (cons 90 4)
    (cons 70 1)
  ))
  (foreach pt pts_CulvertBody_outer
    (setq ent_data_CulvertBody_outer
      (append ent_data_CulvertBody_outer
        (list (cons 10 (list (car pt) (cadr pt) 0.0)))
      )
    )
  )
  (entmake ent_data_CulvertBody_outer)
  (setq sketch_CulvertBody_outer (entlast))
  ; Create LWPOLYLINE for CulvertBody_inner
  (setq pts_CulvertBody_inner (list
    (list (+ InnerL d3) InnerB)
    (list (- InnerR d3) InnerB)
    (list InnerR (+ InnerB d3))
    (list InnerR (- InnerT d2))
    (list (- InnerR d1) InnerT)
    (list (+ InnerL d1) InnerT)
    (list InnerL (- InnerT d2))
    (list InnerL (+ InnerB d3))
  ))
  (setq ent_data_CulvertBody_inner (list
    '(0 . "LWPOLYLINE")
    '(100 . "AcDbEntity")
    (cons 8 "concrete")
    '(100 . "AcDbPolyline")
    (cons 90 8)
    (cons 70 1)
  ))
  (foreach pt pts_CulvertBody_inner
    (setq ent_data_CulvertBody_inner
      (append ent_data_CulvertBody_inner
        (list (cons 10 (list (car pt) (cadr pt) 0.0)))
      )
    )
  )
  (entmake ent_data_CulvertBody_inner)
  (setq sketch_CulvertBody_inner (entlast))

  ; Region: BaseRegion
  (command "._-LAYER" "_S" "hatch" "")
  ; Create hatch from sketch: BaseSlab
  (if (boundp 'sketch_BaseSlab_outer)
    (command "._HATCH" "_P" "ANSI31" "1.0" "0" "_S" sketch_BaseSlab_outer "" "")
    (princ "\nWarning: Boundary entity sketch_BaseSlab_outer not found for region BaseRegion\n")
  )

  ; Dimension: H
  (command "._-LAYER" "_S" "dim" "")
  (setq p1 (list (+ BaseW 200) BaseH 0.0))
  (setq p2 (list (+ BaseW 200) (+ BaseH H) 0.0))
  (setq mid_x (/ (+ (+ BaseW 200) (+ BaseW 200)) 2.0))
  (setq mid_y (/ (+ BaseH (+ BaseH H)) 2.0))
  (setq p3 (list (+ mid_x 100) mid_y 0.0))
  (command "._DIMLINEAR" p1 p2 p3)
  (setq ent (entlast))
  (command "._DIMEDIT" "_N" "H" ent "")

  ; Dimension: 40
  (command "._-LAYER" "_S" "dim" "")
  (setq p1 (list -200 0 0.0))
  (setq p2 (list -200 BaseH 0.0))
  (setq mid_x (/ (+ -200 -200) 2.0))
  (setq mid_y (/ (+ 0 BaseH) 2.0))
  (setq p3 (list (+ mid_x 100) mid_y 0.0))
  (command "._DIMLINEAR" p1 p2 p3)
  (setq ent (entlast))
  (command "._DIMEDIT" "_N" "40" ent "")

  ; Dimension: W
  (command "._-LAYER" "_S" "dim" "")
  (setq p1 (list CulvertX (+ (+ BaseH H) 200) 0.0))
  (setq p2 (list (+ CulvertX W) (+ (+ BaseH H) 200) 0.0))
  (setq mid_x (/ (+ CulvertX (+ CulvertX W)) 2.0))
  (setq mid_y (/ (+ (+ (+ BaseH H) 200) (+ (+ BaseH H) 200)) 2.0))
  (setq p3 (list mid_x (+ mid_y 100) 0.0))
  (command "._DIMLINEAR" p1 p2 p3)
  (setq ent (entlast))
  (command "._DIMEDIT" "_N" "W" ent "")

  ; Dimension: W+50
  (command "._-LAYER" "_S" "dim" "")
  (setq p1 (list 0 -200 0.0))
  (setq p2 (list BaseW -200 0.0))
  (setq mid_x (/ (+ 0 BaseW) 2.0))
  (setq mid_y (/ (+ -200 -200) 2.0))
  (setq p3 (list mid_x (+ mid_y 100) 0.0))
  (command "._DIMLINEAR" p1 p2 p3)
  (setq ent (entlast))
  (command "._DIMEDIT" "_N" "W+50" ent "")

  ; Dimension: a2
  (command "._-LAYER" "_S" "dim" "")
  (setq p1 (list CulvertX (+ BaseH (/ H 2.0)) 0.0))
  (setq p2 (list InnerL (+ BaseH (/ H 2.0)) 0.0))
  (setq mid_x (/ (+ CulvertX InnerL) 2.0))
  (setq mid_y (/ (+ (+ BaseH (/ H 2.0)) (+ BaseH (/ H 2.0))) 2.0))
  (setq p3 (list mid_x (+ mid_y 100) 0.0))
  (command "._DIMLINEAR" p1 p2 p3)
  (setq ent (entlast))
  (command "._DIMEDIT" "_N" "a2" ent "")

  ; Dimension: a2
  (command "._-LAYER" "_S" "dim" "")
  (setq p1 (list InnerR (+ BaseH (/ H 2.0)) 0.0))
  (setq p2 (list (+ CulvertX W) (+ BaseH (/ H 2.0)) 0.0))
  (setq mid_x (/ (+ InnerR (+ CulvertX W)) 2.0))
  (setq mid_y (/ (+ (+ BaseH (/ H 2.0)) (+ BaseH (/ H 2.0))) 2.0))
  (setq p3 (list mid_x (+ mid_y 100) 0.0))
  (command "._DIMLINEAR" p1 p2 p3)
  (setq ent (entlast))
  (command "._DIMEDIT" "_N" "a2" ent "")

  ; Dimension: a3
  (command "._-LAYER" "_S" "dim" "")
  (setq p1 (list (/ BaseW 2.0) BaseH 0.0))
  (setq p2 (list (/ BaseW 2.0) InnerB 0.0))
  (setq mid_x (/ (+ (/ BaseW 2.0) (/ BaseW 2.0)) 2.0))
  (setq mid_y (/ (+ BaseH InnerB) 2.0))
  (setq p3 (list (+ mid_x 100) mid_y 0.0))
  (command "._DIMLINEAR" p1 p2 p3)
  (setq ent (entlast))
  (command "._DIMEDIT" "_N" "a3" ent "")

  ; Dimension: a1
  (command "._-LAYER" "_S" "dim" "")
  (setq p1 (list (/ BaseW 2.0) InnerT 0.0))
  (setq p2 (list (/ BaseW 2.0) (+ BaseH H) 0.0))
  (setq mid_x (/ (+ (/ BaseW 2.0) (/ BaseW 2.0)) 2.0))
  (setq mid_y (/ (+ InnerT (+ BaseH H)) 2.0))
  (setq p3 (list (+ mid_x 100) mid_y 0.0))
  (command "._DIMLINEAR" p1 p2 p3)
  (setq ent (entlast))
  (command "._DIMEDIT" "_N" "a1" ent "")

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

  (setvar "CMDECHO" 1)
  (princ "\nP-CAD rendering complete.\n")
  (princ)
)

; =========================================
; Usage Examples:
; =========================================
;
; Method 1: Set parameters then render
;   (set-params 400 300 30 30 30 15 15 15 40 450)
;   (c:PCAD_Render)
;
; Method 2: Set parameters individually
;   (setq W_cm 400.0)
;   (setq H_cm 300.0)
;   (setq a1_cm 30.0)
;   (setq a2_cm 30.0)
;   (setq a3_cm 30.0)
;   (setq d1_cm 15.0)
;   (setq d2_cm 15.0)
;   (setq d3_cm 15.0)
;   (setq BaseH_cm 40.0)
;   (setq BaseW_cm 450.0)
;   (c:PCAD_Render)
;
; Method 3: Use default parameters
;   (c:PCAD_Render)  ; Uses default values if not set
