; =========================================
; AutoLISP generated from P-CAD
; Parameter Injection: Approach 4 (Global Variables)
; =========================================

; =========================================
; Function: Set Parameters
; Usage: (set-params L h1 h2 cover)
; =========================================
(defun set-params (L_val h1_val h2_val cover_val)
  (setq L L_val)
  (setq h1 h1_val)
  (setq h2 h2_val)
  (setq cover cover_val)
  (princ (strcat "\nParameters set: " "L=" (rtos L) ", " "h1=" (rtos h1) ", " "h2=" (rtos h2) ", " "cover=" (rtos cover) "\n"))
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
  (if (not (boundp 'L)) (setq L 4000.0))
  (if (not (boundp 'h1)) (setq h1 1600.0))
  (if (not (boundp 'h2)) (setq h2 1200.0))
  (if (not (boundp 'cover)) (setq cover 50.0))

  ; Calculate derived values
  (setq InnerL cover)
  (setq InnerR (- L cover))
  (setq InnerB cover)

  ; Setup layers
  (command "._-LAYER" "_M" "outline" "_C" "4" "" "_LW" "0.5" "" "")
  (command "._-LAYER" "_M" "rebar" "_C" "1" "" "_LW" "0.3" "" "")
  (command "._-LAYER" "_M" "dim" "_C" "7" "" "_LW" "0.18" "" "")
  (command "._-LAYER" "_M" "text" "_C" "3" "" "_LW" "0.25" "" "")
  (command "._-LAYER" "_M" "hatch" "_C" "8" "" "_LW" "0.1" "" "")

  ; Setup text style: fsdb_e.shx,cadhzf.shx, width factor 0.75
  (command "._-STYLE" "Standard" "fsdb_e.shx,cadhzf.shx" 0 0.75 0 "_N" "_N" "")

  ; Sketch: SidewallOutline
  (command "._-LAYER" "_S" "outline" "")
  ; Create LWPOLYLINE for SidewallOutline_outer
  (setq pts_SidewallOutline_outer (list
    (list 0 0)
    (list L 0)
    (list L h2)
    (list 0 h1)
  ))
  (setq ent_data_SidewallOutline_outer (list
    '(0 . "LWPOLYLINE")
    '(100 . "AcDbEntity")
    (cons 8 "outline")
    '(100 . "AcDbPolyline")
    (cons 90 4)
    (cons 70 1)
  ))
  (foreach pt pts_SidewallOutline_outer
    (setq ent_data_SidewallOutline_outer
      (append ent_data_SidewallOutline_outer
        (list (cons 10 (list (car pt) (cadr pt) 0.0)))
      )
    )
  )
  (entmake ent_data_SidewallOutline_outer)
  (setq sketch_SidewallOutline_outer (entlast))

  ; Sketch: N1Bars
  (command "._-LAYER" "_S" "rebar" "")
  (command "._LINE" (list InnerL 50.0 0.0) (list InnerR 50.0 0.0) "")
  (command "._LINE" (list InnerL 216.7 0.0) (list InnerR 172.2 0.0) "")
  (command "._LINE" (list InnerL 383.3 0.0) (list InnerR 294.4 0.0) "")
  (command "._LINE" (list InnerL 550.0 0.0) (list InnerR 416.7 0.0) "")
  (command "._LINE" (list InnerL 716.7 0.0) (list InnerR 538.9 0.0) "")
  (command "._LINE" (list InnerL 883.3 0.0) (list InnerR 661.1 0.0) "")
  (command "._LINE" (list InnerL 1050.0 0.0) (list InnerR 783.3 0.0) "")
  (command "._LINE" (list InnerL 1216.7 0.0) (list InnerR 905.6 0.0) "")
  (command "._LINE" (list InnerL 1383.3 0.0) (list InnerR 1027.8 0.0) "")
  (command "._LINE" (list InnerL 1550.0 0.0) (list InnerR 1150.0 0.0) "")

  ; Sketch: N2Bars
  (command "._-LAYER" "_S" "rebar" "")
  (command "._LINE" (list 50.0 InnerB 0.0) (list 50.0 1545.0 0.0) "")
  (command "._LINE" (list 245.0 InnerB 0.0) (list 245.0 1525.5 0.0) "")
  (command "._LINE" (list 440.0 InnerB 0.0) (list 440.0 1506.0 0.0) "")
  (command "._LINE" (list 635.0 InnerB 0.0) (list 635.0 1486.5 0.0) "")
  (command "._LINE" (list 830.0 InnerB 0.0) (list 830.0 1467.0 0.0) "")
  (command "._LINE" (list 1025.0 InnerB 0.0) (list 1025.0 1447.5 0.0) "")
  (command "._LINE" (list 1220.0 InnerB 0.0) (list 1220.0 1428.0 0.0) "")
  (command "._LINE" (list 1415.0 InnerB 0.0) (list 1415.0 1408.5 0.0) "")
  (command "._LINE" (list 1610.0 InnerB 0.0) (list 1610.0 1389.0 0.0) "")
  (command "._LINE" (list 1805.0 InnerB 0.0) (list 1805.0 1369.5 0.0) "")
  (command "._LINE" (list 2000.0 InnerB 0.0) (list 2000.0 1350.0 0.0) "")
  (command "._LINE" (list 2195.0 InnerB 0.0) (list 2195.0 1330.5 0.0) "")
  (command "._LINE" (list 2390.0 InnerB 0.0) (list 2390.0 1311.0 0.0) "")
  (command "._LINE" (list 2585.0 InnerB 0.0) (list 2585.0 1291.5 0.0) "")
  (command "._LINE" (list 2780.0 InnerB 0.0) (list 2780.0 1272.0 0.0) "")
  (command "._LINE" (list 2975.0 InnerB 0.0) (list 2975.0 1252.5 0.0) "")
  (command "._LINE" (list 3170.0 InnerB 0.0) (list 3170.0 1233.0 0.0) "")
  (command "._LINE" (list 3365.0 InnerB 0.0) (list 3365.0 1213.5 0.0) "")
  (command "._LINE" (list 3560.0 InnerB 0.0) (list 3560.0 1194.0 0.0) "")
  (command "._LINE" (list 3755.0 InnerB 0.0) (list 3755.0 1174.5 0.0) "")
  (command "._LINE" (list 3950.0 InnerB 0.0) (list 3950.0 1155.0 0.0) "")

  ; Dimension: L=400CM
  (command "._-LAYER" "_S" "dim" "")
  (setq p1 (list 0 -200 0.0))
  (setq p2 (list L -200 0.0))
  (setq mid_x (/ (+ 0 L) 2.0))
  (setq mid_y (/ (+ -200 -200) 2.0))
  (setq p3 (list mid_x (+ mid_y 100) 0.0))
  (command "._DIMLINEAR" p1 p2 p3)
  (setq ent (entlast))
  (command "._DIMEDIT" "_N" "L=400CM" ent "")

  ; Dimension: h1
  (command "._-LAYER" "_S" "dim" "")
  (setq p1 (list -200 0 0.0))
  (setq p2 (list -200 h1 0.0))
  (setq mid_x (/ (+ -200 -200) 2.0))
  (setq mid_y (/ (+ 0 h1) 2.0))
  (setq p3 (list (+ mid_x 100) mid_y 0.0))
  (command "._DIMLINEAR" p1 p2 p3)
  (setq ent (entlast))
  (command "._DIMEDIT" "_N" "h1" ent "")

  ; Dimension: h2=120CM
  (command "._-LAYER" "_S" "dim" "")
  (setq p1 (list (+ L 200) 0 0.0))
  (setq p2 (list (+ L 200) h2 0.0))
  (setq mid_x (/ (+ (+ L 200) (+ L 200)) 2.0))
  (setq mid_y (/ (+ 0 h2) 2.0))
  (setq p3 (list (+ mid_x 100) mid_y 0.0))
  (command "._DIMLINEAR" p1 p2 p3)
  (setq ent (entlast))
  (command "._DIMEDIT" "_N" "h2=120CM" ent "")

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

  ; --- Labels ---
  ; Label: cover=50mm
  (command "._-LAYER" "_S" "text" "")
  (setq lbl_pt (list (/ L 2.0) -400 0.0))
  (command "._MTEXT" lbl_pt "_J" "_MC" "_H" 40 "_W" 0 "cover=50mm" "")
  ; Label: 10 N1-1~10 扇形分布
  (command "._-LAYER" "_S" "text" "")
  (setq lbl_pt (list (/ L 4.0) 1700 0.0))
  (command "._MTEXT" lbl_pt "_J" "_MC" "_H" 40 "_W" 0 "10 N1-1~10 扇形分布" "")
  ; Label: N2 @ (L-Cover)/20 均匀平行分布
  (command "._-LAYER" "_S" "text" "")
  (setq lbl_pt (list (/ (* 3 L) 4.0) (/ cover 2.0) 0.0))
  (command "._MTEXT" lbl_pt "_J" "_MC" "_H" 40 "_W" 0 "N2 @ (L-Cover)/20 均匀平行分布" "")

  (setvar "CMDECHO" 1)
  (princ "\nP-CAD rendering complete.\n")
  (princ)
)

; =========================================
; Usage Examples:
; =========================================
;
; Method 1: Set parameters then render
;   (set-params 4000 1600 1200 50)
;   (c:PCAD_Render)
;
; Method 2: Set parameters individually
;   (setq L 4000.0)
;   (setq h1 1600.0)
;   (setq h2 1200.0)
;   (setq cover 50.0)
;   (c:PCAD_Render)
;
; Method 3: Use default parameters
;   (c:PCAD_Render)  ; Uses default values if not set
