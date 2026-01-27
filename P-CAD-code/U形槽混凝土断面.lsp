; =========================================
; AutoLISP generated from P-CAD
; Parameter Injection: Approach 4 (Global Variables)
; =========================================

; =========================================
; Function: Set Parameters
; Usage: (set-params B H W a d t)
; =========================================
(defun set-params (B_val H_val W_val a_val d_val t_val)
  (setq B B_val)
  (setq H H_val)
  (setq W W_val)
  (setq a a_val)
  (setq d d_val)
  (setq t_param t_val)  ; Use t_param to avoid conflict with AutoLISP 't'
  (setq t t_param)
  (princ (strcat "\nParameters set: " "B=" (rtos B) ", " "H=" (rtos H) ", " "W=" (rtos W) ", " "a=" (rtos a) ", " "d=" (rtos d) ", " "t=" (rtos t_param) "\n"))
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
  (if (not (boundp 'B)) (setq B 2000.0))
  (if (not (boundp 'H)) (setq H 1500.0))
  (if (not (boundp 'W)) (setq W 1200.0))
  (if (not (boundp 'a)) (setq a 200.0))
  (if (not (boundp 'd)) (setq d 300.0))
  (if (not (boundp 't_param)) (setq t_param 200.0))
  (setq t t_param)  ; Use t_param to avoid conflict with AutoLISP 't'

  ; Calculate derived values
  (setq H_total (+ H t_param))
  (setq x_left_inner (/ (- B W) 2.0))
  (setq x_right_inner (/ (+ B W) 2.0))
  (setq x_wing_left (- (/ (- B W) 2.0) a))
  (setq x_wing_right (+ (/ (+ B W) 2.0) a))

  ; Setup layers
  (command "._LAYER" "_M" "outline" "_C" "4" "" "_LW" "0.25" "" "")
  (command "._LAYER" "_M" "hatch" "_C" "8" "" "_LW" "0.1" "" "")
  (command "._LAYER" "_M" "dim" "_C" "4" "" "_LW" "0.18" "" "")
  (command "._LAYER" "_M" "text" "_C" "3" "" "_LW" "0.18" "" "")

  ; Sketch: U槽轮廓
  (command "._LAYER" "_S" "outline" "")
    (setq pt_0 (list 0 0 0.0))
    (setq pt_1 (list B 0 0.0))
    (setq pt_2 (list B d 0.0))
    (setq pt_3 (list x_wing_right H_total 0.0))
    (setq pt_4 (list x_right_inner H_total 0.0))
    (setq pt_5 (list x_right_inner t_param 0.0))
    (setq pt_6 (list x_left_inner t_param 0.0))
    (setq pt_7 (list x_left_inner H_total 0.0))
    (setq pt_8 (list x_wing_left H_total 0.0))
    (setq pt_9 (list 0 d 0.0))
  (command "._PLINE")
    (command pt_0)
    (command pt_1)
    (command pt_2)
    (command pt_3)
    (command pt_4)
    (command pt_5)
    (command pt_6)
    (command pt_7)
    (command pt_8)
    (command pt_9)
  (command "_C")

  ; Region: U槽断面
  (command "._LAYER" "_S" "hatch" "")
  ; Create hatch on last entity (the polyline)
  (setq hatch_obj (entlast))
  (command "._HATCH" "_P" "ANSI31" "1.0" "0" "_S" "L" "" "")

  ; Dimension: B
  (command "._LAYER" "_S" "dim" "")
    (setq dim_pt (list (/ (+ 0 B) 2.0) (- (min 0 0) 100.0) 0.0))
    (command "._DIMLINEAR" (list 0 0 0.0) (list B 0 0.0) dim_pt)

  ; Dimension: W
  (command "._LAYER" "_S" "dim" "")
    (setq dim_pt (list (/ (+ x_left_inner x_right_inner) 2.0) (- (min t_param t_param) 100.0) 0.0))
    (command "._DIMLINEAR" (list x_left_inner t_param 0.0) (list x_right_inner t_param 0.0) dim_pt)

  ; Dimension: a
  (command "._LAYER" "_S" "dim" "")
    (setq dim_pt (list (/ (+ x_right_inner x_wing_right) 2.0) (- (min H_total H_total) 100.0) 0.0))
    (command "._DIMLINEAR" (list x_right_inner H_total 0.0) (list x_wing_right H_total 0.0) dim_pt)

  ; Dimension: a
  (command "._LAYER" "_S" "dim" "")
    (setq dim_pt (list (/ (+ x_wing_left x_left_inner) 2.0) (- (min H_total H_total) 100.0) 0.0))
    (command "._DIMLINEAR" (list x_wing_left H_total 0.0) (list x_left_inner H_total 0.0) dim_pt)

  ; Dimension: H
  (command "._LAYER" "_S" "dim" "")
    (setq dim_pt (list (+ (max 0 0) 100.0) (/ (+ 0 H_total) 2.0) 0.0))
    (command "._DIMLINEAR" (list 0 0 0.0) (list 0 H_total 0.0) dim_pt)

  ; Dimension: H
  (command "._LAYER" "_S" "dim" "")
    (setq dim_pt (list (+ (max x_left_inner x_left_inner) 100.0) (/ (+ t_param H_total) 2.0) 0.0))
    (command "._DIMLINEAR" (list x_left_inner t_param 0.0) (list x_left_inner H_total 0.0) dim_pt)

  ; Dimension: t
  (command "._LAYER" "_S" "dim" "")
    (setq dim_pt (list (+ (max 0 0) 100.0) (/ (+ 0 t_param) 2.0) 0.0))
    (command "._DIMLINEAR" (list 0 0 0.0) (list 0 t_param 0.0) dim_pt)

  ; Dimension: d
  (command "._LAYER" "_S" "dim" "")
    (setq dim_pt (list (+ (max B B) 100.0) (/ (+ 0 d) 2.0) 0.0))
    (command "._DIMLINEAR" (list B 0 0.0) (list B d 0.0) dim_pt)

  (setvar "CMDECHO" 1)
  (princ "\nP-CAD rendering complete.\n")
  (princ)
)

; =========================================
; Usage Examples:
; =========================================
;
; Method 1: Set parameters then render
;   (set-params 2000 1500 1200 200 300 200)
;   (c:PCAD_Render)
;
; Method 2: Set parameters individually
;   (setq B 2000.0)
;   (setq H 1500.0)
;   (setq W 1200.0)
;   (setq a 200.0)
;   (setq d 300.0)
;   (setq t_param 200.0)
;   (setq t t_param)  ; Use t_param to avoid conflict with AutoLISP 't'
;   (c:PCAD_Render)
;
; Method 3: Use default parameters
;   (c:PCAD_Render)  ; Uses default values if not set
