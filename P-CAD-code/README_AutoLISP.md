# P-CAD to AutoLISP 使用说明

## 概述

`pcad_to_autolisp.py` 将 P-CAD 文件转换为 AutoLISP 脚本，使用 **Approach 4: 全局变量参数注入**。

## 生成 AutoLISP 文件

```bash
python Python\pcad_to_autolisp.py "P-CAD-code\U形槽混凝土断面.pcad" "P-CAD-code\U形槽混凝土断面.lsp"
```

## 在 AutoCAD 中使用

### 方法 1: 使用 set-params 函数（推荐）

```lisp
; 加载脚本
(load "H:/U形槽配筋软件/P-CAD-code/U形槽混凝土断面.lsp")

; 设置参数: (set-params B H W a d t)
(set-params 2000 1500 1200 200 300 200)

; 渲染
(c:PCAD_Render)
```

### 方法 2: 单独设置全局变量

```lisp
; 加载脚本
(load "H:/U形槽配筋软件/P-CAD-code/U形槽混凝土断面.lsp")

; 单独设置每个参数
(setq B 2000.0)
(setq H 1500.0)
(setq W 1200.0)
(setq a 200.0)
(setq d 300.0)
(setq t_param 200.0)  ; 注意: 使用 t_param 而不是 t (t 是 AutoLISP 保留字)
(setq t t_param)

; 渲染
(c:PCAD_Render)
```

### 方法 3: 使用默认参数

```lisp
; 加载脚本
(load "H:/U形槽配筋软件/P-CAD-code/U形槽混凝土断面.lsp")

; 直接渲染（使用 P-CAD 文件中定义的默认值）
(c:PCAD_Render)
```

## 参数说明

| 参数 | 说明 | 默认值 (mm) |
|------|------|-------------|
| B | 底宽 | 2000 |
| H | 槽内净高 | 1500 |
| W | 槽内宽 | 1200 |
| a | 顶翼缘宽(单侧) | 200 |
| d | 外侧底厚(肩高) | 300 |
| t | 底板厚度 | 200 |

**注意**: 参数 `t` 在 AutoLISP 中是保留字（表示逻辑真值），因此代码中使用 `t_param` 作为内部变量名，然后赋值给 `t`。

## 生成的实体

生成的 AutoLISP 脚本会创建：

1. **图层**:
   - `outline`: 轮廓线（青色，线宽 0.25）
   - `hatch`: 填充（灰色，线宽 0.10）
   - `dim`: 尺寸标注（青色，线宽 0.18）
   - `text`: 文字（绿色，线宽 0.18）

2. **几何实体**:
   - 闭合多段线（LWPOLYLINE）：U 形槽轮廓
   - 填充（HATCH）：混凝土剖面线（ANSI37）
   - 尺寸标注（DIMLINEAR）：B, W, a, H, d, t

## 示例：批量生成不同参数

```lisp
; 加载脚本
(load "H:/U形槽配筋软件/P-CAD-code/U形槽混凝土断面.lsp")

; 生成多个不同参数的断面
(set-params 2000 1500 1200 200 300 200)
(c:PCAD_Render)

; 移动到新位置
(command "._MOVE" "ALL" "" (list 0 0 0) (list 3000 0 0))

; 生成另一个断面
(set-params 2500 1800 1500 250 350 250)
(c:PCAD_Render)
```

## 故障排除

### 问题 1: 表达式解析错误

如果生成的 AutoLISP 代码中表达式不正确（如 `x_wing_left` 或 `x_wing_right`），可以手动修复：

```lisp
; 错误的（可能由解析器生成）:
(setq x_wing_left (/ (- B W) (- 2 a)))

; 正确的:
(setq x_wing_left (- (/ (- B W) 2.0) a))
(setq x_wing_right (+ (/ (+ B W) 2.0) a))
```

### 问题 2: 参数未定义

如果出现 "参数未定义" 错误，确保在调用 `(c:PCAD_Render)` 之前设置了所有参数。

### 问题 3: 图层颜色不正确

如果图层颜色不符合预期，可以在 AutoCAD 中手动调整：

```lisp
(command "._LAYER" "_S" "outline" "")
(command "._COLOR" "4")  ; 青色
```

## 技术细节

- **参数注入方式**: 全局变量（Approach 4）
- **表达式求值**: 在 AutoLISP 运行时求值
- **坐标系统**: 2D (Z=0)
- **单位**: 毫米 (mm)

## 相关文件

- `pcad_to_autolisp.py`: P-CAD 到 AutoLISP 转换器
- `U形槽混凝土断面.pcad`: P-CAD 源文件
- `U形槽混凝土断面.lsp`: 生成的 AutoLISP 脚本
