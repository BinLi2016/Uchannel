---
name: image-to-pcad
description: Guide for converting rebar detail images into P-CAD parametric code.
---

# Image-to-P-CAD Conversion Guide

## Purpose
This guide provides instructions for converting a wide variety of reinforced concrete (RC) structure drawings into P-CAD code. This includes:
- **Rebar Details (钢筋大样)**: Individual bar bending diagrams and dimensions.
- **Structural Layouts (结构布置图)**: Sections, plans, and elevations showing concrete outlines and rebar placement.
- **Engineering Tables (工程表格)**: Parameter lookup tables, reinforcement quantity tables, and material summaries.

---

## Step 0: Image Classification

Identify which components are present in the image and map them to P-CAD blocks:

| Drawing Component | P-CAD Block | Best Practice |
|-------------------|-------------|---------------|
| Concrete Outlines | `sketch`, `region` | Use `polyline closed` for sections. |
| Rebar in Section | `bars`, `mesh` | Define `rebar_set` first. |
| Detail Diagrams | `barshape` | Use local origins and parametric dims. |
| Lookup Data | `table type=lookup` | Capture design parameters (H1, B2, etc.). |
| Quantity Schedules | `table type=schedule` | Link to `barshape_ref` for visual schedules. |
| Overall Layout | `sheet`, `view` | Coordinate multiple views and tables. |

---

## Step 1: Structural Analysis (Layouts & Sections)

### 1.1 Capture Concrete Geometry
Identify the main parametric dimensions (e.g., wall thickness, height, width).
- Use `sketch` for the line work.
- Use `region` + `hatch` for filled concrete areas.

### 1.2 Identify Reinforcement Layers
- **Primary Rebar**: Typically shown as thick lines or dots. Map to `bars`.
- **Distribution Rebar/Mesh**: Map to `mesh`.
- **Labels**: Capture labels like `N1-Φ12@200` and map them to `label` or `callout`.

---

## Step 2: Rebar Detail Analysis (大样图)

### 2.1 Extract Shape Information
For each rebar shape, identify:
- **ID & Spec**: e.g., N1, Φ16.
- **Segment Pattern**: The path (x1,y1) -> (x2,y2).
- **Variable Dims**: Annotations like `H3+50` or `B2-12`.

### 2.2 Local Coordinate Setup
- Establish a local origin for each `barshape`.
- Normalize coordinates relative to that origin.

---

## Step 3: Parameter & Table Analysis

### 3.1 Extract Parameters
If the image is a **Parameter Table (参数表)**:
- Define `params` for the baseline values.
- Define a `table type=lookup` to hold the variations.

### 3.2 Extract Formulas
Look for derivation rules in annotations (e.g., "length = H + 2*45") and map them to `derive` blocks.

---

## Step 4: P-CAD Functional Examples

### 4.1 Structural View (Section)
```pcad
sketch Section_A layer=outline {
    polyline outer closed { (0,0) -> (B,0) -> (B,H) -> (0,H); }
}
region Concrete { boundary = Section_A.outer; hatch = concrete; }
bars MainSteel layer=rebar {
    set = N12;
    path = line(cover, cover) -> line(B-cover, cover);
    spacing = 200;
}
```

### 4.2 Rebar Detail
```pcad
barshape N1 {
    type = custom;
    dims { L = H3 + 50; }
    segments = [(0, L) -> (0, 0) -> (45, -45)];
}
```

### 4.3 Parameter Table
```pcad
table UChannelParams {
    type = lookup;
    key = Size;
    columns { Size: string; H: number; B: number; }
    row YU1 { Size="YU1"; H=1000; B=500; }
}
```

---

## Step 5: Best Practices for Versatility

1.  **Parametric First**: Never hardcode dimensions if you see a variable (H, L, B) in the image.
2.  **Semantic Layering**: Use `outline` for concrete, `rebar` for steel, `text` for labels.
3.  **Modular Components**: Use `component` for recurring elements like cover slabs or typical joints.
4.  **Scaling**: Remember that the transpiler scales `barshape` in tables automatically; keep `barshape` definitions at "real world" scale (mm).

---

## Verification Checklist
- [ ] Does the `sketch` represent all external boundaries?
- [ ] Are all rebars accounted for in `bars`, `mesh`, or `barshape`?
- [ ] Are variables (H1, B2) used instead of hardcoded numbers?
- [ ] Does the `table` match the provided schedule in the image?
- [ ] Are layers and colors consistent with the P-CAD-DSL standard?

---

## Step 1: Image Analysis

### 1.1 Identify the Layout Type
- **Grid Layout**: Shapes arranged in rows and columns (most common for 大样图)
- **Freeform Layout**: Shapes placed at specific positions

### 1.2 Extract Shape Information
For each rebar shape in the image, identify:

| Property | Description | Example |
|----------|-------------|---------|
| **ID** | Shape identifier | N1, N2, N3... |
| **Rebar Spec** | Diameter and grade | Φ12, Φ16 |
| **Shape Type** | Geometry classification | L_bend, U_bend, stirrup, custom |
| **Key Dimensions** | Annotated measurements | 21.6, 45.3, H3+50 |
| **Segment Pattern** | Point-to-point path | (0,0) -> (a,0) -> (a,b) |
| **Hooks** | End treatments | 90° hook, 135° hook |

### 1.3 Identify Parameters
Look for:
- **Fixed values**: Constant dimensions (21.6, 45.3)
- **Variable expressions**: Parametric references (H3+50, B2-12)
- **Derived values**: Computed from other parameters

---

## Step 2: Coordinate System Setup

### 2.1 Establish Local Origin
Each barshape uses a **local coordinate system** with origin at a logical point:
- For L-bends: Origin at the corner
- For straight bars: Origin at one end
- For stirrups: Origin at bottom-left

### 2.2 Normalize Segment Points
Express all coordinates relative to the local origin:
```
segments = [(x1, y1) -> (x2, y2) -> ... -> (xn, yn)];
```

**Tip**: Use the first annotated dimension as reference and build from there.

---

## Step 3: P-CAD Code Structure

### 3.1 Required Blocks

```pcad
// 1. Parameters (if any variable dimensions)
params {
    H1 = 1500;
    H3 = 1000;
    B2 = 800;
}

// 2. Derived values (computed from params)
derive {
    N1_H = H3 + 50;
    N3_H = B2 - 12;
}

// 3. Layers
layers {
    outline: #00FFFF, 0.25;
    rebar: #FF0000, 0.20;
    text: #00FF00, 0.18;
    dim: #00FFFF, 0.18;
}

// 4. Rebar sets (define specifications)
rebar_set R12 { dia = 12; grade = HRB400; }
rebar_set R16 { dia = 16; grade = HRB400; }

// 5. Barshape definitions (one per shape in image)
barshape N1 {
    type = custom;
    dims { H = N1_H; }
    segments = [...];
}

// 6. Table (for schedule/summary)
table RebarDetailsTable {
    type = schedule;
    columns { ... }
    row { ... }
}

// 7. Sheet (layout and placement)
sheet RebarDetailsSheet {
    size = A3;
    place table RebarDetailsTable;
}
```

---

## Step 4: Barshape Segment Patterns

### 4.1 Common Shape Types

#### Straight Bar
```pcad
barshape N_straight {
    type = straight;
    segments = [(0, 0) -> (L, 0)];
    dims { L = 500; }
}
```

#### L-Bend (with hook)
```pcad
barshape N_Lbend {
    type = L_bend;
    // Horizontal segment with 45° bend up, then vertical
    segments = [
        (hook_len, 0) ->    // Hook start
        (0, 0) ->           // Corner
        (0, H)              // Vertical rise
    ];
    dims { H = H3 + 50; hook_len = 21.6; }
}
```

#### L-Bend with 45° Transition
```pcad
barshape N_L45 {
    type = custom;
    segments = [
        (hook_len, 0) ->          // Start with hook
        (0, 0) ->                 // First corner
        (0, H - bend45) ->        // Before 45° bend
        (bend45, H) ->            // After 45° bend
        (bend45 + tail, H)        // End
    ];
    dims { H = N1_H; hook_len = 21.6; bend45 = 45.3; tail = 30; }
}
```

#### U-Bend (Stirrup-like)
```pcad
barshape N_stirrup {
    type = custom;
    segments = [
        (hook, H) ->
        (0, H) ->
        (0, 0) ->
        (W, 0) ->
        (W, H) ->
        (W - hook, H)
    ];
    dims { W = B2 - 12; H = 122; hook = 16.2; }
}
```

#### Trapezoidal Shape
```pcad
barshape N_trapezoid {
    type = custom;
    // Top segment shorter than bottom
    segments = [
        (0, 0) ->           // Bottom-left
        (W_bottom, 0) ->    // Bottom-right
        (W_top + offset, H) -> // Top-right
        (offset, H) ->      // Top-left
        (0, 0)              // Close
    ];
    dims { W_bottom = L2 - 12; W_top = W_bottom; H = B1; offset = 0; }
}
```

#### Slanted Shape (Parallelogram)
```pcad
barshape N_slant {
    type = custom;
    // Slanted sides
    segments = [
        (0, 0) ->
        (W, 0) ->
        (W + slant, H) ->
        (slant, H) ->
        (0, 0)
    ];
    dims { W = L2 - 12; H = L3 + L4 - 20; slant = H * 0.176; }
}
```

---

## Step 5: Dimension Annotations

### 5.1 Identifying Dimension Locations
In the reference image, dimensions are typically placed:
- **Horizontal dims**: Below or beside horizontal segments
- **Vertical dims**: To the left or right of vertical segments
- **Diagonal dims**: Parallel to the segment

### 5.2 P-CAD Dimension Syntax (for future enhancement)
```pcad
barshape N1 {
    segments = [...];
    dims { H = N1_H; }
    
    // Dimension annotations (proposed enhancement)
    annotations {
        dim linear { from = (0, 0); to = (21.6, 0); text = "21.6"; }
        dim linear { from = (0, 0); to = (0, H); text = "H3+50"; }
    }
}
```

---

## Step 6: Table Definition

### 6.1 Schedule Table Structure
```pcad
table RebarDetailsTable {
    type = schedule;
    
    columns {
        编号: string;           // ID column
        规格: string;           // Rebar spec (Φ12, Φ16)
        大样: barshape_ref;     // Shape reference (renders the shape)
        备注: string;           // Remarks (variable expression)
    }
    
    // One row per shape
    row { 编号 = "N1"; 规格 = "Φ16"; 大样 = "N1"; 备注 = "H3+50"; }
    row { 编号 = "N2"; 规格 = "Φ12"; 大样 = "N2"; 备注 = "B2-12"; }
    // ... more rows
}
```

### 6.2 Layout Configuration
```pcad
table RebarDetailsTable {
    // ...columns and rows...
    
    layout {
        cell_width = 60;     // Width per column in mm
        cell_height = 80;    // Height for barshape cells
        header_height = 15;  // Header row height
    }
}
```

---

## Step 7: Best Practices

### 7.1 Naming Conventions
- **Barshapes**: Use IDs from the image (N1, N2, ...)
- **Parameters**: Use descriptive names matching drawing conventions (H1, B2, L3)
- **Derived values**: Prefix with shape ID (N1_H, N3_W)

### 7.2 Coordinate Consistency
- Always use the same origin convention within a shape
- Prefer origin at logical starting point of rebar

### 7.3 Expression Handling
- Quote string expressions: `dims { H = "H3+50"; }`
- Keep numeric values unquoted: `dims { hook = 21.6; }`

### 7.4 Parameterization Strategy
- Extract only dimensions that change with design parameters
- Keep construction dimensions (hooks, bends) as constants

---

## Example: Complete P-CAD File

```pcad
// ===========================================
// Rebar Details - Set 2
// Based on: DrawingShot\钢筋大样-2.png
// ===========================================

params {
    H1 = 1500;
    H3 = 1000;
    B1 = 500;
    B2 = 800;
    L2 = 600;
    L3 = 400;
    L4 = 300;
}

derive {
    N1_H = H3 + 50;
    N3_H = B2 - 12;
    N6_H = H1 + 30;
    N4_W = L2 - 12;
    N7_H = L4 + L3 - 20;
    N7_W = L2 - 12;
}

layers {
    outline: #00FFFF, 0.25;
    rebar: #FF0000, 0.20;
    text: #00FF00, 0.18;
}

// N1: L-bend with 45° transition (Φ16)
barshape N1 {
    type = custom;
    dims { H = N1_H; }
    segments = [
        (21.6, N1_H + 45.3) ->
        (0, N1_H + 45.3) ->
        (0, 45.3) ->
        (45.3, 0) ->
        (75.3, 0)
    ];
}

// N3: U-shape stirrup (Φ12)
barshape N3 {
    type = custom;
    dims { H = N3_H; }
    segments = [
        (16.2, N3_H) ->
        (0, N3_H) ->
        (0, 0) ->
        (16.2, 0)
    ];
}

// Table
table RebarDetailsTable2 {
    type = schedule;
    columns {
        编号: string;
        规格: string;
        大样: barshape_ref;
        备注: string;
    }
    row { 编号 = "N1"; 规格 = "Φ16"; 大样 = "N1"; 备注 = "H3+50"; }
    row { 编号 = "N3"; 规格 = "Φ12"; 大样 = "N3"; 备注 = "B2-12"; }
}

sheet RebarDetailsSheet2 {
    size = A3;
    scale = 1:1;
    place table RebarDetailsTable2;
}
```

---

## Verification Checklist

After generating P-CAD code, verify:

- [ ] All shapes from the image are defined as `barshape` blocks
- [ ] Segment paths correctly trace the shape geometry
- [ ] Parameters and derived values match the image annotations
- [ ] Table includes all shapes with correct specifications
- [ ] Layers are properly defined
- [ ] File transpiles without errors
- [ ] AutoCAD rendering matches the reference image layout
