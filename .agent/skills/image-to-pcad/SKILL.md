---
name: image-to-pcad
description: Guide for converting rebar detail images into P-CAD parametric code.
---

# Image-to-P-CAD Conversion Guide

Convert reinforced concrete drawings into P-CAD parametric code.

**Specification**: [P-CAD-DSL.md](file:///f:/Uchannel/P-CAD-Specification/P-CAD-DSL.md)

---

## Assumptions & Interpretations

> [!IMPORTANT]
> **Geometric Curves**: 
> - For **Sketches** (concrete outlines): Use Arc Segments with `:r=<radius>` syntax. Use **Positive R** for outward (convex) curves in CCW loops, **Negative R** for inward (concave).
> - For **Rebar** (barshapes): Use Corner Fillets with `:r=<radius>` syntax for bends.
> - See [arc_segments.md](reference/arc_segments.md) for detailed syntax and direction rules.
>
> **Hidden Lines**: Dashed lines should be interpreted as geometric guides or hidden boundaries.

---

## Quick Reference

| Drawing Component | P-CAD Block | Reference |
|-------------------|-------------|-----------|
| Concrete Outlines | `sketch`, `region` | [structural_examples.md](examples/structural_examples.md) |
| Rebar in Section | `bars`, `mesh` | [structural_examples.md](examples/structural_examples.md) |
| Detail Diagrams | `barshape` | [barshape_examples.md](examples/barshape_examples.md) |
| Curved Corners | `:r=<radius>` syntax | [arc_segments.md](reference/arc_segments.md) |
| Lookup Tables | `table type=lookup` | [table_examples.md](examples/table_examples.md) |
| Quantity Schedules | `table type=schedule` | [table_examples.md](examples/table_examples.md) |
| Materials & Grades | `materials`, `rebar_set` | [materials.md](reference/materials.md) |
| Reusable Assemblies | `component` | [advanced_features.md](reference/advanced_features.md) |

---

## Workflow

### Step 1: Classify the Image

Identify what components are present:
- **Rebar Details (钢筋大样)**: Bar bending diagrams → `barshape`
- **Structural Layouts**: Sections/elevations → `sketch` + `region` + `bars`/`mesh`
- **Parameter Tables**: Lookup data → `table type=lookup`
- **Schedules**: Quantity tables → `table type=schedule`

### Step 2: Extract Parameters

1. Identify **variable dimensions** (H1, B2, L3) → `params` block
2. Identify **computed values** (H3+50, B2-12) → `derive` block
3. Identify **fixed constants** (21.6, 45.3) → Use as literals

> [!NOTE]
> **Units**: Always use `units mm;` for AutoCAD drawings. If the source image uses cm or m, convert all values to mm (multiply by 10 for cm, 1000 for m). This ensures consistent scaling and correct dimension display in AutoCAD.

60: ### Step 3: Define Layers and Materials
61: 
62: > [!IMPORTANT]
63: > **Unit Normalization**: Hand-drawn sketches often mix units (cm, mm, m). 
64: > **Rule**: Standardize everything to **mm** in the `params` block immediately.
65: > - 400cm → `L = 4000;`
66: > - 2.5m → `H = 2500;`
67: 
68: ```pcad
69: layers {
70:     outline: #00FFFF, 0.25;
    rebar:   #FF0000, 0.20;
    text:    #00FF00, 0.18;
    dim:     #00FFFF, 0.18;
}

rebar_set N12 { dia = 12; grade = HRB400; }
rebar_set N16 { dia = 16; grade = HRB400; }
```

### Step 4: Create Geometry

Choose the appropriate block type:

| Need | Block | Key Properties |
|------|-------|----------------|
| Concrete outline | `sketch` | `polyline closed` |
| Filled section | `region` | `boundary`, `hatch`, `islands` |
| Grid reinforcement | `mesh` | `region`, `spacing_x`, `spacing_y` |
| Linear rebar | `bars` | `path`, `count`, `spacing` |
| Rebar diagram | `barshape` | `segments`, `dims` |

### Step 5: Add Tables/Layout

For **schedules with barshapes**:
```pcad
table RebarSchedule {
    type = schedule;
    columns { 编号: string; 规格: rebar_spec; 大样: barshape_ref; }
    row { 编号="N1"; 规格=Φ16; 大样="N1"; }
}
```

For **grid layout without borders**:
```pcad
barshape_layout Details {
    grid = 3x3;
    cell_size = (100, 150);
    place N1 at (0, 0) { 
        label = "N1 Φ16"; 
        
        // Advanced: Anchored Annotations
        annotations = [
            { text="Corner"; anchor=cell.top_left; offset=(5,-5); },
            { text="Center"; anchor=cell.center; offset=(0,0); }
        ];
    }
}
}
```


### Step 6: Add Dimensions

Use `dim` blocks to annotate geometry.

> [!TIP]
> **Align Dimension Lines**: To ensure professional alignment (baseline position), use the `offset` parameter.
> - **Horizontal (`dim linear`)**: `offset` is vertical shift (+ up, - down).
> - **Vertical (`dim vertical`)**: `offset` is horizontal shift (+ right, - left).
> - **Units**: You can use `h` units to scale offsets by text height (e.g., `offset = 3h`).
> - **Rule**: Use the same `offset` value (e.g., `-500` or `3.5h`) for all dimensions in the same row/column.

```pcad
// Example: Aligning bottom dimensions
// Example: Aligning bottom dimensions
dim linear { from=(0,0); to=(L1,0); offset=3h; text="2400"; }
dim linear { from=(L1,0); to=(L1+L2,0); offset=3h; }
```

### Step 7: Add Labels & Notes

Use `label` blocks for general notes or titles.

> [!TIP]
> **Relative Positioning**: Use `anchor` to attach labels to tables or other entities. `offset` supports `h` units.

```pcad
label "Notes" {
    text = "1. All dimensions in mm.\n2. Concrete grade C30.";
    anchor = RebarTable.bottom_left;  // Anchors to table corner
    offset = (0, -2.5h);              // 2.5 lines below table
}
```

---
107: 
108: ## Critical Rules
109: 
110: > [!CAUTION]
111: > **No Inline Math in Sketches**: To prevent parser hangs (regex backtracking), **never** write expressions inside `sketch` or `polyline` coordinates. 
112: > - ❌ `(L - cv, H - cv)` 
113: > - ✅ Compute `pt_x = L - cv; pt_y = H - cv;` in `derive`, then use `(pt_x, pt_y)`.
114: 
115: > [!IMPORTANT]
116: > **Semantic Interpretation**: 
117: > - Identify engineering keywords: "Fan" (`扇形`), "Var" (`变量`), or formulas like `@ (L-cv)/20`.
118: > - Do not just trace lines; use `derive` to generate the logic. For a "Fan", calculate the slope change per bar.
119: 
120: > [!WARNING]
121: > **Variable Scoping**: Only use `params`/`derive` values in segment expressions.
> Local `dims` variables cause `numberp: nil` errors!

```pcad
// ✅ CORRECT
derive { N1_H = H3 + 50; }
barshape N1 {
    segments = [(0, N1_H) -> (0, 0)];  // Uses derived value
}

// ❌ WRONG
barshape N1 {
    dims { H = H3 + 50; }
    segments = [(0, H) -> (0, 0)];     // ERROR: H not defined
}
```

> [!IMPORTANT]
> **Visual Proportions**: When setting default values in the `params` block, always try to match the aspect ratio and relative scale of the original image. For example, if the bar in the image is 4x taller than it is wide, set `H=800` and `W=200` as defaults rather than generic `1000/1000` values. This ensures the first render is visually representative.

> [!TIP]
> **Arc Segments**: Use `:r=<radius>` for curved corners in `barshape` blocks.
> See [arc_segments.md](reference/arc_segments.md) for details.

> [!TIP]
> **Arcs in Structural Sketches**: Sketch polylines **DO** support `:r=` syntax using `LWPOLYLINE` bulges.
> - **Convex (Outward) Arcs**: Use `:r=+R` (Positive) for standard CCW loops.
> - **Concave (Inward) Arcs**: Use `:r=-R` (Negative) for standard CCW loops.
> - **Note**: For complex curves not representable by simple circular arcs, use arc approximation.
> See [structural_examples.md](examples/structural_examples.md) for examples.

### Arc Approximation Technique

For circular arcs in structural sections (like U-channels), calculate intermediate points:

```pcad
params {
    Ri = 2000;      // Arc radius
    inner_x = 750;  // X-coordinate of arc endpoints
    H = 1200;       // Height at arc endpoints
}

derive {
    // Arc center is above the endpoints
    arc_center_y = H + sqrt(Ri * Ri - inner_x * inner_x);
    arc_bottom_y = arc_center_y - Ri;
    
    // Intermediate points for smooth approximation
    x_mid = inner_x * 0.6;
    y_mid = arc_center_y - sqrt(Ri * Ri - x_mid * x_mid);
}

sketch U_Section layer=outline {
    polyline outer closed {
        // ... start points ...
        (inner_x, H) -> 
        (x_mid, y_mid) ->      // Intermediate point
        (0, arc_bottom_y) ->   // Arc bottom
        (-x_mid, y_mid) ->     // Symmetric intermediate
        (-inner_x, H) ->
        // ... end points ...
    }
}
```

### Stadium / Capsule Pattern (Sketch)
**Rule**: In a `sketch`, `:r=R` defines the *incoming* segment as an arc.
- To draw an arc from A to B: Add `:r=R` to point **B**.
- To draw a line from B to C: Leave point **C** plain.

```pcad
polyline stadium closed {
    (0, H) ->             // Start 
    (W, H):r=R ->         // Line -> Arc (Target has :r)
    (W, 0) ->             // Right Arc -> Line (Target plain)
    (0, 0):r=R ->         // Line -> Arc (Target has :r)
}
```

---

## Verification Checklist

- [ ] All shapes from image defined as `barshape` blocks
- [ ] Segment paths correctly trace geometry
- [ ] Parameters from image captured in `params`/`derive`
- [ ] Segment expressions use only `params`/`derive` values
- [ ] Arc segments use `:r=<radius>` syntax for curves
- [ ] Curves in sketches use arc approximation (intermediate points)
- [ ] Hidden lines interpreted as geometric relations, not literal boundaries
- [ ] Table matches schedule in image
- [ ] Layers defined consistently
- [ ] Dimensions aligned using consistent `offset` values
- [ ] File transpiles without errors

---

## File Structure

```
.agent/skills/image-to-pcad/
├── SKILL.md              ← This file (workflow guide)
├── examples/
│   ├── barshape_examples.md
│   ├── table_examples.md
│   └── structural_examples.md
└── reference/
    ├── arc_segments.md
    ├── materials.md
    └── advanced_features.md
```

---

## Minimal Example

```pcad
units mm;

params {
    H3 = 1000;
    B2 = 800;
}

derive {
    N1_H = H3 + 50;
    N3_H = B2 - 12;
}

layers {
    rebar: #FF0000, 0.20;
}

barshape N1 {
    type = custom;
    segments = [
        (21.6, N1_H + 45.3) ->
        (0, N1_H + 45.3) ->
        (0, 45.3) ->
        (45.3, 0) ->
        (75.3, 0)
    ];
}

barshape N3 {
    type = custom;
    segments = [
        (16.2, N3_H) ->
        (0, N3_H) ->
        (0, 0) ->
        (16.2, 0)
    ];
}

barshape_layout Details {
    title = "Rebar Details";
    grid = 2x1;
    cell_size = (100, 150);
    place N1 at (0, 0) { label = "N1 Φ16"; note = "H3+50"; }
    place N3 at (1, 0) { label = "N3 Φ12"; note = "B2-12"; }
}
```
