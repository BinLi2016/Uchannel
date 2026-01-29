# Advanced P-CAD Features

Reference for components, inheritance, drawing info, and other advanced features.

---

## Drawing Information Block

Add drawing-level metadata at the top of your P-CAD file:

```pcad
drawing_info {
    title = "U型槽钢筋布置图";
    subtitle = "U-Channel Rebar Layout";
    project = "某市政工程";
    drawing_no = "S-RC-001";
    revision = "A";
    date = "2026-01";
    scale = 1:50;
    units = cm;
    author = "设计员";
    checker = "校核员";
    status = for_review;
}
```

### Status Values

| Status | Description |
|--------|-------------|
| `draft` | Work in progress |
| `for_review` | Ready for review |
| `approved` | Approved for construction |
| `as_built` | As-built documentation |

---

## Reusable Components

Use `component` for recurring assemblies:

```pcad
component CoverPlate {
    sketch plate layer=outline {
        polyline outer closed {
            (0,0) -> (500,0) -> (500,50) -> (0,50);
        }
    }
    
    region CoverConcrete {
        boundary = plate.outer;
        hatch = concrete;
    }
    
    mesh CoverMesh layer=mesh {
        set = N6;
        region = CoverConcrete;
        spacing_x = 120;
        spacing_y = 120;
    }
}

// Place the component:
place component CoverPlate at (100, 200) scale = 1:1;
```

---

## Barshape Inheritance

Create bar shape variants using `inherit`:

```pcad
// Base shape
barshape S_N7 {
    set = N7;
    type = custom;
    segments = [(0,0) -> (L2-12,0) -> (L2-12,H) -> (0,H)];
    dims { W = L2 - 12; H = 100; }
}

// Variant with different dimension
barshape S_N7_1 {
    set = N7_1;
    inherit = S_N7;
    dims { W = (L2-12) - (B2-12); }
}
```

### Inheritance Rules

| Can Override | Cannot Override |
|--------------|-----------------|
| `dims` | `type` |
| `annotations` | Base segment count |
| `hooks` | |

---

## Symmetric Sketches

Define only half of a symmetric section:

```pcad
sketch 对称截面 layer=outline symmetric=vertical center_x=W/2 {
    // Define right half only
    polyline right_half {
        (W/2, 0) -> (W, 0) -> (W, H) -> (W/2, H);
    }
}
// Left half is automatically mirrored
```

### Axis Values

| Axis | Description |
|------|-------------|
| `vertical` | Mirror across Y axis |
| `horizontal` | Mirror across X axis |
| `both` | Quarter symmetry |

---

## Mirror Block

Explicit mirroring control:

```pcad
sketch 右半侧壁 layer=outline {
    polyline p { (50,0) -> (100,0) -> (100,80) -> (50,80); }
}

mirror 左半侧壁 {
    source = 右半侧壁;
    axis = vertical;
    center = (50, 0);
    include_original = true;
}
```

---

## Section Markers

```pcad
section_marker A {
    from = (x1, y1);
    to = (x2, y2);
    label = "A-A";
    direction = left;
    style = arrow;
    target_view = V_A_A;
}
```

---

## Detail Markers

```pcad
detail_marker D1 {
    center = (x, y);
    radius = 15;
    label = "1";
    scale = 1:10;
    target_view = V_Detail_1;
}
```

---

## Slope Annotations

```pcad
slope SideTaper {
    on = (top_x, top_y) -> (bottom_x, bottom_y);
    type = ratio;
    value = 25;
    label = "25:1";
}
```

### Slope Types

| Type | Example |
|------|---------|
| `ratio` | 25:1 |
| `percentage` | 2% |
| `grade` | 1:50 |

---

## Chamfer and Fillet Notation

```pcad
chamfer TopCorner {
    type = chamfer;
    at = 框架涵截面.inner.corner(2);
    dim1 = 10;
    dim2 = 10;
    notation = "C10";
}

chamfer BottomFillet {
    type = fillet;
    at = (a2, slab_t);
    dim1 = 10;
    notation = "R10";
}
```

---

## Extended Dimensions

### Dimension Chain

```pcad
dim_chain horizontal {
    baseline_y = H + 15;
    points = [(0, H), (a2, H), (W - a2, H), (W, H)];
    labels = ["a₂", "内宽", "a₂"];
}
```

### Cumulative Dimensions

```pcad
dim_cumulative horizontal {
    origin = (0, 0);
    points = [(50, 0), (100, 0), (200, 0)];
    offset = -20;
}
```

### Angular Dimensions

```pcad
dim angular {
    center = (100, 50);
    from_angle = 0;
    to_angle = 45;
    radius = 30;
    text = "45°";
}
```
