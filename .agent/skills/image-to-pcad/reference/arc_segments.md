# Arc Segments and Fillet Radius

Reference for curved lines in P-CAD.

---

## Two Modes of Operation

P-CAD handles curves in two ways depending on the block type:

1.  **Corner Fillets** (`barshape`): Rounds the intersection corner between two segments.
2.  **Arc Segments** (`sketch`): Defines the segment itself as a curve.

---

## 1. Corner Fillets (Barshapes)

Used primarily in `barshape` blocks to represent bent rebar corners.

### Syntax
```
(x, y):r=<radius>
```
The point `(x, y)` is the **Virtual Intersection Point** (where the straight segments would meet).

### Behavior
- The `r` value is the bend radius.
- The transpiler uses the AutoCAD `FILLET` command.
- The curve starts *before* the point and ends *after* it.

---

## 2. Arc Segments (Sketches)

Used in `sketch` blocks (polylines) to define complex geometric boundaries.

### Syntax
```
(x, y):r=<radius>
```
The point `(x, y)` is the **End Point** of the arc segment. The arc originates from the *previous* vertex.

### Behavior
- **Radius**: `r` defines the radius of the arc connecting `Previous_Point` to `Current_Point`.
- **Direction**:
    - **Positive (+R)**: Counter-Clockwise (CCW) arc.
    - **Negative (-R)**: Clockwise (CW) arc.
- **AutoCAD Mapping**: Uses `LWPOLYLINE` bulge values (Group Code 42).

### Example: U-Channel

```pcad
sketch U_Section {
    polyline boundary closed {
        // ... previous points ...
        (W_inner/2, H) ->             // Start of arc
        (0, y_bot):r=-R ->            // End of arc (CW direction towards bottom)
        (-W_inner/2, H):r=-R ->       // End of next arc (CW direction towards top-left)
        // ... next points ...
    }
}
```

---

## Geometry Semantics

| Feature | Corner Fillet (`barshape`) | Arc Segment (`sketch`) |
| :--- | :--- | :--- |
| **Point (x,y)** | Virtual Interaction Corner | Actual Segment End Point |
| **Radius** | Fillet Radius | Segment Radius |
| **Geometry** | Line -> Arc -> Line | Arc (from Prev to Curr) |
| **Sign** | Always Positive | + = CCW, - = CW |
| **CAD Implementation** | `FILLET` command | `LWPOLYLINE` Bulge |

---

## CAD Output Mapping

| P-CAD | DXF/DWG |
|-------|---------|
| `sketch` Point `:r=R` | `LWPOLYLINE` vertex with calculated `bulge` |
| `barshape` Point `:r=R` | `LINE` / `FILLET` / `LINE` sequence |
