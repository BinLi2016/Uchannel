# Arc Segments and Fillet Radius

Reference for curved corners in P-CAD polylines and barshapes.

---

## Syntax

Use the `:r=<radius>` suffix on **virtual intersection points**:

```
(x, y):r=<radius>
```

The actual arc is computed as a tangent fillet between adjacent segments.

---

## Virtual Intersection Points

The point `(x, y)` represents where adjacent straight segments **would meet if extended**.
The arc is then calculated to smoothly connect the two segments with the given radius.

```
        Actual path with fillet
              ╭──────
             ╱
            ╱
           ╱
──────────○ ← Virtual intersection point (x, y):r=R
```

---

## Examples

### Polyline with Rounded Corners

```pcad
sketch Stadium layer=outline {
    polyline boundary closed {
        (0, 0):r=R -> (W, 0):r=R -> (W, H):r=R -> (0, H):r=R;
    }
}
```

### Barshape with Mixed Radii

```pcad
barshape N_UBar {
    type = custom;
    segments = [
        (0, H) -> 
        (x1, H):r=R1 ->     // Upper radius
        (x2, 0):r=R2 ->     // Lower radius
        (x3, 0):r=R2 -> 
        (x4, H):r=R1 -> 
        (x5, H)
    ];
}
```

### Stadium Shape (Semicircular Ends)

When `R = H/2`, you get perfect semicircular ends:

```pcad
params {
    W = 770;
    H = 220;
    R = 110;  // R = H/2
}

barshape StadiumStirrup {
    type = stirrup;
    segments = [
        (0, 0):r=R -> (W, 0):r=R 
        -> (W, H):r=R -> (0, H):r=R
    ];
}
```

---

## Geometry Semantics

Given virtual intersection point P with adjacent segments S1 (incoming) and S2 (outgoing):

1. Calculate tangent point T1 on S1 at distance R from P
2. Calculate tangent point T2 on S2 at distance R from P
3. The arc from T1 to T2 is tangent to both segments

| Corner Angle | Tangent Distance |
|--------------|------------------|
| 90° | R |
| Other θ | R × tan(θ/2) |

---

## CAD Output Mapping

| P-CAD | DXF/DWG |
|-------|---------|
| Point with `:r=R` | LWPOLYLINE vertex with bulge |
| Semicircle (180°) | bulge = ±1.0 |
| Quarter circle (90°) | bulge ≈ ±0.414 |
