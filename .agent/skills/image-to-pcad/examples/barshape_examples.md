# Barshape Examples

Reference examples for common rebar shape patterns in P-CAD.

---

## Straight Bar

```pcad
barshape N_straight {
    type = straight;
    segments = [(0, 0) -> (L, 0)];
    dims { L = 500; }
}
```

---

## L-Bend (with hook)

```pcad
derive { N1_H = H3 + 50; }

barshape N_Lbend {
    type = L_bend;
    segments = [
        (hook_len, 0) ->    // Hook start
        (0, 0) ->           // Corner
        (0, N1_H)           // Vertical rise
    ];
    dims { H = N1_H; hook_len = 21.6; }
}
```

---

## L-Bend with 45° Transition

```pcad
derive { N1_H = H3 + 50; }

barshape N_L45 {
    type = custom;
    segments = [
        (-21.6, N1_H) ->       // Hook extends left
        (0, N1_H) ->           // Top of vertical bar
        (0, 45.3) ->           // Before 45° bend
        (45.3, 0) ->           // After 45° bend
        (75.3, 0)              // Horizontal tail
    ];
    dims { H = N1_H; }
}
```

---

## U-Bend / Stirrup

```pcad
derive { N3_H = B2 - 12; }

barshape N_stirrup {
    type = custom;
    segments = [
        (hook, N3_H) ->
        (0, N3_H) ->
        (0, 0) ->
        (W, 0) ->
        (W, N3_H) ->
        (W - hook, N3_H)
    ];
    dims { W = B2 - 12; H = N3_H; hook = 16.2; }
}
```

---

## Hat-Shape / U-Bar (with Arc Segments)

Uses `:r=<radius>` for curved corners:

```pcad
params {
    L_flange = 250;
    H = 400;
    W_slope = 150;
    L_bottom = 600;
    R1 = 125;   // Upper shoulder radius
    R2 = 160;   // Lower corner radius
}

derive {
    x1 = L_flange;
    x2 = x1 + W_slope;
    x3 = x2 + L_bottom;
    x4 = x3 + W_slope;
}

barshape N_UBar {
    type = custom;
    segments = [
        (0, H) -> 
        (x1, H):r=R1 -> 
        (x2, 0):r=R2 -> 
        (x3, 0):r=R2 -> 
        (x4, H):r=R1 -> 
        (x4 + L_flange, H)
    ];
}
```

---

## Trapezoidal Shape

```pcad
barshape N_trapezoid {
    type = custom;
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

---

## Closed Stirrup with Hooks

```pcad
barshape S3 {
    type = stirrup;
    segments = [(0,0) -> (a,0) -> (a,b) -> (0,b) -> (0,0)];
    bend_radius = [r, r, r, r];
    hooks {
        start = hook(angle = 135, length = 50);
        end   = hook(angle = 135, length = 50);
    }
    dims { a = 120; b = 200; r = 8; }
}
```

---

## Barshape Variant (Inheritance)

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

---

## Slanted Bar / Parallelogram (with Trig)

Used for shapes with specific angles (e.g., 80° corner) or slanted edges.

```pcad
params {
    W_base = 212;
    H_side = 410;
}

derive {
    W = W_base - 12;
    S = H_side - 20;
    // 10 degree tilt results in 80 degree corner (90-10=80)
    dx = S * sin(10);
    dy = S * cos(10);
}

barshape N_slanted {
    type = custom;
    segments = [
        (-17, 0) ->          // Bottom-left tail
        (W, 0) ->            // Bottom edge
        (W + dx, dy) ->      // Right slanted side
        (W + dx + 12.5, dy) -> // Top-right tail
        (dx, dy) ->          // Top edge
        (0, 0)               // Close to left side
    ];
}
```

---

## Coordinate System Conventions

| Shape Type | Origin Placement | Segment Direction |
|------------|------------------|-------------------|
| L-bend | Corner junction | Hook in -X, main bar in +Y |
| U-shape | Bottom-left of opening | Counter-clockwise |
| Rectangle/Stirrup | Bottom-left corner | Counter-clockwise (closed) |
| Trapezoid | Center of top edge | As needed |
