# Structural Examples

Reference examples for sketches, regions, mesh, and bars in P-CAD.

---

## Basic Section Outline

```pcad
sketch Section_A layer=outline {
    polyline outer closed {
        (0,0) -> (B,0) -> (B,H) -> (0,H);
    }
}

region Concrete {
    boundary = Section_A.outer;
    hatch = concrete;
}
```

---

## U-Channel Cross-Section with Arc Curve

A complete parametric U-channel with inner arc curve:

```pcad
units mm;

params {
    B = 1600;      // Bottom width
    H = 1200;      // Total height
    C = 350;       // Edge vertical height
    a1 = 250;      // Top wall thickness
    Ri = 2000;     // Inner radius (2.0m)
    B_top = 2000;  // Top total width
}

derive {
    half_B = B / 2;
    half_B_top = B_top / 2;
    inner_x = half_B_top - a1;
    
    // Arc center calculation
    arc_center_y = H + sqrt(Ri * Ri - inner_x * inner_x);
    arc_bottom_y = arc_center_y - Ri;
    
    // Intermediate points for arc approximation
    x_mid = inner_x * 0.6;
    y_mid = arc_center_y - sqrt(Ri * Ri - x_mid * x_mid);
}

layers {
    outline: #00FFFF, 0.25;
    hatch:   #B4B4B4, 0.10;
    dim:     #00FFFF, 0.18;
}

sketch U_Section layer=outline {
    polyline outer closed {
        (-half_B, 0) -> 
        (half_B, 0) -> 
        (half_B, C) -> 
        (half_B_top, H) -> 
        (inner_x, H) -> 
        (x_mid, y_mid) ->        // Arc approximation
        (0, arc_bottom_y) -> 
        (-x_mid, y_mid) ->       // Symmetric point
        (-inner_x, H) -> 
        (-half_B_top, H) -> 
        (-half_B, C);
    }
}

region ConcreteBody {
    boundary = U_Section.outer;
    hatch = concrete;
}

dim vertical {
    from = (-half_B_top - 100, 0);
    to = (-half_B_top - 100, H);
    text = "H";
}

dim horizontal {
    from = (-half_B, -150);
    to = (half_B, -150);
    text = "B";
}

dim radial {
    center = (0, arc_center_y);
    radius = Ri;
    text = "R=2.0";
}
```

> **Note**: Use `sqrt()` in `derive` block for arc geometry calculations. Add intermediate points for smooth curve approximation.

---

## Sloped Wall Section

```pcad
params {
    L = 4000;
    h1 = 1600;
    h2 = 1200;
    cover = 50;
}

sketch SidewallOutline layer=outline {
    polyline outer closed {
        (0,0) -> (L, 0) -> (L, h2) -> (0, h1);
    }
}
```

---

## Mesh (Grid Reinforcement)

```pcad
mesh WebMesh layer=mesh {
    set = N6;
    region = InnerRegion;
    spacing_x = 120;
    spacing_y = 120;
    style = "grid";
    label = "N6@12";
}
```

---

## Bars (Linear Reinforcement)

```pcad
bars BaseBars layer=rebar {
    set = N12;
    path = line(cover,cover) -> line(L2-cover,cover);
    count = 2;
    spacing = 50;
    label = "N12×2";
}
```

---

## Bars as Dots (Section View)

For longitudinal bars perpendicular to the drawing plane:

```pcad
bars N3_纵筋 {
    set = N3;
    render_style = dots;
    dot_radius = 6;
    spacing_y = 20;
}
```

---

## Region with Hatch Islands

For sections with actual penetrations:

```pcad
region 侧墙剖面 {
    boundary = sketch.墙体外形;
    hatch = concrete;
    islands = [sketch.窗洞, sketch.管道套管];
    island_detection = explicit;
}
```

> **Note**: Islands are for real penetrations (pipes, openings), not for defining basic section shape.

---

## Dimension Annotations

```pcad
dim linear {
    from = (0, -200);
    to = (L, -200);
    text = "L=400CM";
}

dim vertical {
    from = (-200, 0);
    to = (-200, h1);
    text = "h1";
}
```

---

## Labels and Callouts

```pcad
label "A-A" at (500,1200) layer=text;

callout WebMesh {
    at = (600,200);
    leader = auto;
    text = WebMesh.label;
}
```

---

## Complete Structural View

```pcad
params {
    L = 4000;
    h1 = 1600;
    h2 = 1200;
    cover = 50;
}

derive {
    InnerL = cover;
    InnerR = L - cover;
}

layers {
    outline: #00FFFF, 0.5;
    rebar:   #FF0000, 0.3;
    dim:     #FFFFFF, 0.18;
    text:    #00FF00, 0.25;
}

sketch SidewallOutline layer=outline {
    polyline outer closed {
        (0,0) -> (L, 0) -> (L, h2) -> (0, h1);
    }
}

// Horizontal bars
sketch N1Bars layer=rebar {
    line N1_0 (InnerL, 50.0) -> (InnerR, 50.0);
    line N1_1 (InnerL, 216.7) -> (InnerR, 172.2);
    // ... more bars
}

dim linear {
    from = (0, -200);
    to = (L, -200);
    text = "L=400CM";
}

label "N1 分布钢筋" at (L/2, h1 + 100) layer=text;
```
