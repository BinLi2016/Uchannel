# Materials and Rebar Sets

Reference for steel grades, rebar specifications, and non-rebar components.

---

## Materials Block

Define material grades and properties:

```pcad
materials {
    steel {
        HPB300 { fy = 300; density = 7850; }
        HRB400 { fy = 400; density = 7850; }
        Q235   { fy = 235; density = 7850; }
    }
    concrete {
        C30 { }
        C35 { }
        C40 { }
    }
}
```

---

## Rebar Set

Define rebar specifications with optional grade and weight:

```pcad
rebar_set N6  { dia = 6;  grade = HPB300; weight_per_m = 0.222; }
rebar_set N8  { dia = 8;  grade = HPB300; weight_per_m = 0.395; }
rebar_set N12 { dia = 12; grade = HRB400; weight_per_m = 0.888; }
rebar_set N16 { dia = 16; grade = HRB400; weight_per_m = 1.58; }
```

### Properties

| Property | Description | Required |
|----------|-------------|----------|
| `dia` | Diameter in mm | Yes |
| `grade` | Steel grade (HPB300, HRB400) | No |
| `weight_per_m` | Weight per meter (kg/m) | No |

---

## Rebar Spec in Tables

Use `rebar_spec` column type for automatic parsing:

```pcad
columns {
    规格: rebar_spec;
}

// Supported formats:
// Φ12         => dia=12, grade=unknown
// HPB300-Φ8   => dia=8, grade=HPB300
// HRB400-Φ16  => dia=16, grade=HRB400
```

Access properties in expressions:
- `规格.dia` - diameter
- `规格.grade` - steel grade

---

## Item Sets (Non-Rebar Components)

For angle steel, plates, and other non-rebar items:

```pcad
item_set AngleSteel_L25 {
    type = angle_steel;
    spec = "L25x25x3";
    material = steel.Q235;
    weight_per_m = 0.57;
}

item_set CoverSlab {
    type = concrete_member;
    material = concrete.C35;
}

item_set EmbeddedPlate {
    type = plate;
    spec = "200x200x10";
    material = steel.Q235;
}
```

### Item Types

| Type | Description |
|------|-------------|
| `angle_steel` | Angle steel (角钢) |
| `plate` | Steel plate (钢板) |
| `embedded` | Embedded items (预埋件) |
| `concrete_member` | Concrete components |
| `custom` | Other items |

---

## Standard Rebar Weights

| Diameter (mm) | Weight (kg/m) |
|---------------|---------------|
| Φ6 | 0.222 |
| Φ8 | 0.395 |
| Φ10 | 0.617 |
| Φ12 | 0.888 |
| Φ14 | 1.21 |
| Φ16 | 1.58 |
| Φ18 | 2.00 |
| Φ20 | 2.47 |
| Φ22 | 2.98 |
| Φ25 | 3.85 |

---

## Standard Bend Radii

| Bar Diameter | Min Bend Radius (HPB300) | Min Bend Radius (HRB400) |
|--------------|--------------------------|--------------------------|
| ≤ 16mm | 2.5d | 4d |
| 16-25mm | 4d | 5d |
| > 25mm | 5d | 6d |
