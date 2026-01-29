# Table Examples

Reference examples for P-CAD table definitions.

---

## Parameter Lookup Table

```pcad
table UChannelParams {
    type = lookup;
    key = Size;
    
    columns {
        Size: string;
        H: number unit=mm;
        B: number unit=mm;
        cover: number unit=mm;
    }
    
    row YU1 { Size="YU1"; H=1000; B=500; cover=40; }
    row YU2 { Size="YU2"; H=1200; B=600; cover=40; }
    row YU3 { Size="YU3"; H=1500; B=800; cover=40; }
}

// Usage in derive block:
derive {
    current = table(UChannelParams, "YU3");
    H = current.H;
    B = current.B;
}
```

---

## Rebar Schedule Table

```pcad
table RebarSchedule {
    type = schedule;
    
    columns {
        编号: string;
        规格: rebar_spec;
        大样: barshape_ref;
        根数: integer;
        单根长: number unit=cm;
        总长: number unit=m computed;
        每米重: number unit=kg/m;
        总重: number unit=kg computed;
    }
    
    row { 编号="N1"; 规格=Φ16; 大样="N1"; 根数=10; 单根长=320; 每米重=1.58; }
    row { 编号="N2"; 规格=Φ12; 大样="N2"; 根数=20; 单根长=180; 每米重=0.888; }
    row { 编号="N3"; 规格=Φ12; 大样="N3"; 根数=8;  单根长=240; 每米重=0.888; }
    
    compute {
        总长 = 单根长 * 根数 / 100;
        总重 = 总长 * 每米重;
    }
    
    summary {
        钢筋合计 = sum(总重);
        HRB400钢筋 = sum(总重 where 规格.grade == HRB400);
    }
}
```

---

## Simple Details Table

```pcad
table RebarDetailsTable {
    type = schedule;
    
    columns {
        编号: string;           // ID column
        规格: string;           // Rebar spec (Φ12, Φ16)
        大样: barshape_ref;     // Shape reference (renders the shape)
        备注: string;           // Remarks (variable expression)
    }
    
    row { 编号 = "N1"; 规格 = "Φ16"; 大样 = "N1"; 备注 = "H3+50"; }
    row { 编号 = "N2"; 规格 = "Φ12"; 大样 = "N2"; 备注 = "B2-12"; }
    row { 编号 = "N3"; 规格 = "Φ12"; 大样 = "N3"; 备注 = "B2-12"; }
    
    layout {
        cell_width = 60;
        cell_height = 80;
        header_height = 15;
    }
}
```

---

## Multi-Row Header Table

```pcad
table 钢筋数量表 {
    type = schedule;
    
    columns {
        编号: integer;
        规格: rebar_spec;
        大样: barshape_ref;
        单根长: number unit=cm;
        总长: number unit=m computed;
        每米重: number unit=kg/m;
        总重: number unit=kg computed;
    }
    
    layout {
        header {
            row { cell 编号 rowspan=2; cell 规格 rowspan=2; cell 大样 rowspan=2; cell "长度" colspan=2; cell "重量" colspan=2; }
            row { cell 单根长; cell 总长; cell 每米重; cell 总重; }
        }
    }
}
```

---

## Barshape Layout (Grid, No Table Borders)

Use when shapes should be arranged in a visual grid without table borders:

```pcad
barshape_layout RebarDetails {
    title = "Details of rebars";
    grid = 3x3;              // columns x rows
    cell_size = (100, 150);  // width, height in mm
    origin = (0, 0);
    
    place N1 at (0, 0) { label = "N1 Φ16"; note = "H3+50"; }
    place N2 at (1, 0) { label = "N2 Φ16"; note = "H3+50"; }
    place N3 at (2, 0) { label = "N3 Φ12"; note = "B2-12"; }
    
    place N4 at (0, 1) { label = "N4 Φ12"; note = "L2-12"; }
    place N5 at (1, 1) { label = "N5 Φ16"; note = "H1+30"; }
    place N6 at (2, 1) { label = "N6 Φ16"; note = "H1+30"; }
}
```

### Grid Position Reference

```
       col=0      col=1      col=2
      ┌─────────┬─────────┬─────────┐
row=0 │ (0,0)   │ (1,0)   │ (2,0)   │
      ├─────────┼─────────┼─────────┤
row=1 │ (0,1)   │ (1,1)   │ (2,1)   │
      └─────────┴─────────┴─────────┘
```
