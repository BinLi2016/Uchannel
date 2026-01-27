Below is a complete, self-contained, implementation-oriented specification for P-CAD (Parametric CAD Description Language), written as a language standard document.
It is intentionally precise, non-academic, and engineer-usable, suitable as:

an internal technical standard,

a basis for a parser / interpreter,

or a design document for a CAD automation system.

The specification is organized so that Level A (2D-only) is fully normative, while Level B+ are explicitly extensible but optional.

P-CAD

Parametric CAD Description Language
Language Specification v1.0 (Level A – 2D Parametric Drawings)

1. Scope and Design Objectives
1.1 Purpose

P-CAD is a domain-specific language (DSL) for describing parametric engineering drawings, with particular focus on:

civil / structural components,

reinforced concrete detailing,

multi-view layout drawings (plans, sections, elevations),

symbolic reinforcement representation (e.g. N6@12).

1.2 Design Goals

P-CAD is designed to be:

Parametric
All geometry and annotations may depend on symbolic parameters.

Deterministic
The same input must always generate the same drawing.

CAD-friendly
Maps cleanly to DXF/DWG/SVG/Canvas primitives.

Human-readable
Engineers must be able to read and edit files directly.

Incrementally implementable
Level A requires no solids, no topology, no boolean geometry.

2. Language Levels
Level	Capability	Status
Level A	2D parametric drawings	Normative (this spec)
Level B	2.5D extrusions, sections	Informative
Level C	Full solids, face semantics	Informative

This document is fully normative for Level A.

3. File Structure

A P-CAD file consists of ordered blocks:

[units]
[origin]
[drawing_info]?
[layers]
[hatch_style]*
[materials]?
[params]
[derive]
[rebar_set]*
[item_set]*
[table]*
[barshape]*

{component | sketch | region | mesh | bars | dim | dim_chain | dim_cumulative | dim_ordinate |
 dim_spacing | label | callout | chamfer | mirror | slope | section_marker | section_label |
 detail_marker | elevation_marker | grid_system | view | sheet}+


Order matters only where references are required.

4. Lexical and Syntax Rules
4.1 Units
units mm | cm | m;


All numeric literals are interpreted in declared units.

Unit-qualified literals:
Numeric literals may optionally include a unit suffix with no spaces (e.g., 8cm, 0.55m, 12mm).
When a unit suffix is present, the literal is converted to the file unit before evaluation.

4.2 Identifiers

Case-sensitive

Must start with a letter or underscore

May contain digits and _

Valid:

A_A
U槽剖面
mesh_main

4.3 Numbers and Expressions

Expressions support:

literals: 120, 3.5, 8cm, 0.55m

operators: + - * /

parentheses

parameter references

Example:

inner_b = b - 2*cover;


No trigonometric or conditional expressions in Level A.

5. Coordinate System

Global 2D Cartesian coordinate system

Right-handed

Origin default (0,0)

origin (0,0);


Each view may apply translation and scale.

6. Layers and Styles
layers {
  outline: color(0,255,255) lineweight(0.25);
  rebar:   color(255,0,0)   lineweight(0.20);
  mesh:    color(255,0,0)   lineweight(0.18);
  hatch:   color(180,180,180) lineweight(0.10) pattern(ANSI37);
  dim:     color(0,255,255) lineweight(0.18);
  text:    color(0,255,0)   lineweight(0.18);
}

6.1 Layer Semantics

Layers are semantic, not decorative.

Layer	Intended use
outline	Concrete outlines
rebar	Steel bars (linear bars, stirrups, etc.)
mesh	Steel mesh / distributed reinforcement lines
hatch	Material hatch (standard CAD fill for cut sections)
dim	Dimensions
text	Notes, labels
7. Parameters and Derived Values
7.1 Parameter Block
params {
  L1 = 5000;
  h1 = 1500;
  cover = 40;
}


Parameters are immutable after declaration.

7.2 Derived Block
derive {
  inner_h = h1 - cover;
}


Derived values may reference parameters or earlier derived values.

8. Geometry Primitives (Level A)
8.1 Sketch

A sketch defines 2D geometry, not a solid.

sketch Name layer=outline {
  polyline outer closed {
    (0,0) -> (L2,0) -> (L2,h1) -> (0,h1);
  }

  line center (0,h1/2) -> (L2,h1/2);
}

Supported primitives:

polyline (open / closed)

line

rect

circle (optional but allowed)

8.2 Region

A region defines a fillable / trimmable area.

region InnerRegion {
  boundary = inset(U槽剖面.outer, cover);
}

Supported operations:

inset(polyline, offset)

offset(polyline, offset) (non-closed allowed)

No boolean unions in Level A.

9. Reinforcement Model (Level A)
9.1 Rebar Set
rebar_set N6  { dia = 6;  }
rebar_set N12 { dia = 12; }


A rebar set is a symbolic steel type, not geometry.

9.2 Mesh (Grid reinforcement)
mesh WebMesh layer=mesh {
  set = N6;
  region = InnerRegion;
  spacing_x = 120;
  spacing_y = 120;
  style = "grid";
  label = "N6@12";
}

Semantics:

Generates orthogonal grid lines

Trimmed to region

Label is semantic (used for callouts)

9.3 Bars (Linear reinforcement)
bars BaseBars layer=rebar {
  set = N12;
  path = line(cover,cover) -> line(L2-cover,cover);
  count = 2;
  spacing = 50;
  label = "N12×2";
}

10. Annotation
10.1 Dimensions
dim linear {
  from = (0,0);
  to   = (L2,0);
  text = "L2";
}


Supported types:

linear

vertical

horizontal

Dimensions are parametric objects, not text.

10.2 Labels
label "A-A" at (500,1200) layer=text;

10.3 Callouts
callout WebMesh {
  at = (600,200);
  leader = auto;
  text = WebMesh.label;
}


Callouts bind annotations to semantic objects.

10.4 Break Marks (Long Member Abbreviation)
break_mark {
  on = line(x1,y1) -> line(x2,y2);
  style = zigzag | wave;
  gap = 20;
}

Semantics:

- `break_mark` is a graphical abbreviation only; it does not change geometry length.
- Intended for long members or repetitive regions where the drawing shows a shortened view.
- `gap` is the omitted length shown by the break symbol; dimensions should still show full length.

11. Views
11.1 View Definition
view V_A_A {
  source = A_A_外形;
  at = (80,620);
  scale = 1:15;
}


A view is a transform applied to a sketch or group.

11.2 View Semantics

No hidden-line removal

No projection math

Pure 2D copy + transform

This mirrors manual CAD drafting practice.

Optional view breaks:
Views may include `break_mark` objects to visually shorten long members while keeping dimensions full.

12. Sheet
12.1 Sheet Definition
sheet S1 {
  size = A3;
  scale = 1:15;

  title "U型槽钢筋布置图";

  place V_A_A;
  place V_B_B;

  notes at (80,1380) {
    "注：";
    "1. 保护层厚度40mm；";
  }
}

12.2 Sheet Responsibilities

Layout coordination

Title, notes, scale

No geometry creation

13. Rendering Order

Within a sheet:

outline

hatch (regions)

mesh / bars

dim

label / notes

Later objects draw over earlier ones.

14. Error Handling (Normative)

An implementation must:

Reject undefined identifiers

Reject circular parameter dependencies

Reject regions with invalid boundaries

Reject meshes without regions

Warnings (not errors):

Overlapping meshes

Labels outside sheet bounds

15. Mapping to CAD Outputs (Informative)
P-CAD	DXF / DWG
polyline	LWPOLYLINE
mesh	LINE grid + TRIM
region	HATCH boundary
dim	DIMLINEAR
label	MTEXT
16. Extensibility Rules

Future levels may add:

extrude

section

station

table_extensions

Level A implementations must ignore unknown blocks gracefully.

17. Philosophy Summary

P-CAD is not a geometry kernel.
It is a parametric drafting language that formalizes how engineers already draw.


18. Tabular Data (Level A)
18.1 Purpose

Engineering drawings commonly include standardized tables, such as:

- parameter lookup tables (参数表)
- rebar schedules / quantity tables (钢筋数量表)
- material summary tables (汇总表)

P-CAD provides a unified `table` block to model these tables as semantic, computable objects.

18.2 Table Definition

Syntax:

table Name {
  type = lookup | schedule | summary;
  key = <column_name>;

  columns {
    <col_name>: <col_type> [unit=<unit>] [computed];
    ...
  }

  row <RowKey>? { <col_name>=<expr>; ... }

  compute {
    <col_name> = <expr>;
    ...
  }

  summary {
    <name> = <summary_expr>;
    ...
  }

  layout {
    header {
      row { cell 编号 rowspan=2; cell 规格 rowspan=2; cell "大样" rowspan=2; cell "长度" colspan=2; cell "重量" colspan=2; }
      row { cell 单根长; cell 总长; cell 每米重; cell 总重; }
    }
    column_groups {
      "长度" = [单根长, 总长];
      "重量" = [每米重, 总重];
    }
  }
}

Notes:

- `row <RowKey>` is allowed only when `type=lookup`. The row key value must be unique.
- `key` is required for `type=lookup` and must match a declared column name.
- In Level A, `<expr>` supports only literals, + - * /, parentheses, and references.
- `layout` is optional. If omitted, a single header row is generated from the column names.

18.2.2 Table Units and Literals

Rules:

- If a column declares `unit`, row literals without suffix are interpreted in that column unit.
- A unit-qualified literal (e.g., 8cm, 0.55m) overrides the column unit and is converted into it.
- If a column has no `unit`, literals are interpreted in the file unit.
- Mixed units inside a single computed expression should be normalized; implementations may warn if not.

Supported unit suffixes in Level A (tables): mm, cm, m, kg, kg/m, m3.

18.2.1 Built-in Accessors

- `table(TableName, keyValue)` returns a row record for `type=lookup`.
- `table(TableName)` returns the table object (for export / placement).

Tables are placeable on sheets:

place table TableName;

18.3 Column Types

Supported `<col_type>` in Level A:

- `string`
- `integer`
- `number` (optionally with `unit`)
- `rebar_spec` (e.g., Φ6, Φ8, Φ12; provides `.dia` and `.grade`)
- `barshape_ref` (reference to a `barshape` object)
- `material_ref` (reference to `materials`, e.g., steel.HPB300, concrete.C35)
- `item_set_ref` (reference to a non-rebar `item_set`)

Unit rules:

- `unit` defines the input unit for the column and is used for validation and export.
- Implementations may warn if arithmetic mixes incompatible units.

`rebar_spec` literal forms (normative):

- `Φ12` => dia=12, grade=unknown
- `HPB300-Φ8` => dia=8, grade=HPB300
- `HRB400-Φ12` => dia=12, grade=HRB400

If grade is unknown, predicates comparing `.grade` must evaluate to false (not error).

18.4 Example: Parameter Lookup Table

table U型槽参数表 {
  type = lookup;
  key = 编号;

  columns {
    编号: string;
    L1: number unit=mm;
    h1: number unit=mm;
    h2: number unit=mm;
    t1: number unit=cm;
    t2: number unit=cm;
    cover: number unit=mm;
  }

  row YU1 { 编号="YU1"; L1=700;  h1=80;  h2=225; t1=8; t2=10; cover=40; }
  row YU2 { 编号="YU2"; L1=800;  h1=80;  h2=225; t1=8; t2=10; cover=40; }
  row YU3 { 编号="YU3"; L1=1000; h1=80;  h2=250; t1=8; t2=12; cover=40; }
}

Lookup usage:

derive {
  current = table(U型槽参数表, "YU3");
  L1 = current.L1;
  h1 = current.h1;
}

18.5 Example: Rebar Schedule Table

table YU1钢筋数量表 {
  type = schedule;

  columns {
    编号: integer;
    规格: rebar_spec;
    大样: barshape_ref;
    根数: integer;
    单根长: number unit=cm;
    总长: number unit=m computed;
    每米重: number unit=kg/m;
    总重: number unit=kg computed;
  }

  row { 编号=1; 规格=Φ8;  大样=S1; 根数=10; 单根长=320; 每米重=0.395; }
  row { 编号=2; 规格=Φ12; 大样=S2; 根数=5;  单根长=480; 每米重=0.888; }

  compute {
    总长 = 单根长 * 根数 / 100;
    总重 = 总长 * 每米重;
  }

  summary {
    HPB300钢筋 = sum(总重 where 规格.grade == HPB300);
    HRB400钢筋 = sum(总重 where 规格.grade == HRB400);
    钢筋合计 = sum(总重);
  }

  layout {
    header {
      row { cell 编号 rowspan=2; cell 规格 rowspan=2; cell 大样 rowspan=2; cell "长度" colspan=2; cell "重量" colspan=2; }
      row { cell 单根长; cell 总长; cell 每米重; cell 总重; }
    }
  }
}

18.6 Example: Mixed Item Schedule

table 盖板与角钢材料表 {
  type = schedule;

  columns {
    编号: integer;
    材料: material_ref;
    构件: item_set_ref;
    数量: number unit=pcs;
    单件长: number unit=cm;
    总长: number unit=m computed;
    单位重: number unit=kg/m;
    总重: number unit=kg computed;
  }

  row { 编号=1; 材料=steel.Q235; 构件=Angle25; 数量=12; 单件长=49cm; 单位重=0.57; }
  row { 编号=2; 材料=concrete.C35; 构件=CoverSlab; 数量=1; 单件长=0; 单位重=0; }

  compute {
    总长 = 单件长 * 数量 / 100;
    总重 = 总长 * 单位重;
  }
}


19. Bar Shape Diagrams (Level A)
19.1 Purpose

Bar shape diagrams (大样) are used in schedules to define bending geometry and dimensions.
In Level A, `barshape` is a 2D semantic object that can be rendered and referenced by tables.

19.2 Barshape Definition

barshape Name {
  type = straight | L_bend | U_bend | stirrup | custom;
  segments = [ (x1,y1) -> (x2,y2), ... ];

  bend_radius = <expr> | [<expr>, ...];
  hooks {
    start = hook(angle=90, length=50);
    end   = hook(angle=135, length=75);
  }
  dims {
    <name> = <expr>;
    ...
  }
}

Example:

barshape S1 {
  type = straight;
  segments = [ (0,0) -> (a,0) ];
  dims { a = 320; }
}

barshape S2 {
  type = L_bend;
  segments = [ (0,0) -> (a,0) -> (a,b) ];
  dims { a = 200; b = 150; }
}

barshape S3 {
  type = stirrup;
  segments = [ (0,0) -> (a,0) -> (a,b) -> (0,b) -> (0,0) ];
  bend_radius = [r, r, r, r];
  hooks {
    start = hook(angle=135, length=50);
    end   = hook(angle=135, length=50);
  }
  dims { a = 120; b = 200; r = 8; }
}


20. Materials and Grades (Level A)
20.1 Materials Block

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

20.2 Extending `rebar_set`

rebar_set N6  { dia = 6;  grade = HPB300; weight_per_m = 0.222; }
rebar_set N8  { dia = 8;  grade = HPB300; weight_per_m = 0.395; }
rebar_set N12 { dia = 12; grade = HRB400; weight_per_m = 0.888; }

Notes:

- `grade` and `weight_per_m` are optional in Level A but enable schedule validation and summaries.

20.3 Item Sets (Non-Rebar Components)

An `item_set` describes non-rebar components that appear in schedules (e.g., angle steel, embedded plates).

item_set Name {
  type = angle_steel | plate | embedded | concrete_member | custom;
  spec = "L25x25x3";
  material = steel.Q235;
  weight_per_m = 0.57;
}

Notes:

- `material` should reference a `materials` entry when applicable.
- `weight_per_m` is optional; if present it enables weight computations in schedules.


21. Notes Block (Level A)

The existing sheet notes syntax remains valid:

notes at (80,1380) {
  "注：";
  "1. 保护层厚度40mm；";
}

Enhanced form (informative but recommended):

notes at (80,1380) layer=text {
  title = "注：";
  items = [
    "1. 钢筋保护层厚度：底板40mm，侧壁25mm；",
    "2. 混凝土标号C35，钢筋HPB300、HRB400；"
  ];
}


22. Component (Reusable Assembly) (Level A)

A `component` is a named reusable group of P-CAD objects, enabling assemblies like cover plates,
grates, and typical details to be reused across sheets.

component Name {
  {sketch | region | mesh | bars | dim | label | callout | table}+
}

Placement:

place component Name at (x,y) scale = 1:10;

Example:

component 盖板 {
  sketch CoverPlate layer=outline {
    polyline outer closed {
      (0,0) -> (500,0) -> (500,50) -> (0,50);
    }
  }

  region CoverConcrete {
    boundary = CoverPlate.outer;
    hatch = concrete;
  }

  mesh CoverMesh layer=mesh {
    set = N6;
    region = CoverConcrete;
    spacing_x = 120;
    spacing_y = 120;
    style = "grid";
    label = "N6@12";
  }

  bars CoverBars layer=rebar {
    set = N12;
    path = line(40,25) -> line(460,25);
    count = 2;
    spacing = 50;
    label = "N12×2";
  }
}


23. Section Markers and Section Views (Level A)

23.1 Section Marker

section_marker A {
  from = (x1,y1);
  to   = (x2,y2);
  label = "A-A";
  direction = left | right;
}

23.2 Linking Views

view V_A_A {
  source = 侧墙剖面;
  section = A;
  at = (80,620);
  scale = 1:15;
}

Notes:

- In Level A, `section_marker` is annotation-only and does not perform projection.


24. Title Block (Level A)

sheet S1 {
  size = A3;
  scale = 1:15;

  titleblock {
    title = "U型槽钢筋布置图";
    project = "某市政工程";
    drawing_no = "S-01";
    drawn_by = "制图";
    checked_by = "校核";
    date = "2026-01";
  }

  place V_A_A;
  place table YU1钢筋数量表;
}


25. Standard CAD Hatch (Level A)
25.1 Hatch Style

hatch_style concrete {
  pattern = ANSI37;
  scale = 1.0;
  angle = 0;
}

hatch_style steel_section {
  pattern = ANSI31;
  scale = 1.0;
  angle = 45;
}

25.2 Region Hatch with Islands (Holes)

`region` may declare hatch fill and inner islands (holes). Islands represent real voids/openings:

region 侧墙剖面 {
  boundary = sketch.墙体外形;
  hatch = concrete;
  islands = [sketch.窗洞, sketch.管道套管];
  island_detection = explicit;
}

Allowed island detection modes:

- `explicit` (default): only use declared `islands`
- `auto`: detect nested closed boundaries as islands
- `none`: ignore islands and fill the entire boundary

Normative clarification:

- Rebar symbols are drawn as steel objects (mesh/bars) over hatch; they are not islands.


26. Table Computation and Aggregation (Level A)

26.1 Computed Columns

Rules:

- `compute` is evaluated per row, after row literals are bound.
- A computed expression may reference other columns in the same row.
- Circular dependencies in `compute` are errors.

26.2 Summary Expressions

Supported summary functions:

- `sum(column)`
- `count()`
- `sum(column where <predicate>)`

Predicates support:

- numeric comparisons: `==`, `!=`, `>=`, `<=`, `>`, `<`
- field access on `rebar_spec`: `.dia`, `.grade`

Examples:

summary {
  钢筋合计 = sum(总重);
  HRB400钢筋 = sum(总重 where 规格.grade == HRB400);
  大直径根数 = sum(根数 where 规格.dia >= 12);
}


27. Cross-Section Boundary Modeling (Level A)

27.1 Single-Loop Boundary Principle

When modeling cross-sections of structural members, prefer a **single closed polyline** that traces
the actual material boundary, rather than an enclosing shape with islands subtracted.

Rationale:

- Matches how engineers hand-draw sections (one continuous pencil stroke)
- Avoids implicit boolean geometry (subtraction is not a Level A primitive)
- Produces cleaner DXF/DWG output (single LWPOLYLINE vs. HATCH with islands)
- More intuitive for parametric editing

27.2 When to Use Islands

Islands (`islands = [...]`) should be reserved for **real penetrations or openings** that exist
within an otherwise solid region, such as:

- pipe sleeves (管道套管)
- window/door openings in walls (窗洞、门洞)
- drainage holes (排水孔)
- embedded conduits (预埋管线)

Islands are NOT appropriate for:

- defining the basic shape of a cross-section (e.g., U-channel, T-beam, box culvert)
- modeling the "empty space" of an open channel or trough

27.3 Guidance for Common Cross-Section Types

| Section Type | Recommended Approach |
|--------------|---------------------|
| Rectangular beam/slab | Single closed rectangle |
| T-beam (T形梁) | Single 8-point closed polyline |
| I-beam (工字梁) | Single 12-point closed polyline |
| U-channel (U形槽) | Single 10-12 point closed polyline tracing the concrete profile |
| Box culvert (箱涵) | Single closed polyline (outer walls + inner cavity traced as one loop) |
| Solid with penetrations | Outer boundary + islands for actual holes |

27.4 Example: U-Channel Single-Loop Boundary

Correct approach (single connected loop):

```
sketch U槽轮廓 layer=outline {
  polyline boundary closed {
    // 外底边
    (0, 0) -> (B, 0)
    // 右外壁 (垂直段 + 斜坡段)
    -> (B, d) -> (x_wing_right, H)
    // 右翼缘顶面 -> 右内壁顶
    -> (x_right_inner, H)
    // 右内壁 (垂直下行)
    -> (x_right_inner, t)
    // 内底面 (从右到左)
    -> (x_left_inner, t)
    // 左内壁 (垂直上行)
    -> (x_left_inner, H)
    // 左翼缘顶面
    -> (x_wing_left, H)
    // 左外壁 (斜坡段 + 垂直段)
    -> (0, d);
    // 自动闭合回 (0, 0)
  }
}

region U槽断面 {
  boundary = U槽轮廓.boundary;
  hatch = concrete;
  // 无 islands — 这是单一连通断面
}
```

Discouraged approach (outer + island subtraction):

```
// 不推荐：需要隐式布尔减法
sketch U槽外框 layer=outline {
  polyline outer closed {
    (0, 0) -> (B, 0) -> (B, H) -> (0, H);
  }
}

sketch U槽内腔 layer=outline {
  polyline inner closed {
    (x_left, t) -> (x_right, t) -> (x_right, H) -> (x_left, H);
  }
}

region U槽断面 {
  boundary = U槽外框.outer;
  hatch = concrete;
  islands = [U槽内腔];  // 不推荐：内腔不是"穿孔"而是断面形状的一部分
}
```

27.5 Normative Rule

For cross-sections where the material forms a single connected region (no internal holes),
implementations SHOULD:

1. Accept single-loop boundary definitions without islands
2. Warn if islands are used to define basic section geometry (not penetrations)
3. Generate simpler CAD output (single polyline boundary for hatch)

For cross-sections with actual penetrations (pipes, conduits, openings),
implementations MUST support the `islands` mechanism as specified in Section 25.2.


28. Chamfer and Fillet Notation (Level A)

28.1 Purpose

Engineering drawings commonly annotate corner treatments (chamfers, fillets, bevels) with
dimension notation like `d₁×d₂` for asymmetric chamfers or `R10` for fillets. P-CAD provides
a `chamfer` block to formally describe these annotations.

28.2 Chamfer Definition

```
chamfer Name {
  type = chamfer | fillet | bevel;
  at = <polyline_ref>.corner(<index>) | (<x>, <y>);
  dim1 = <expr>;              // first leg or radius
  dim2 = <expr>;              // second leg (chamfer only, optional if symmetric)
  notation = auto | "<custom>";  // display format
  layer = dim;                // default layer
}
```

28.3 Notation Formats

| Type | Notation Example | Meaning |
|------|------------------|---------|
| chamfer (symmetric) | `C10` or `10×10` | 10 unit legs on both sides |
| chamfer (asymmetric) | `d₁×d₂` or `5×8` | 5 unit horizontal, 8 unit vertical |
| fillet | `R10` or `r=10` | 10 unit radius |
| bevel | `45°×10` | 45° angle, 10 unit depth |

28.4 Examples

```
chamfer TopCorner {
  type = chamfer;
  at = 框架涵截面.inner.corner(2);
  dim1 = d1;
  dim2 = d2;
  notation = "${dim1}×${dim2}";
}

chamfer BottomFillet {
  type = fillet;
  at = (a2, slab_t);
  dim1 = 10;
  notation = "R${dim1}";
}
```

28.5 Corner Reference Semantics

- `corner(index)` uses 0-based indexing from the first vertex of the polyline.
- For closed polylines, `corner(0)` refers to the angle at the first vertex.
- Implementations should render the chamfer/fillet symbol and leader to the specified location.

28.6 Automatic Geometry Modification (Informative)

In Level A, `chamfer` is annotation-only and does not modify polyline geometry.
Future levels may support automatic corner modification:

```
// Level B+ (informative)
polyline with_chamfers closed {
  (0,0) -> (100,0) chamfer(5,5)
       -> (100,80) fillet(10)
       -> (0,80);
}
```


29. Extended Dimension Features (Level A)

29.1 Purpose

Engineering drawings often require advanced dimension formatting beyond simple linear dimensions.
This section extends the `dim` block with additional properties for offset, extension lines,
and parametric expression display.

29.2 Extended Dimension Syntax

```
dim <type> {
  from = (<x1>, <y1>);
  to   = (<x2>, <y2>);
  text = "<display_text>";

  // Extended properties (all optional)
  offset = <expr>;              // perpendicular offset from baseline
  extension_gap = <expr>;       // gap before extension line starts (default: 2)
  extension_overhang = <expr>;  // overhang beyond dimension line (default: 3)
  text_position = center | above | below | outside;
  text_rotation = horizontal | aligned | <angle>;
  precision = <integer>;        // decimal places for computed values
  show_unit = true | false;     // append unit suffix to dimension text
}
```

29.3 Parametric Expression Display

Dimension text may include expressions using `${}` interpolation:

```
dim horizontal {
  from = (0, -10);
  to = (total_base, -10);
  text = "W+50";                // literal display
}

dim horizontal {
  from = (0, -10);
  to = (total_base, -10);
  text = "${W}+50=${total_base}";  // shows computed value
}
```

Implementations should:

- Evaluate expressions inside `${}` at render time
- Format numeric results according to `precision`
- Append unit suffix if `show_unit = true`

29.4 Dimension Chains

Chained dimensions share a common baseline:

```
dim_chain horizontal {
  baseline_y = H + 15;
  points = [(0, H), (a2, H), (W - a2, H), (W, H)];
  labels = ["a₂", "内宽", "a₂"];
}
```

29.5 Cumulative (Running) Dimensions

```
dim_cumulative horizontal {
  origin = (0, 0);
  points = [(50, 0), (100, 0), (200, 0)];
  offset = -20;
  // Displays: 50, 100, 200 (cumulative from origin)
}
```

29.6 Ordinate Dimensions

Ordinate dimensions show X or Y coordinates from a datum:

```
dim_ordinate {
  datum = (0, 0);
  axis = x | y;
  points = [(50, 20), (100, 40), (150, 60)];
  offset = 10;
}
```

29.7 Angular Dimensions

```
dim angular {
  center = (<x>, <y>);
  from_angle = <expr>;
  to_angle = <expr>;
  radius = <expr>;
  text = auto | "<custom>";
}
```

Example:

```
dim angular {
  center = (100, 50);
  from_angle = 0;
  to_angle = 45;
  radius = 30;
  text = "45°";
}
```


30. Extended Layer Semantics (Level A)

30.1 Purpose

Civil and structural engineering drawings require semantic layers beyond the basic set.
This section defines additional standard layers for common use cases.

30.2 Extended Layer Definitions

```
layers {
  // Core layers (from Section 6)
  outline:    color(0,255,255)   lineweight(0.25);
  rebar:      color(255,0,0)     lineweight(0.20);
  mesh:       color(255,0,0)     lineweight(0.18);
  hatch:      color(180,180,180) lineweight(0.10) pattern(ANSI37);
  dim:        color(0,255,255)   lineweight(0.18);
  text:       color(0,255,0)     lineweight(0.18);

  // Extended layers for civil engineering
  foundation: color(165,42,42)   lineweight(0.20);   // foundation, footings
  bedding:    color(139,90,43)   lineweight(0.15);   // bedding, leveling course
  soil:       color(139,90,43)   lineweight(0.10);   // soil, backfill, embankment
  waterproof: color(0,0,255)     lineweight(0.15);   // waterproofing membrane
  drainage:   color(0,191,255)   lineweight(0.15);   // drainage, water flow
  centerline: color(255,255,0)   lineweight(0.13) linetype(CENTER);
  hidden:     color(128,128,128) lineweight(0.13) linetype(DASHED);
  existing:   color(128,128,128) lineweight(0.18);   // existing structures
  proposed:   color(0,255,0)     lineweight(0.25);   // proposed/new work
  demolish:   color(255,0,0)     lineweight(0.18) linetype(DASHED);

  // Annotation layers
  leader:     color(0,255,255)   lineweight(0.13);   // callout leaders
  symbol:     color(0,255,0)     lineweight(0.15);   // north arrows, section marks
  grid:       color(100,100,100) lineweight(0.10);   // grid lines
}
```

30.3 Linetype Attribute

Layers may specify a linetype:

```
layer Name: color(...) lineweight(...) linetype(<type>);
```

Standard linetypes:

| Linetype | Description | Pattern |
|----------|-------------|---------|
| CONTINUOUS | Solid line (default) | ───────── |
| DASHED | Short dashes | ── ── ── |
| HIDDEN | Hidden/obscured lines | - - - - - |
| CENTER | Centerline | ─ ─ ─ ─ ─ |
| PHANTOM | Phantom/alternate | ─ ─ · ─ ─ · |
| DOT | Dotted line | · · · · · · · |

30.4 Layer Groups (Informative)

Implementations may support layer groups for visibility control:

```
layer_group structural {
  layers = [outline, rebar, mesh, hatch];
}

layer_group annotation {
  layers = [dim, text, leader, symbol];
}
```


31. Symmetric and Mirror Modifiers (Level A)

31.1 Purpose

Many structural cross-sections are symmetric. P-CAD supports a `symmetric` modifier
to define only half of a section and automatically generate the mirror.

31.2 Symmetric Sketch

```
sketch Name layer=<layer> symmetric=<axis> {
  // Define only the half or quarter profile
  polyline half_profile {
    ...
  }
}
```

Axis values:

- `vertical` — mirror across Y axis (X = 0 or specified center)
- `horizontal` — mirror across X axis (Y = 0 or specified center)
- `both` — quarter symmetry, mirror both axes

31.3 Symmetric Sketch with Center

```
sketch 对称截面 layer=outline symmetric=vertical center_x=W/2 {
  // Define right half only
  polyline right_half {
    (W/2, 0) -> (W, 0) -> (W, H) -> (W/2, H);
  }
}
// Left half is automatically mirrored
```

31.4 Mirror Block

For explicit mirroring control:

```
mirror Name {
  source = <sketch_ref>;
  axis = vertical | horizontal;
  center = (<x>, <y>) | <expr>;
  include_original = true | false;  // default: true
}
```

Example:

```
sketch 右半侧壁 layer=outline {
  polyline p { (50,0) -> (100,0) -> (100,80) -> (50,80); }
}

mirror 左半侧壁 {
  source = 右半侧壁;
  axis = vertical;
  center = (50, 0);
  include_original = true;
}
// Result: complete symmetric profile from x=0 to x=100
```

// Result: complete symmetric profile from x=0 to x=100
  ```

31.5 Semantic Implications

- Dimensions on symmetric sketches should use the `symmetric` attribute to indicate
  that the dimension applies to both sides.
- Rebar counts on symmetric sections may use `count_per_side` for clarity.


32. Drawing Information Block (Level A)

32.1 Purpose

Provides drawing-level metadata separate from sheet layout.

32.2 Syntax

```
drawing_info {
  title = "<drawing title>";
  subtitle = "<optional subtitle>";
  project = "<project name>";
  drawing_no = "<drawing number>";
  revision = "<revision code>";
  date = "<date>";
  scale = <scale>;
  units = <unit>;
  author = "<drafter name>";
  checker = "<checker name>";
  approver = "<approver name>";

  // Optional metadata
  client = "<client name>";
  location = "<project location>";
  phase = "<design phase>";
  status = draft | for_review | approved | as_built;
}
```

32.3 Example

```
drawing_info {
  title = "框架涵横截面图";
  subtitle = "Frame Culvert Cross-Section";
  project = "某市政排水工程";
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

32.4 Relationship to Sheet

`drawing_info` provides logical metadata; `sheet` handles physical layout.
A drawing may span multiple sheets:

```
drawing_info {
  title = "U型槽配筋图";
  drawing_no = "S-01";
}

sheet S1 {
  size = A3;
  page = 1 of 2;
  titleblock { inherit = drawing_info; }
  place V_平面图;
}

sheet S2 {
  size = A3;
  page = 2 of 2;
  titleblock { inherit = drawing_info; }
  place V_剖面图;
  place table 钢筋数量表;
}
```


33. Extended Hatch Patterns (Level A)

33.1 Purpose

Civil engineering drawings use domain-specific hatch patterns beyond standard CAD fills.

33.2 Additional Standard Patterns

```
hatch_style concrete {
  pattern = ANSI37;
  scale = 1.0;
  angle = 0;
}

hatch_style gravel {
  pattern = GRAVEL;
  scale = 0.8;
  angle = 0;
}

hatch_style sand {
  pattern = SAND;
  scale = 0.6;
  angle = 0;
}

hatch_style compacted_fill {
  pattern = EARTH;
  scale = 0.8;
  angle = 45;
}

hatch_style rock {
  pattern = ROCK;
  scale = 1.0;
  angle = 0;
}

hatch_style water {
  pattern = WATER;
  scale = 1.0;
  angle = 0;
}

hatch_style insulation {
  pattern = INSUL;
  scale = 0.5;
  angle = 0;
}
```

33.3 Custom Hatch Definition (Informative)

```
hatch_style custom_bedding {
  type = custom;
  lines = [
    { angle = 45;  origin = (0,0); spacing = 5; },
    { angle = -45; origin = (0,0); spacing = 5; }
  ];
  // Creates cross-hatch pattern
}
```

33.4 Gradient Fill (Level B+)

```
// Informative, not required in Level A
hatch_style gradient_fill {
  type = gradient;
  color1 = (200, 200, 200);
  color2 = (100, 100, 100);
  direction = vertical | horizontal | radial;
}
```


34. Section and Detail References (Level A)

34.1 Purpose

Engineering drawings use standardized symbols and references to link views.

34.2 Section Marker Enhancements

Extended `section_marker` with standard symbol options:

```
section_marker A {
  from = (x1, y1);
  to = (x2, y2);
  label = "A-A";
  direction = left | right | both;
  style = arrow | circle | flag;
  target_view = V_A_A;  // optional link to target view
}
```

34.3 Detail Markers

```
detail_marker D1 {
  center = (x, y);
  radius = 15;
  label = "1";
  scale = 1:10;
  target_view = V_Detail_1;
}
```

34.4 Elevation Markers

```
elevation_marker EL1 {
  at = (x, y);
  elevation = "+5.500";
  datum = "±0.000";
  direction = up | down;
}
```

34.5 Grid Lines and Column Markers

```
grid_system {
  x_grid = ["A", "B", "C", "D"] at [0, 600, 1200, 1800];
  y_grid = ["1", "2", "3"] at [0, 500, 1000];
  
  circle_style = true;  // encircled grid labels
  extend_top = 50;
  extend_bottom = 50;
}
```


35. Slope and Taper Annotations (Level A)

35.1 Purpose

Civil engineering drawings frequently show slopes (ratio `1:n` or percentage `%`) for walls,
embankments, and drainage.

35.2 Syntax

```
slope Name {
  on = <line_ref> | (<x1>, <y1>) -> (<x2>, <y2>);
  type = ratio | percentage | grade;
  value = <expr>;             // e.g. 25 for "25:1" or 2 for "2%"
  direction = up | down | auto;
  label = auto | "<custom>";
  offset = <expr>;            // distance from line
  layer = dim;
}
```

35.3 Examples

```
slope SideTaper {
  on = (top_cap_w/2, H3 - 50) -> (L2/2, 0);
  type = ratio;
  value = 25;
  label = "25:1";
}
```


36. Specialized Spacing Dimensions (Level A)

36.1 Purpose

Reinforcement drawings often use `@` notation (e.g., `N1@200`) to indicate spacing.

36.2 Syntax

```
dim_spacing {
  from = (<x1>, <y1>);
  to   = (<x2>, <y2>);
  spacing = <expr>;
  count = <expr> | auto;    // if auto, calculated from distance
  text = "@${spacing}" | "${count}@${spacing}";
}
```


37. Rebar Section Styles (Level A)

37.1 Purpose

Longitudinal bars (bars running perpendicular to the drawing plane) are represented as dots.

37.2 Syntax

```
bars Name {
  ...
  render_style = path | dots;  // default: path
  dot_radius = <expr>;         // if dots, radius of the circle
  fill = solid | none;
}
```

37.3 Automating Sectioning (Informative)

In advanced implementations, `bars` defined in a 3D context should automatically render
as dots if they intersect the 2D section plane.


38. Enhanced Rebar Path with Hooks (Level A)

38.1 Purpose

Support for standard anchorage hooks and curves in 2D rebar paths.

38.2 Syntax

```
bars Name {
  path = (x1, y1) [hook_start] -> (x2, y2) -> (x3, y3) [hook_end];
  
  hook_params {
    start = hook(angle=90, length=150, radius=40);
    end   = hook(angle=135, length=100, radius=40);
  }
}
```


39. Multi-leader Callouts (Level A)

39.1 Purpose

Support for callouts that point to multiple entities or locations.

39.2 Syntax

```
callout Name {
  at = [ (x1, y1), (x2, y2) ];  // array of points
  text = "<label>";
  leader_style = straight | curved | spline;
}
```


40. Appendix: Updated Example — Wing Wall Section

```
drawing_info {
  title = "Section of wing wall";
  scale = 1:20;
}

sketch 翼墙断面 symmetric=vertical {
  polyline wall {
    (20, 250) -> (20, 245) -> (14, 245) -> (40, 0);
  }
}

slope 墙面坡度 {
  on = 翼墙断面.wall.segment(3);
  type = ratio;
  value = 25;
}

bars N3_纵筋 {
  set = N3;
  render_style = dots;
  dot_radius = 6;
  spacing_y = 20;
}

callout N4_筋 {
  at = [ (15, 248), (17, 243) ];
  text = "N4";
}
```


41. Rebar Variant Naming (Level A)

41.1 Purpose

Labeling variations of a single rebar type, e.g., `N7(N7-1)` where `N7-1` is a sub-variant.

41.2 Syntax

```
rebar_set Name {
  ...
  variants = [
    { id = "N7-1"; length_adjustment = -50; }
  ];
}
```

41.3 Callout with Variant

```
callout Name {
  text = "${set.name}(${set.variant})";
}
```


42. Section Labels (Level A)

42.1 Purpose

Standardized rendering of section labels like "A-A" with underline.

42.2 Syntax

```
section_label Name {
  text = "A-A";
  at = (x, y);
  style = underlined | boxed | plain;
  layer = text;
}
```


43. L-Shaped and T-Shaped Section Helpers (Level A)

43.1 Purpose

Common cross-section shapes can be generated from parameters.

43.2 Syntax

```
sketch Name layer=outline type=L_section {
  B1 = <expr>;      // vertical leg height
  L1 = <expr>;      // horizontal leg length
  t1 = <expr>;      // vertical leg thickness
  t2 = <expr>;      // horizontal leg thickness
}
```

This generates an 8-point closed polyline automatically.

43.3 Other Built-in Section Types

| Type | Points | Parameters |
|------|--------|------------|
| `L_section` | 6 | B1, L1, t1, t2 |
| `T_section` | 8 | B, H, tf, tw |
| `I_section` | 12 | B, H, tf, tw |
| `U_section` | 10+ | B, H, t_wall, t_base |
| `box_section` | 8 | B, H, t |


44. Angled and Bent Leaders (Level A)

44.1 Purpose

Leaders in engineering drawings often have specific bend points.

44.2 Syntax

```
callout Name {
  at = (target_x, target_y);
  text = "<label>";
  leader_style = straight | angled | multi | spline;
  bend_points = [ (x1, y1), (x2, y2) ];  // optional waypoints
  text_position = (tx, ty);              // explicit text location
}
```


45. Rebar Position Semantics (Level A)

45.1 Purpose

Specify rebar placement relative to concrete faces without explicit coordinates.

45.2 Syntax

```
bars Name {
  set = <rebar_set>;
  region = <region_ref>;
  position = outer_face | inner_face | center | top | bottom | left | right;
  offset = <expr>;   // distance from specified face
  spacing = <expr>;
}
```


46. Appendix: Complete Example — A-A L-Section

```
drawing_info {
  title = "A-A";
  scale = 1:20;
}

sketch L形断面 layer=outline type=L_section {
  B1 = 200;
  L1 = 120;  // L3+L4
  t1 = 30;
  t2 = 30;
}

bars N1_纵筋 {
  set = N1;
  render_style = dots;
  region = L形断面;
  position = outer_face;
  spacing = 20;
}

bars N7_分布筋 {
  set = N7;
  variants = [ { id = "N7-1"; } ];
  path = ...;
}

section_label A_A {
  text = "A-A";
  at = (60, 220);
  style = underlined;
}

dim horizontal {
  from = (0, -30);
  to = (120, -30);
  text = "L3+L4";
}
```


47. Barshape Expression Dimensions (Level A)

47.1 Purpose

Bar shape diagrams often show dimensions as parametric expressions like `H3+50` or `28-(L2-12)`.

47.2 Syntax

```
barshape Name {
  ...
  dims {
    <name> = <expr>;              // Computed dimension
    <name> = <literal>;           // Fixed dimension
  }
  
  annotations {
    dim_<name> at segment(<n>) text="<display_text>";
    radius at bend(<n>) text="R<value>";
  }
}
```

47.3 Expression Display

Dimension text may show:
- Literal value: `"21.6"`
- Variable reference: `"H3+50"`
- Complex expression: `"(H3+50)-(a11+30)"`

Implementations should:
- Store both the expression and computed value
- Display the expression text in the diagram
- Use the computed value for length calculations


48. Bend Radius Notation (Level A)

48.1 Purpose

Standard notation for reinforcement bend radii based on bar diameter.

48.2 Syntax

```
barshape Name {
  ...
  bend_radius = <expr> | [<expr>, ...];   // Per-bend or uniform
  bend_notation = "R×d" | "R<value>";     // Display format
}
```

48.3 Standard Bend Radii

| Bar Diameter | Min Bend Radius (HPB300) | Min Bend Radius (HRB400) |
|--------------|--------------------------|--------------------------|
| ≤ 16mm | 2.5d | 4d |
| 16-25mm | 4d | 5d |
| > 25mm | 5d | 6d |

48.4 Example

```
barshape S_N1 {
  set = N1;   // dia = 16
  bend_radius = 2 * dia;
  annotations {
    radius at bend(1) text="R2×d";
  }
}
```


49. Barshape Inheritance and Variants (Level A)

49.1 Purpose

Many bar shapes are minor variations of a base shape, e.g., `N7` and `N7-1`.

49.2 Syntax

```
barshape S_N7 {
  set = N7;
  type = custom;
  segments = [...];
  dims { ... }
}

barshape S_N7_1 {
  set = N7_1;
  inherit = S_N7;                // Inherit base geometry
  dims {
    base = (L2 - 12) - (B2 - 12);  // Override specific dimension
  }
}
```

49.3 Semantics

- `inherit` copies all properties from parent barshape
- Child may override: `dims`, `annotations`, `hooks`
- Child may NOT change: `type`, base segment count


50. Barshape Sheet Layout (Level A)

50.1 Purpose

Organize multiple barshapes into a grid layout on a details sheet.

50.2 Syntax

```
sheet RebarDetails {
  type = barshape_sheet;
  size = A4;
  scale = 1:1;

  layout = grid(rows=3, cols=3);
  
  place barshape S_N1 at cell(1, 1);
  place barshape S_N2 at cell(1, 2);
  place barshape S_N3 at cell(1, 3);
  ...

  title "Details of rebars";
}
```

50.3 Auto-Layout

```
sheet RebarDetails {
  type = barshape_sheet;
  auto_layout = true;
  items = [S_N1, S_N2, S_N3, S_N4, S_N5, S_N6, S_N7, S_N7_1, S_N8];
  max_cols = 3;
  spacing = (50, 100);
}
```


51. Appendix: Complete Rebar Details Sheet Example

```
drawing_info {
  title = "Details of rebars";
  scale = 1:1;
}

params {
  H3 = 200;
  L2 = 100;
  B2 = 60;
}

rebar_set N1 { dia = 16; grade = HRB400; }
rebar_set N7 { dia = 12; grade = HRB400; }
rebar_set N7_1 { dia = 12; parent = N7; }

barshape S_N1 {
  set = N1;
  type = L_bend;
  segments = [ (0,0) -> (0, H3+50) -> (21.6, H3+50) ];
  bend_radius = 2 * dia;
  dims {
    vertical = H3 + 50;
    horizontal = 21.6;
  }
  annotations {
    dim at segment(1) text="H3+50";
    dim at segment(2) text="21.6";
    radius at bend(1) text="R2×d";
  }
}

barshape S_N7_1 {
  set = N7_1;
  inherit = S_N7;
  dims { base = (L2-12)-(B2-12); }
  annotations {
    dim at segment(1) text="(L2-12)-(B2-12)";
  }
}

sheet RebarSheet {
  type = barshape_sheet;
  auto_layout = true;
  items = [S_N1, S_N2, S_N3, S_N4, S_N5, S_N6, S_N7, S_N7_1, S_N8];
}
```


52. Horizontal Grid Systems (Level A)

52.1 Purpose

Plan views and front views often use horizontal grid lines (A, B, C, D) instead of vertical.

52.2 Syntax

```
grid_system {
  type = horizontal | vertical | both;
  labels = ["A", "B", "C", "D"];
  positions = [y1, y2, y3, y4];           // For horizontal grids
  
  left_extension = <expr>;
  right_extension = <expr>;
  label_style = letter | number | roman;
  show_both_sides = true | false;
  circle_style = true | false;
}
```

52.3 Grid Reference in Dimensions

Dimensions can link to grid lines:

```
dim_chain vertical {
  baseline_x = 500;
  points = [...];
  grid_labels = ["D", "C", "B", "A"];    // Show grid reference
}
```


53. Zone-Based Rebar Placement (Level A)

53.1 Purpose

Large foundations have different rebar zones (left, right, center, corner).

53.2 Syntax

```
bars Name {
  set = <rebar_set>;
  zone = left | right | center | corner | full;
  
  // Zone boundaries
  zone_start = (x1, y1);
  zone_end = (x2, y2);
  
  // Or reference to grid
  from_grid = B;
  to_grid = A;
}
```

53.3 Zone Inheritance

```
bars N3_1_变体 {
  inherit = N3_中层纵筋;
  zone = right;                          // Apply to right zone only
  label = "N3-1";
}
```


54. Grid-Linked Dimensions (Level A)

54.1 Purpose

Dimension chains that explicitly link to grid references.

54.2 Syntax

```
dim_chain_grid vertical {
  grids = ["A", "B", "C", "D"];
  baseline_x = <expr>;
  offset = 50;
  show_values = true;                    // Show distances between grids
}
```

Example output: Dimensions between A-B, B-C, C-D with grid bubbles.


55. Appendix: Complete Foundation Front View Example

```
drawing_info {
  title = "Front view of foundation";
  scale = 1:20;
}

params {
  L3 = 200;
  L4 = 300;
  H_slab = 115;
}

grid_system {
  type = horizontal;
  labels = ["A", "B", "C", "D"];
  positions = [115, 74, 24, 0];
  show_both_sides = true;
}

sketch 基础 layer=outline {
  polyline outer closed {
    (0, 0) -> (500, 0) -> (470, 115) -> (30, 115);
  }
}

bars N7_分布筋 {
  set = N7;
  render_style = dots;
  spacing_x = 20;
  zone = full;
}

bars N1_1_顶筋 {
  set = N1_1;
  zone = right;
  from_grid = B;
  to_grid = A;
}

dim_chain_grid vertical {
  grids = ["A", "B", "C", "D"];
  baseline_x = 550;
}
```