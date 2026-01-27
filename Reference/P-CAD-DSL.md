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
[layers]
[hatch_style]*
[materials]?
[params]
[derive]
[rebar_set]*
[table]*
[barshape]*

{component | sketch | region | mesh | bars | dim | label | callout | section_marker | view | sheet}+


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