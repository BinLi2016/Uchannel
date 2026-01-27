// Divider belt (机非分隔带) painter.

window.DividerBeltPainter = {
  _helpers: {
    line(MX, a, b, color) {
      const { McDbLine, McGePoint3d, McCmColor } = MX;
      const ln = new McDbLine(new McGePoint3d(a.x, a.y, 0), new McGePoint3d(b.x, b.y, 0));
      if (color) ln.trueColor = new McCmColor(color.r, color.g, color.b);
      return ln;
    },
    text(MX, textString, pos, height, color, opts) {
      const { McDbText, McGePoint3d, McCmColor, McDb } = MX;
      const t = new McDbText();
      t.textString = String(textString);
      t.height = height;
      t.position = new McGePoint3d(pos.x, pos.y, 0);
      if (opts?.center) {
        t.horizontalMode = McDb.TextHorzMode.kTextCenter;
        t.verticalMode = McDb.TextVertMode.kTextVertMid;
        t.alignmentPoint = t.position;
      }
      if (opts?.rotationRad) {
        t.rotation = opts.rotationRad;
      }
      if (color) t.trueColor = new McCmColor(color.r, color.g, color.b);
      return t;
    },
    polyline(MX, pts, isClosed, color) {
      const { McDbPolyline, McGePoint3d, McCmColor } = MX;
      const pl = new McDbPolyline();
      pts.forEach((p) => pl.addVertexAt(new McGePoint3d(p.x, p.y, 0)));
      pl.isClosed = !!isClosed;
      if (color) pl.trueColor = new McCmColor(color.r, color.g, color.b);
      return pl;
    },
  },

  drawParamTable(headers, rows, position) {
    const MX = window.MxCAD || {};
    const { McDbPolyline, McGePoint3d, McCmColor, McDbText, McDb } = MX;
    const entities = [];

    const startX = position.x;
    const startY = position.y;
    const rowHeight = 34;
    const titleHeight = 55;

    const colWidths = headers.map((h) => Math.max(70, String(h).length * 10 + 35));
    colWidths[0] = 80;

    const titleText = new McDbText();
    titleText.textString = '机非分隔带参数表';
    titleText.height = 18;
    titleText.position = new McGePoint3d(startX + colWidths.reduce((a, b) => a + b, 0) / 2, startY + titleHeight / 2, 0);
    titleText.horizontalMode = McDb.TextHorzMode.kTextCenter;
    titleText.verticalMode = McDb.TextVertMode.kTextVertMid;
    titleText.alignmentPoint = titleText.position;
    titleText.trueColor = new McCmColor(0, 0, 0);
    entities.push(titleText);

    let currentY = startY;

    const drawCell = (x, y, w, h, textString, isHeader) => {
      const rect = new McDbPolyline();
      rect.addVertexAt(new McGePoint3d(x, y, 0));
      rect.addVertexAt(new McGePoint3d(x + w, y, 0));
      rect.addVertexAt(new McGePoint3d(x + w, y - h, 0));
      rect.addVertexAt(new McGePoint3d(x, y - h, 0));
      rect.isClosed = true;
      rect.trueColor = new McCmColor(0, 0, 0);
      entities.push(rect);

      const text = new McDbText();
      text.textString = String(textString ?? '');
      text.height = isHeader ? 10 : 9;
      text.position = new McGePoint3d(x + w / 2, y - h / 2, 0);
      text.horizontalMode = McDb.TextHorzMode.kTextCenter;
      text.verticalMode = McDb.TextVertMode.kTextVertMid;
      text.alignmentPoint = text.position;
      text.trueColor = new McCmColor(0, 0, 0);
      entities.push(text);
    };

    // header row
    headers.forEach((h, i) => {
      const x = startX + colWidths.slice(0, i).reduce((a, b) => a + b, 0);
      drawCell(x, currentY, colWidths[i], rowHeight, h, true);
    });
    currentY -= rowHeight;

    const rowToCells = (r) => [
      r.id,
      r.L,
      r.Hmin,
      r.Hmax,
      r.B,
      r.T,
      r.n,
      r.e1,
      r.n1,
      r.d1,
      r.n2,
      r.d2,
      r.a,
      r.m,
      r.b,
      r.e2,
    ];

    rows.forEach((r) => {
      const cells = rowToCells(r);
      cells.forEach((cell, colIndex) => {
        const x = startX + colWidths.slice(0, colIndex).reduce((a, b) => a + b, 0);
        drawCell(x, currentY, colWidths[colIndex], rowHeight, cell, false);
      });
      currentY -= rowHeight;
    });

    return entities;
  },

  drawLayoutDiagrams(position) {
    const MX = window.MxCAD || {};
    const entities = [];
    const outline = { r: 0, g: 0, b: 0 };

    // Elevation (schematic)
    const ex = position.x;
    const ey = position.y;
    const L = 2000;
    const H = 230;
    entities.push(this._helpers.text(MX, '机非分隔带钢筋网片立面(示意)', { x: ex + L / 2, y: ey + H + 70 }, 14, outline, { center: true }));
    entities.push(this._helpers.polyline(MX, [
      { x: ex, y: ey },
      { x: ex + L, y: ey },
      { x: ex + L, y: ey + H },
      { x: ex, y: ey + H },
    ], true, outline));
    entities.push(this._helpers.text(MX, 'N1 Φ20 @12.5cm', { x: ex + L + 160, y: ey + H - 20 }, 12, outline));
    entities.push(this._helpers.text(MX, 'N2 Φ16 @12.5cm', { x: ex + L + 160, y: ey + H - 50 }, 12, outline));

    // Cross-section (schematic)
    const cx = position.x + 2600;
    const cy = position.y;
    const w = 1750 / 10;
    const h = 2300 / 10;
    entities.push(this._helpers.text(MX, '机非分隔带正截面钢筋布置图(示意)', { x: cx + w / 2, y: cy + h + 70 }, 14, outline, { center: true }));
    entities.push(this._helpers.polyline(MX, [
      { x: cx, y: cy },
      { x: cx + w, y: cy },
      { x: cx + w, y: cy + h },
      { x: cx, y: cy + h },
    ], true, outline));
    entities.push(this._helpers.text(MX, 'N3 Φ16', { x: cx + w + 120, y: cy + h - 40 }, 12, outline));
    entities.push(this._helpers.text(MX, 'N4 φ8', { x: cx + w + 120, y: cy + h - 70 }, 12, outline));
    return entities;
  },
};
