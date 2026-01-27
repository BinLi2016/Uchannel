// Anti-collision curb stone drawing entrypoints.

window.drawCurbStoneFromSample = function () {
  const MX = window.MxCAD || {};
  const mxcad = MX.MxCpp && typeof MX.MxCpp.getCurrentMxCAD === 'function' ? MX.MxCpp.getCurrentMxCAD() : null;
  if (!mxcad) {
    console.error('MXCAD not ready.');
    return;
  }

  const drainagePainter = window.DrainageChannelPainter;
  const entities = [];

  // Title
  if (MX.McDbText) {
    const t = new MX.McDbText();
    t.textString = '防撞侧石结构及钢筋布置图';
    t.height = 30;
    t.position = new MX.McGePoint3d(1400, 1500, 0);
    t.horizontalMode = MX.McDb.TextHorzMode.kTextCenter;
    t.verticalMode = MX.McDb.TextVertMode.kTextVertMid;
    t.alignmentPoint = t.position;
    t.trueColor = new MX.McCmColor(0, 0, 0);
    entities.push(t);
  }

  // Cross-section profile (schematic)
  // Using cm units scaled to drawing units (1cm -> 10 units).
  const s = 10;
  const baseX = 0;
  const baseY = 0;
  const height = 153 * s;
  const width = 25 * s;
  const outline = new MX.McDbPolyline();
  [
    [baseX, baseY],
    [baseX + width, baseY],
    [baseX + width, baseY + height],
    [baseX, baseY + height],
  ].forEach(([x, y]) => outline.addVertexAt(new MX.McGePoint3d(x, y, 0)));
  outline.isClosed = true;
  outline.trueColor = new MX.McCmColor(0, 0, 0);
  entities.push(outline);

  // Rebar dots (N3) at 12.5cm spacing (9*12.5 shown; we draw 9 dots)
  for (let i = 0; i < 9; i++) {
    const y = baseY + (35 + i * 12.5) * s;
    const c = new MX.McDbCircle(new MX.McGePoint3d(baseX + width / 2, y, 0), 4);
    c.trueColor = new MX.McCmColor(220, 30, 30);
    entities.push(c);
  }

  // Callouts
  const callouts = [
    { text: 'N1 Φ14', x: baseX + width + 180, y: baseY + height - 80 },
    { text: 'N2 Φ14', x: baseX + width + 180, y: baseY + height - 110 },
    { text: 'N3 Φ12', x: baseX + width + 180, y: baseY + height - 140 },
    { text: 'N4 φ8', x: baseX + width + 180, y: baseY + height - 170 },
    { text: '^140mm 钢管', x: baseX + width + 180, y: baseY + height - 200 },
  ];
  callouts.forEach((c) => {
    const tx = new MX.McDbText();
    tx.textString = c.text;
    tx.height = 12;
    tx.position = new MX.McGePoint3d(c.x, c.y, 0);
    tx.trueColor = new MX.McCmColor(0, 0, 0);
    entities.push(tx);
  });

  // Quantity table
  if (drainagePainter?.drawRebarQuantityTable) {
    entities.push(...drainagePainter.drawRebarQuantityTable(window.CurbStoneQuantityTable, { x: 1200, y: 900 }));
  }

  // Notes
  (window.CurbStoneNotes || []).forEach((n, idx) => {
    const tx = new MX.McDbText();
    tx.textString = n;
    tx.height = 10;
    tx.position = new MX.McGePoint3d(0, -200 - idx * 18, 0);
    tx.trueColor = new MX.McCmColor(0, 0, 0);
    entities.push(tx);
  });

  // Title block reuse
  if (drainagePainter?.drawTitleBlock) {
    entities.push(...drainagePainter.drawTitleBlock(window.CurbStoneProjectInfo, { x: 3200, y: 200 }));
  }

  const basePoint = new MX.McGePoint3d(0, 0, 0);
  window.createAndDrawBlock('CurbStoneDrawing', entities, basePoint, basePoint);
};
