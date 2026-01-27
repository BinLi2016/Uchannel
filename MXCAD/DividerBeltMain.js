// Main entrypoints for drawing divider belt screenshots.

window.drawDividerBeltLayoutSheet = function () {
  const MX = window.MxCAD || {};
  const mxcad = MX.MxCpp && typeof MX.MxCpp.getCurrentMxCAD === 'function' ? MX.MxCpp.getCurrentMxCAD() : null;
  if (!mxcad) {
    console.error('MXCAD not ready.');
    return;
  }

  const painter = window.DividerBeltPainter;
  const drainagePainter = window.DrainageChannelPainter;

  const entities = [];

  // Title
  entities.push(painter._helpers.text(MX, '机非分隔带钢筋布置图', { x: 2000, y: 1600 }, 30, { r: 0, g: 0, b: 0 }, { center: true }));

  // Diagrams
  entities.push(...painter.drawLayoutDiagrams({ x: 0, y: 1000 }));

  // Parameter table
  entities.push(...painter.drawParamTable(window.DividerBeltParamHeaders, window.DividerBeltParamRows, { x: 0, y: 650 }));

  // Notes
  const notes = window.DividerBeltNotes || [];
  const notesX = 3200;
  const notesY = 600;
  entities.push(painter._helpers.text(MX, '附注:', { x: notesX, y: notesY }, 14, { r: 0, g: 0, b: 0 }));
  notes.forEach((n, i) => {
    entities.push(painter._helpers.text(MX, n, { x: notesX, y: notesY - 24 - i * 18 }, 10, { r: 0, g: 0, b: 0 }));
  });

  // Title block (reuse drainage title block renderer)
  if (drainagePainter?.drawTitleBlock) {
    entities.push(...drainagePainter.drawTitleBlock(window.DividerBeltProjectInfo, { x: 3200, y: -50 }));
  }

  const basePoint = new MX.McGePoint3d(0, 0, 0);
  window.createAndDrawBlock('DividerBeltLayout', entities, basePoint, basePoint);
};

window.drawDividerBeltQuantityTables = function () {
  const MX = window.MxCAD || {};
  const mxcad = MX.MxCpp && typeof MX.MxCpp.getCurrentMxCAD === 'function' ? MX.MxCpp.getCurrentMxCAD() : null;
  if (!mxcad) {
    console.error('MXCAD not ready.');
    return;
  }

  const painter = window.DrainageChannelPainter;
  if (!painter?.drawRebarQuantityTable) {
    console.error('DrainageChannelPainter.drawRebarQuantityTable missing.');
    return;
  }

  const entities = [];
  const types = ['YU1', 'YU2', 'YU3', 'YU4', 'YU5', 'YU6'];
  const startX = 0;
  let startY = 900;

  types.forEach((t) => {
    const table = window.DividerBeltQuantityByType?.[t];
    if (table) {
      entities.push(...painter.drawRebarQuantityTable(table, { x: startX, y: startY }));
      startY -= 360;
    }
  });

  const basePoint = new MX.McGePoint3d(0, 0, 0);
  window.createAndDrawBlock('DividerBeltQuantityTables', entities, basePoint, basePoint);
};

window.drawDividerBeltFromSample = function () {
  // For verification we only need deterministic drawings.
  window.drawDividerBeltLayoutSheet();
};
