// Pump station non-motor lane drainage drawing.

window.drawPumpStationDrainageSheet = function (payload) {
  const MX = window.MxCAD || {};
  const mxcad = MX.MxCpp && typeof MX.MxCpp.getCurrentMxCAD === 'function' ? MX.MxCpp.getCurrentMxCAD() : null;
  if (!mxcad) {
    console.error('MXCAD not ready.');
    return;
  }

  const painter = window.DrainageChannelPainter;
  if (!painter) {
    console.error('DrainageChannelPainter missing.');
    return;
  }

  const entities = [];

  // Title
  const title = new MX.McDbText();
  title.textString = '泵站非机动车道排水边沟及盖板配筋图';
  title.height = 26;
  title.position = new MX.McGePoint3d(2200, 1650, 0);
  title.horizontalMode = MX.McDb.TextHorzMode.kTextCenter;
  title.verticalMode = MX.McDb.TextVertMode.kTextVertMid;
  title.alignmentPoint = title.position;
  title.trueColor = new MX.McCmColor(0, 0, 0);
  entities.push(title);

  // Two channel section variants
  if (payload?.section1) {
    entities.push(...painter.drawMotorLaneChannelSection(payload.section1, { x: 0, y: 900 }, 1.0));
  }
  if (payload?.section2) {
    entities.push(...painter.drawMotorLaneChannelSection(payload.section2, { x: 900, y: 900 }, 1.0));
  }

  // Perforated cover plan (schematic)
  const cover = payload?.cover;
  if (cover) {
    const ox = 2100;
    const oy = 900;
    const w = cover.widthMm;
    const l = cover.lengthMm;
    const outline = new MX.McDbPolyline();
    [
      [ox, oy],
      [ox + w, oy],
      [ox + w, oy + l],
      [ox, oy + l],
    ].forEach(([x, y]) => outline.addVertexAt(new MX.McGePoint3d(x, y, 0)));
    outline.isClosed = true;
    outline.trueColor = new MX.McCmColor(0, 0, 0);
    entities.push(outline);

    const slots = cover.slots;
    if (slots?.count) {
      const slotW = slots.slotWidthMm;
      const slotL = slots.slotLengthMm;
      const count = slots.count;
      const gap = (w - count * slotW) / (count + 1);
      for (let i = 0; i < count; i++) {
        const sx = ox + gap + i * (slotW + gap);
        const sy = oy + (l - slotL) / 2;
        const slot = new MX.McDbPolyline();
        [
          [sx, sy],
          [sx + slotW, sy],
          [sx + slotW, sy + slotL],
          [sx, sy + slotL],
        ].forEach(([x, y]) => slot.addVertexAt(new MX.McGePoint3d(x, y, 0)));
        slot.isClosed = true;
        slot.trueColor = new MX.McCmColor(0, 0, 0);
        entities.push(slot);
      }
    }

    const label = new MX.McDbText();
    label.textString = '边沟盖板二平面大样(示意)';
    label.height = 16;
    label.position = new MX.McGePoint3d(ox + w / 2, oy + l + 70, 0);
    label.horizontalMode = MX.McDb.TextHorzMode.kTextCenter;
    label.verticalMode = MX.McDb.TextVertMode.kTextVertMid;
    label.alignmentPoint = label.position;
    label.trueColor = new MX.McCmColor(0, 0, 0);
    entities.push(label);
  }

  // Tables
  // Keep Y positive so zoomAll reliably includes the table/notes extents.
  if (payload?.channelTable) {
    entities.push(...painter.drawRebarQuantityTable(payload.channelTable, { x: 0, y: 400 }));
  }
  if (payload?.coverTable) {
    entities.push(...painter.drawRebarQuantityTable(payload.coverTable, { x: 1300, y: 400 }));
  }

  // Notes + title block
  if (payload?.projectInfo && painter.drawTitleBlock) {
    // Keep title block close to tables so it stays in view after zoom.
    entities.push(...painter.drawTitleBlock(payload.projectInfo, { x: 2250, y: 420 }));
  }
  if (payload?.notes && painter.drawNotes) {
    entities.push(...painter.drawNotes(payload.notes, { x: 0, y: 180 }));
  }

  const dimEntities = entities.filter((e) => e && e._isDimension);
  const blockEntities = entities.filter((e) => !e || !e._isDimension);
  const basePoint = new MX.McGePoint3d(0, 0, 0);
  window.createAndDrawBlock('PumpStationDrainageDrawing', blockEntities, basePoint, basePoint);
  dimEntities.forEach((dim) => mxcad.drawEntity(dim));
};

window.drawPumpStationDrainageFromSample = async function () {
  try {
    const resp = await fetch('PumpStationDrainageSample.json');
    const payload = await resp.json();
    window.drawPumpStationDrainageSheet(payload);
  } catch (e) {
    console.error('Failed to load PumpStationDrainageSample.json', e);
  }
};
