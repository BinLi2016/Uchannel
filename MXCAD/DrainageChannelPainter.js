/**
 * Drainage Channel Painter Module
 * Renders all components of the drainage channel reinforcement drawing
 */

window.DrainageChannelPainter = {

    _helpers: {
        line: function (MX, start, end, color) {
            const { McDbLine, McGePoint3d, McCmColor } = MX;
            const ln = new McDbLine(new McGePoint3d(start.x, start.y, 0), new McGePoint3d(end.x, end.y, 0));
            if (color) ln.trueColor = new McCmColor(color.r, color.g, color.b);
            return ln;
        },
        polyline: function (MX, points, isClosed, color) {
            const { McDbPolyline, McGePoint3d, McCmColor } = MX;
            const pl = new McDbPolyline();
            points.forEach(pt => pl.addVertexAt(new McGePoint3d(pt.x, pt.y, 0)));
            pl.isClosed = !!isClosed;
            if (color) pl.trueColor = new McCmColor(color.r, color.g, color.b);
            return pl;
        },
        text: function (MX, textString, position, height, color, opts) {
            const { McDbText, McGePoint3d, McCmColor, McDb } = MX;
            const tx = new McDbText();
            tx.textString = textString;
            tx.height = height;
            tx.position = new McGePoint3d(position.x, position.y, 0);
            if (opts?.center) {
                tx.horizontalMode = McDb.TextHorzMode.kTextCenter;
                tx.verticalMode = McDb.TextVertMode.kTextVertMid;
                tx.alignmentPoint = tx.position;
            }
            if (opts?.rotationRad) {
                tx.rotation = opts.rotationRad;
            }
            if (color) tx.trueColor = new McCmColor(color.r, color.g, color.b);
            return tx;
        },
        dimensionAligned: function (MX, start, end, linePoint, text, color) {
            const { McDbAlignedDimension, McGePoint3d, McCmColor } = MX;
            if (!McDbAlignedDimension) return null;
            const dim = new McDbAlignedDimension();
            dim.xLine1Point = new McGePoint3d(start.x, start.y, 0);
            dim.xLine2Point = new McGePoint3d(end.x, end.y, 0);
            dim.dimLinePoint = new McGePoint3d(linePoint.x, linePoint.y, 0);
            if (text) dim.dimensionText = text;
            if (color) dim.trueColor = new McCmColor(color.r, color.g, color.b);
            dim._isDimension = true;
            // 应用统一的标注比例设置
            if (typeof window.applyDimensionScale === 'function') {
                window.applyDimensionScale(dim);
            }
            if (typeof dim.needToUpdateDimBlock === 'function') dim.needToUpdateDimBlock(true);
            if (typeof dim.recomputeDimBlock === 'function') dim.recomputeDimBlock();
            return dim;
        },
        dimensionRotated: function (MX, start, end, linePoint, rotation, text, color) {
            const { McDbRotatedDimension, McGePoint3d, McCmColor } = MX;
            if (!McDbRotatedDimension) return null;
            const dim = new McDbRotatedDimension();
            dim.xLine1Point = new McGePoint3d(start.x, start.y, 0);
            dim.xLine2Point = new McGePoint3d(end.x, end.y, 0);
            dim.dimLinePoint = new McGePoint3d(linePoint.x, linePoint.y, 0);
            dim.rotation = rotation || 0;
            if (text) dim.dimensionText = text;
            if (color) dim.trueColor = new McCmColor(color.r, color.g, color.b);
            dim._isDimension = true;
            // 应用统一的标注比例设置
            if (typeof window.applyDimensionScale === 'function') {
                window.applyDimensionScale(dim);
            }
            if (typeof dim.needToUpdateDimBlock === 'function') dim.needToUpdateDimBlock(true);
            if (typeof dim.recomputeDimBlock === 'function') dim.recomputeDimBlock();
            return dim;
        },
        normalizePointsMm: function (pointsMm, origin, scale) {
            return (pointsMm || []).map(p => ({
                x: origin.x + p.x * scale,
                y: origin.y + p.y * scale,
            }));
        },
    },

    /**
     * Motor-lane drainage channel cross-section (水沟配筋图)
     */
    drawMotorLaneChannelSection: function (section, position, scale = 1.0) {
        const MX = window.MxCAD || {};
        const { McCmColor } = MX;
        const entities = [];

        const ox = position.x;
        const oy = position.y;

        // Dimensions (mm)
        const w = section.outerWidthMm;
        const h = section.outerHeightMm;
        const wall = section.wallThicknessMm;
        const base = section.baseThicknessMm;
        const lip = section.topOuterLipWidthMm;
        const seatW = section.topSeatWidthMm;
        const seatD = section.topSeatDepthMm;

        const leftSeatOuterX = lip;
        const rightSeatOuterX = w - lip;
        const leftInnerWallX = wall;
        const rightInnerWallX = w - wall;
        const seatBottomY = h - seatD;

        const outlineColor = { r: 0, g: 160, b: 220 };
        const rebarColor = { r: 220, g: 30, b: 30 };
        const dimColor = { r: 0, g: 0, b: 0 };
        const cover = 30;

        // Concrete outline (outer)
        entities.push(this._helpers.line(MX, { x: ox, y: oy }, { x: ox + w * scale, y: oy }, outlineColor));
        entities.push(this._helpers.line(MX, { x: ox, y: oy }, { x: ox, y: oy + h * scale }, outlineColor));
        entities.push(this._helpers.line(MX, { x: ox + w * scale, y: oy }, { x: ox + w * scale, y: oy + h * scale }, outlineColor));
        entities.push(this._helpers.line(MX, { x: ox, y: oy + h * scale }, { x: ox + leftSeatOuterX * scale, y: oy + h * scale }, outlineColor));
        entities.push(this._helpers.line(MX, { x: ox + rightSeatOuterX * scale, y: oy + h * scale }, { x: ox + w * scale, y: oy + h * scale }, outlineColor));

        // Void outline (inner cavity + top seat pocket)
        const voidPts = [
            { x: leftSeatOuterX, y: h },
            { x: leftSeatOuterX, y: h - seatD },
            { x: leftInnerWallX, y: h - seatD },
            { x: leftInnerWallX, y: base },
            { x: rightInnerWallX, y: base },
            { x: rightInnerWallX, y: h - seatD },
            { x: rightSeatOuterX, y: h - seatD },
            { x: rightSeatOuterX, y: h },
        ].map(p => ({ x: ox + p.x * scale, y: oy + p.y * scale }));

        for (let i = 0; i < voidPts.length - 1; i++) {
            entities.push(this._helpers.line(MX, voidPts[i], voidPts[i + 1], outlineColor));
        }

        // Section title
        entities.push(this._helpers.text(MX, "水沟配筋图", { x: ox + (w * scale) / 2, y: oy + h * scale + 60 }, 20, dimColor, { center: true }));

        // Rebar (schematic in section)
        // N1: U-shape (placed with reasonable covers to match bending diagram proportions)
        const n1Pts = [
            { x: ox + cover * scale, y: oy + cover * scale },
            { x: ox + cover * scale, y: oy + (h - cover) * scale },
            { x: ox + (w - cover) * scale, y: oy + (h - cover) * scale },
            { x: ox + (w - cover) * scale, y: oy + cover * scale }
        ];
        entities.push(this._helpers.polyline(MX, n1Pts, false, rebarColor));

        // N2: bracket bars near both top corners (schematic)
        const n2Local = [{ x: 0, y: 0 }, { x: 140, y: 0 }, { x: 140, y: 410 }, { x: 280, y: 410 }];
        const n2LeftOrigin = { x: ox + (wall + cover) * scale, y: oy + (seatBottomY - 410 - cover) * scale };
        const n2RightOrigin = { x: ox + (w - wall - cover - 280) * scale, y: oy + (seatBottomY - 410 - cover) * scale };
        entities.push(this._helpers.polyline(MX, n2Local.map(p => ({ x: n2LeftOrigin.x + p.x * scale, y: n2LeftOrigin.y + p.y * scale })), false, rebarColor));
        entities.push(this._helpers.polyline(MX, n2Local.map(p => ({ x: n2RightOrigin.x + p.x * scale, y: n2RightOrigin.y + p.y * scale })), false, rebarColor));

        // N3: longitudinal bars (shown as circles in section) placed along base
        const { McDbCircle, McGePoint3d } = MX;
        const n3Count = 5;
        const n3Y = oy + (base + cover) * scale;
        const n3StartX = ox + (wall + cover) * scale;
        const n3EndX = ox + (w - wall - cover) * scale;
        for (let i = 0; i < n3Count; i++) {
            const t = i / (n3Count - 1);
            const cx = n3StartX + (n3EndX - n3StartX) * t;
            const c = new McDbCircle(new McGePoint3d(cx, n3Y, 0), 6 * scale);
            c.trueColor = new McCmColor(rebarColor.r, rebarColor.g, rebarColor.b);
            entities.push(c);
        }

        // Callouts
        entities.push(this._helpers.text(MX, "N1 Φ18@100", { x: ox + w * scale + 160, y: oy + h * scale - 40 }, 14, dimColor));
        entities.push(this._helpers.text(MX, "N2 Φ12@100", { x: ox + w * scale + 160, y: oy + h * scale - 70 }, 14, dimColor));
        entities.push(this._helpers.text(MX, "N3 Φ12", { x: ox + w * scale + 160, y: oy + h * scale - 100 }, 14, dimColor));

        // Key dimensions in cm (as in reference)
        const cm = (mm) => (mm / 10).toString();
        const dim1 = this._helpers.dimensionAligned(MX, { x: ox, y: oy }, { x: ox + w * scale, y: oy }, { x: ox + (w * scale) / 2, y: oy - 70 }, cm(w), dimColor);
        const dim2 = this._helpers.dimensionAligned(MX, { x: ox + leftInnerWallX * scale, y: oy }, { x: ox + rightInnerWallX * scale, y: oy }, { x: ox + (w * scale) / 2, y: oy - 110 }, cm(section.innerWidthMm), dimColor);
        const dim3 = this._helpers.dimensionRotated(MX, { x: ox, y: oy }, { x: ox, y: oy + base * scale }, { x: ox - 80, y: oy + (base * scale) / 2 }, Math.PI / 2, cm(base), dimColor);
        const dim4 = this._helpers.dimensionRotated(MX, { x: ox + w * scale, y: oy + base * scale }, { x: ox + w * scale, y: oy + (base + section.innerHeightMm) * scale }, { x: ox + w * scale + 80, y: oy + (base + section.innerHeightMm / 2) * scale }, Math.PI / 2, cm(section.innerHeightMm), dimColor);
        const dim5 = this._helpers.dimensionRotated(MX, { x: ox + w * scale, y: oy }, { x: ox + w * scale, y: oy + h * scale }, { x: ox + w * scale + 130, y: oy + (h / 2) * scale }, Math.PI / 2, cm(h), dimColor);
        const dim6 = this._helpers.dimensionAligned(MX, { x: ox, y: oy + h * scale }, { x: ox + (lip + seatW) * scale, y: oy + h * scale }, { x: ox + (lip + seatW / 2) * scale, y: oy + h * scale + 20 }, "4+6=10", dimColor);
        const dim7 = this._helpers.dimensionAligned(MX, { x: ox + (w - lip - seatW) * scale, y: oy + h * scale }, { x: ox + w * scale, y: oy + h * scale }, { x: ox + (w - (lip + seatW / 2)) * scale, y: oy + h * scale + 20 }, "6+4=10", dimColor);
        [dim1, dim2, dim3, dim4, dim5, dim6, dim7].forEach((d) => { if (d) entities.push(d); });

        return entities;
    },

    /**
     * Motor-lane cover plate detail (水沟盖板详图)
     */
    drawMotorLaneCoverDetail: function (cover, position, scale = 1.0) {
        const MX = window.MxCAD || {};
        const entities = [];
        const outlineColor = { r: 0, g: 160, b: 220 };
        const dimColor = { r: 0, g: 0, b: 0 };

        const ox = position.x;
        const oy = position.y;

        const w = cover.widthMm;
        const l = cover.lengthMm;
        const t = cover.thicknessMm;

        // Title
        entities.push(this._helpers.text(MX, "水沟盖板详图", { x: ox + (w * scale) / 2, y: oy + 620 }, 20, dimColor, { center: true }));

        // Plan outline (top view)
        const plan = [
            { x: ox, y: oy },
            { x: ox + w * scale, y: oy },
            { x: ox + w * scale, y: oy + l * scale },
            { x: ox, y: oy + l * scale },
        ];
        entities.push(this._helpers.polyline(MX, plan, true, outlineColor));
        const dimW = this._helpers.dimensionAligned(MX, { x: ox, y: oy }, { x: ox + w * scale, y: oy }, { x: ox + (w * scale) / 2, y: oy - 70 }, "50", dimColor);
        const dimL = this._helpers.dimensionRotated(MX, { x: ox, y: oy }, { x: ox, y: oy + l * scale }, { x: ox - 60, y: oy + (l * scale) / 2 }, Math.PI / 2, "49", dimColor);
        [dimW, dimL].forEach((d) => { if (d) entities.push(d); });

        // Section sketch (side) next to plan
        const sx = ox + (w + 220) * scale;
        const sy = oy + (l * 0.15) * scale;
        const sec = [
            { x: sx, y: sy },
            { x: sx + w * scale, y: sy },
            { x: sx + w * scale, y: sy + t * scale },
            { x: sx, y: sy + t * scale },
        ];
        entities.push(this._helpers.polyline(MX, sec, true, outlineColor));
        const dimT = this._helpers.dimensionRotated(MX, { x: sx + w * scale, y: sy }, { x: sx + w * scale, y: sy + t * scale }, { x: sx + w * scale + 50, y: sy + (t * scale) / 2 }, Math.PI / 2, "10", dimColor);
        const dimSpan = this._helpers.dimensionAligned(MX, { x: sx, y: sy }, { x: sx + w * scale, y: sy }, { x: sx + (w * scale) / 2, y: sy - 55 }, "3  4x11=44  3", dimColor);
        [dimT, dimSpan].forEach((d) => { if (d) entities.push(d); });

        return entities;
    },

    /**
     * Motor-lane cover plate reinforcement plan (水沟盖板配筋图)
     */
    drawMotorLaneCoverRebarPlan: function (cover, coverTable, position, scale = 1.0) {
        const MX = window.MxCAD || {};
        const { McDbLine, McGePoint3d, McCmColor } = MX;
        const entities = [];
        const outlineColor = { r: 0, g: 160, b: 220 };
        const rebarColor = { r: 220, g: 30, b: 30 };
        const dimColor = { r: 0, g: 0, b: 0 };

        const ox = position.x;
        const oy = position.y;
        const w = cover.widthMm;
        const l = cover.lengthMm;

        entities.push(this._helpers.text(MX, "水沟盖板配筋图", { x: ox + (w * scale) / 2, y: oy + 620 }, 20, dimColor, { center: true }));

        const outline = [
            { x: ox, y: oy },
            { x: ox + w * scale, y: oy },
            { x: ox + w * scale, y: oy + l * scale },
            { x: ox, y: oy + l * scale },
        ];
        entities.push(this._helpers.polyline(MX, outline, true, outlineColor));

        // M2 (vertical bars): 5 bars, 4 spaces * 85 = 340 mm
        const spanX = 340;
        const startX = (w - spanX) / 2;
        const dx = 85;
        for (let i = 0; i < 5; i++) {
            const x = ox + (startX + i * dx) * scale;
            const y1 = oy + 25 * scale;
            const y2 = oy + (l - 25) * scale;
            const ln = new McDbLine(new McGePoint3d(x, y1, 0), new McGePoint3d(x, y2, 0));
            ln.trueColor = new McCmColor(rebarColor.r, rebarColor.g, rebarColor.b);
            entities.push(ln);
        }

        // M1 (horizontal bars): 7 bars, 6 spaces * 70 = 420 mm
        const spanY = 420;
        const startY = (l - spanY) / 2;
        const dy = 70;
        for (let i = 0; i < 7; i++) {
            const y = oy + (startY + i * dy) * scale;
            const x1 = ox + 20 * scale;
            const x2 = ox + (w - 20) * scale;
            const ln = new McDbLine(new McGePoint3d(x1, y, 0), new McGePoint3d(x2, y, 0));
            ln.trueColor = new McCmColor(rebarColor.r, rebarColor.g, rebarColor.b);
            entities.push(ln);
        }

        const dimSpanX = this._helpers.dimensionAligned(MX, { x: ox + startX * scale, y: oy }, { x: ox + (startX + spanX) * scale, y: oy }, { x: ox + (w * scale) / 2, y: oy + l * scale + 25 }, "4x8.5=34", dimColor);
        const dimSpanY = this._helpers.dimensionRotated(MX, { x: ox, y: oy + startY * scale }, { x: ox, y: oy + (startY + spanY) * scale }, { x: ox + w * scale + 55, y: oy + (l * scale) / 2 }, Math.PI / 2, "6x7=42", dimColor);
        [dimSpanX, dimSpanY].forEach((d) => { if (d) entities.push(d); });
        entities.push(this._helpers.text(MX, "M1 Φ16", { x: ox + w * scale + 120, y: oy + l * scale - 40 }, 14, dimColor));
        entities.push(this._helpers.text(MX, "M2 Φ16", { x: ox + w * scale + 120, y: oy + l * scale - 70 }, 14, dimColor));
        entities.push(this._helpers.text(MX, "M3 φ8", { x: ox + w * scale + 120, y: oy + l * scale - 100 }, 14, dimColor));

        return entities;
    },

    /**
     * Table: 编号/图式/直径/每根长/根数/总长/单位重/总重
     */
    drawRebarQuantityTable: function (tableData, position) {
        const MX = window.MxCAD || {};
        const { McDbPolyline, McGePoint3d, McCmColor, McDbText, McDb } = MX;
        const entities = [];

        // Use a neutral stroke/text color so tables remain visible
        // regardless of whether the CAD viewport background is dark or light.
        const strokeColor = new McCmColor(220, 220, 220);

        const startX = position.x;
        const startY = position.y;
        const colWidths = [70, 140, 90, 110, 80, 110, 110, 110];
        const rowHeight = 38;
        const titleHeight = 55;

        // Title
        const titleText = new McDbText();
        titleText.textString = tableData.title;
        titleText.height = 20;
        titleText.position = new McGePoint3d(startX + colWidths.reduce((a, b) => a + b, 0) / 2, startY + titleHeight / 2, 0);
        titleText.horizontalMode = McDb.TextHorzMode.kTextCenter;
        titleText.verticalMode = McDb.TextVertMode.kTextVertMid;
        titleText.alignmentPoint = titleText.position;
        titleText.trueColor = strokeColor;
        entities.push(titleText);

        let currentY = startY;

        // Headers
        tableData.headers.forEach((header, i) => {
            const x = startX + colWidths.slice(0, i).reduce((a, b) => a + b, 0);
            const rect = new McDbPolyline();
            rect.addVertexAt(new McGePoint3d(x, currentY, 0));
            rect.addVertexAt(new McGePoint3d(x + colWidths[i], currentY, 0));
            rect.addVertexAt(new McGePoint3d(x + colWidths[i], currentY - rowHeight, 0));
            rect.addVertexAt(new McGePoint3d(x, currentY - rowHeight, 0));
            rect.isClosed = true;
            rect.trueColor = strokeColor;
            entities.push(rect);

            const text = new McDbText();
            text.textString = header;
            text.height = 10;
            text.position = new McGePoint3d(x + colWidths[i] / 2, currentY - rowHeight / 2, 0);
            text.horizontalMode = McDb.TextHorzMode.kTextCenter;
            text.verticalMode = McDb.TextVertMode.kTextVertMid;
            text.alignmentPoint = text.position;
            text.trueColor = strokeColor;
            entities.push(text);
        });

        currentY -= rowHeight;

        const fmt = (n, digits) => {
            if (typeof n !== 'number' || Number.isNaN(n)) return '';
            return n.toFixed(digits);
        };

        // Rows
        tableData.items.forEach(item => {
            const row = [
                item.id,
                '',
                item.diameterMm?.toString?.() || '',
                fmt(item.lengthPerBarM, 3),
                item.quantity?.toString?.() || '',
                fmt(item.totalLengthM, 3),
                fmt(item.unitWeightKgPerM, 3),
                fmt(item.totalWeightKg, 2),
            ];

            row.forEach((cell, colIndex) => {
                const x = startX + colWidths.slice(0, colIndex).reduce((a, b) => a + b, 0);

                const rect = new McDbPolyline();
                rect.addVertexAt(new McGePoint3d(x, currentY, 0));
                rect.addVertexAt(new McGePoint3d(x + colWidths[colIndex], currentY, 0));
                rect.addVertexAt(new McGePoint3d(x + colWidths[colIndex], currentY - rowHeight, 0));
                rect.addVertexAt(new McGePoint3d(x, currentY - rowHeight, 0));
                rect.isClosed = true;
                rect.trueColor = strokeColor;
                entities.push(rect);

                if (colIndex === 1) {
                    // 图式: draw a tiny shape if available
                    const shape = item.bendShape;
                    if (shape?.pointsMm?.length >= 2) {
                        const pad = 8;
                        const minX = Math.min(...shape.pointsMm.map(p => p.x));
                        const maxX = Math.max(...shape.pointsMm.map(p => p.x));
                        const minY = Math.min(...shape.pointsMm.map(p => p.y));
                        const maxY = Math.max(...shape.pointsMm.map(p => p.y));
                        const bw = Math.max(maxX - minX, 1);
                        const bh = Math.max(maxY - minY, 1);
                        const availW = colWidths[colIndex] - 2 * pad;
                        const availH = rowHeight - 2 * pad;
                        const s = Math.min(availW / bw, availH / bh);
                        const pts = shape.pointsMm.map(p => ({
                            x: x + pad + (p.x - minX) * s,
                            y: (currentY - rowHeight) + pad + (p.y - minY) * s,
                        }));
                        const pl = new McDbPolyline();
                        pts.forEach(pt => pl.addVertexAt(new McGePoint3d(pt.x, pt.y, 0)));
                        pl.isClosed = false;
                        pl.trueColor = strokeColor;
                        entities.push(pl);
                    }
                } else {
                    const text = new McDbText();
                    text.textString = cell;
                    text.height = 10;
                    text.position = new McGePoint3d(x + colWidths[colIndex] / 2, currentY - rowHeight / 2, 0);
                    text.horizontalMode = McDb.TextHorzMode.kTextCenter;
                    text.verticalMode = McDb.TextVertMode.kTextVertMid;
                    text.alignmentPoint = text.position;
                    text.trueColor = strokeColor;
                    entities.push(text);
                }
            });

            currentY -= rowHeight;
        });

        // Summary
        currentY -= 10;
        entities.push(this._helpers.text(MX, `${tableData.concreteText}  ${tableData.totalSteelText}`, { x: startX + 10, y: currentY - 15 }, 12, { r: 220, g: 220, b: 220 }));

        return entities;
    },

    /**
     * Bending diagrams (N1/N2) placed as standalone views.
     */
    drawBendingDiagrams: function (items, position, scale = 1.0) {
        const MX = window.MxCAD || {};
        const entities = [];
        const outlineColor = { r: 220, g: 220, b: 220 };
        const dimColor = { r: 220, g: 220, b: 220 };

        const startX = position.x;
        const startY = position.y;
        const gapX = 360;

        items.forEach((item, idx) => {
            const x0 = startX + idx * gapX;
            const y0 = startY;
            entities.push(this._helpers.text(MX, item.id, { x: x0, y: y0 + 260 }, 16, dimColor));

            const shape = item.bendShape;
            if (!shape?.pointsMm?.length) return;

            const pts = shape.pointsMm.map(p => ({ x: x0 + p.x * scale, y: y0 + p.y * scale }));
            entities.push(this._helpers.polyline(MX, pts, false, outlineColor));

            // Segment dimension entities
            (shape.segmentDimsMmText || []).forEach((t, i) => {
                const p1 = pts[i];
                const p2 = pts[i + 1];
                if (!p1 || !p2) return;
                const linePoint = { x: (p1.x + p2.x) / 2, y: y0 - 30 };
                const dimEntity = this._helpers.dimensionAligned(MX, p1, p2, linePoint, t, dimColor);
                if (dimEntity) entities.push(dimEntity);
            });
        });

        return entities;
    },

    /**
     * Draw cross-section view
     */
    drawCrossSectionView: function (geometry, rebarPositions, dimensions, position, scale = 1.0) {
        const MX = window.MxCAD || {};
        const { McDbPolyline, McGePoint3d, McCmColor, McDbCircle, McDbText, McDb } = MX;
        const entities = [];

        const offsetX = position.x;
        const offsetY = position.y;

        // Draw outer contour
        const outerPoly = new McDbPolyline();
        geometry.outerContour.forEach(pt => {
            outerPoly.addVertexAt(new McGePoint3d(offsetX + pt.x * scale, offsetY + pt.y * scale, 0));
        });
        outerPoly.isClosed = true;
        outerPoly.trueColor = new McCmColor(255, 255, 255); // White
        entities.push(outerPoly);

        // Draw inner contour
        const innerPoly = new McDbPolyline();
        geometry.innerContour.forEach(pt => {
            innerPoly.addVertexAt(new McGePoint3d(offsetX + pt.x * scale, offsetY + pt.y * scale, 0));
        });
        innerPoly.isClosed = false; // Open at top
        innerPoly.trueColor = new McCmColor(255, 255, 255); // White
        entities.push(innerPoly);

        // Draw rebars as circles (cross-section view)
        rebarPositions.forEach(rebar => {
            if (rebar.type === 'point') {
                const circle = new McDbCircle(
                    new McGePoint3d(offsetX + rebar.x * scale, offsetY + rebar.y * scale, 0),
                    rebar.diameter / 2 * scale
                );
                circle.trueColor = new McCmColor(0, 255, 0); // Green
                entities.push(circle);

                // Add rebar mark label (only for first rebar of each type)
                // We'll add labels selectively to avoid clutter
            }
        });

        // Draw dimension entities
        dimensions.forEach(dim => {
            if (dim.type === 'horizontal') {
                const dimEntity = this._helpers.dimensionAligned(MX,
                    { x: offsetX + dim.start.x * scale, y: offsetY + dim.start.y * scale },
                    { x: offsetX + dim.end.x * scale, y: offsetY + dim.end.y * scale },
                    { x: offsetX + dim.textPosition.x * scale, y: offsetY + dim.textPosition.y * scale },
                    dim.text,
                    { r: 255, g: 0, b: 0 }
                );
                if (dimEntity) entities.push(dimEntity);
            } else if (dim.type === 'vertical') {
                const dimEntity = this._helpers.dimensionRotated(MX,
                    { x: offsetX + dim.start.x * scale, y: offsetY + dim.start.y * scale },
                    { x: offsetX + dim.end.x * scale, y: offsetY + dim.end.y * scale },
                    { x: offsetX + dim.textPosition.x * scale, y: offsetY + dim.textPosition.y * scale },
                    Math.PI / 2,
                    dim.text,
                    { r: 255, g: 0, b: 0 }
                );
                if (dimEntity) entities.push(dimEntity);
            }
        });

        return entities;
    },

    /**
     * Draw plan view (cover plate)
     */
    drawPlanView: function (planGeometry, rebarGrid, position, scale = 1.0) {
        const MX = window.MxCAD || {};
        const { McDbPolyline, McGePoint3d, McCmColor, McDbLine } = MX;
        const entities = [];

        const offsetX = position.x;
        const offsetY = position.y;

        // Draw cover plate outline
        const outline = new McDbPolyline();
        planGeometry.outline.forEach(pt => {
            outline.addVertexAt(new McGePoint3d(offsetX + pt.x * scale, offsetY + pt.y * scale, 0));
        });
        outline.isClosed = true;
        outline.trueColor = new McCmColor(255, 255, 255); // White
        entities.push(outline);

        // Draw drainage openings
        planGeometry.openings.forEach(opening => {
            const openingPoly = new McDbPolyline();
            opening.points.forEach(pt => {
                openingPoly.addVertexAt(new McGePoint3d(offsetX + pt.x * scale, offsetY + pt.y * scale, 0));
            });
            openingPoly.isClosed = true;
            openingPoly.trueColor = new McCmColor(255, 255, 255); // White
            entities.push(openingPoly);
        });

        // Draw rebar grid
        rebarGrid.forEach(rebar => {
            if (rebar.type === 'line') {
                const line = new McDbLine(
                    new McGePoint3d(offsetX + rebar.start.x * scale, offsetY + rebar.start.y * scale, 0),
                    new McGePoint3d(offsetX + rebar.end.x * scale, offsetY + rebar.end.y * scale, 0)
                );
                line.trueColor = new McCmColor(0, 255, 0); // Green for rebar
                entities.push(line);
            }
        });

        return entities;
    },

    /**
     * Draw dimension table
     */
    drawDimensionTable: function (tableData, position) {
        const MX = window.MxCAD || {};
        const { McDbPolyline, McGePoint3d, McCmColor, McDbText, McDb } = MX;
        const entities = [];

        const startX = position.x;
        const startY = position.y;
        const colWidths = [60, 150, 120, 100]; // Column widths
        const rowHeight = 35;
        const titleHeight = 50;

        // Draw title
        const titleText = new McDbText();
        titleText.textString = tableData.title;
        titleText.height = 20;
        titleText.position = new McGePoint3d(
            startX + colWidths.reduce((a, b) => a + b, 0) / 2,
            startY + titleHeight / 2,
            0
        );
        titleText.horizontalMode = McDb.TextHorzMode.kTextCenter;
        titleText.verticalMode = McDb.TextVertMode.kTextVertMid;
        titleText.alignmentPoint = titleText.position;
        titleText.trueColor = new McCmColor(255, 255, 255);
        entities.push(titleText);

        let currentY = startY;

        // Draw header row
        tableData.headers.forEach((header, i) => {
            const x = startX + colWidths.slice(0, i).reduce((a, b) => a + b, 0);

            // Cell border
            const rect = new McDbPolyline();
            rect.addVertexAt(new McGePoint3d(x, currentY, 0));
            rect.addVertexAt(new McGePoint3d(x + colWidths[i], currentY, 0));
            rect.addVertexAt(new McGePoint3d(x + colWidths[i], currentY - rowHeight, 0));
            rect.addVertexAt(new McGePoint3d(x, currentY - rowHeight, 0));
            rect.isClosed = true;
            rect.trueColor = new McCmColor(255, 255, 255);
            entities.push(rect);

            // Cell text
            const text = new McDbText();
            text.textString = header;
            text.height = 12;
            text.position = new McGePoint3d(x + colWidths[i] / 2, currentY - rowHeight / 2, 0);
            text.horizontalMode = McDb.TextHorzMode.kTextCenter;
            text.verticalMode = McDb.TextVertMode.kTextVertMid;
            text.alignmentPoint = text.position;
            text.trueColor = new McCmColor(255, 255, 255);
            entities.push(text);
        });

        currentY -= rowHeight;

        // Draw data rows
        tableData.rows.forEach(row => {
            row.forEach((cell, colIndex) => {
                const x = startX + colWidths.slice(0, colIndex).reduce((a, b) => a + b, 0);

                // Cell border
                const rect = new McDbPolyline();
                rect.addVertexAt(new McGePoint3d(x, currentY, 0));
                rect.addVertexAt(new McGePoint3d(x + colWidths[colIndex], currentY, 0));
                rect.addVertexAt(new McGePoint3d(x + colWidths[colIndex], currentY - rowHeight, 0));
                rect.addVertexAt(new McGePoint3d(x, currentY - rowHeight, 0));
                rect.isClosed = true;
                rect.trueColor = new McCmColor(255, 255, 255);
                entities.push(rect);

                // Cell text
                const text = new McDbText();
                text.textString = cell.toString();
                text.height = 10;
                text.position = new McGePoint3d(x + colWidths[colIndex] / 2, currentY - rowHeight / 2, 0);
                text.horizontalMode = McDb.TextHorzMode.kTextCenter;
                text.verticalMode = McDb.TextVertMode.kTextVertMid;
                text.alignmentPoint = text.position;
                text.trueColor = new McCmColor(0, 255, 255);
                entities.push(text);
            });
            currentY -= rowHeight;
        });

        return entities;
    },

    /**
     * Draw rebar schedule table
     */
    drawRebarScheduleTable: function (tableData, position) {
        const MX = window.MxCAD || {};
        const { McDbPolyline, McGePoint3d, McCmColor, McDbText, McDb } = MX;
        const entities = [];

        const startX = position.x;
        const startY = position.y;
        const colWidths = [60, 80, 80, 100, 100, 100]; // Column widths
        const rowHeight = 40;
        const titleHeight = 50;

        // Draw title
        const titleText = new McDbText();
        titleText.textString = tableData.title;
        titleText.height = 20;
        titleText.position = new McGePoint3d(
            startX + colWidths.reduce((a, b) => a + b, 0) / 2,
            startY + titleHeight / 2,
            0
        );
        titleText.horizontalMode = McDb.TextHorzMode.kTextCenter;
        titleText.verticalMode = McDb.TextVertMode.kTextVertMid;
        titleText.alignmentPoint = titleText.position;
        titleText.trueColor = new McCmColor(255, 255, 255);
        entities.push(titleText);

        let currentY = startY;

        // Draw header row
        tableData.headers.forEach((header, i) => {
            const x = startX + colWidths.slice(0, i).reduce((a, b) => a + b, 0);

            // Cell border
            const rect = new McDbPolyline();
            rect.addVertexAt(new McGePoint3d(x, currentY, 0));
            rect.addVertexAt(new McGePoint3d(x + colWidths[i], currentY, 0));
            rect.addVertexAt(new McGePoint3d(x + colWidths[i], currentY - rowHeight, 0));
            rect.addVertexAt(new McGePoint3d(x, currentY - rowHeight, 0));
            rect.isClosed = true;
            rect.trueColor = new McCmColor(255, 255, 255);
            entities.push(rect);

            // Cell text (handle multi-line headers)
            const text = new McDbText();
            text.textString = header.replace(/\n/g, ' '); // Replace newlines with space for now
            text.height = 10;
            text.position = new McGePoint3d(x + colWidths[i] / 2, currentY - rowHeight / 2, 0);
            text.horizontalMode = McDb.TextHorzMode.kTextCenter;
            text.verticalMode = McDb.TextVertMode.kTextVertMid;
            text.alignmentPoint = text.position;
            text.trueColor = new McCmColor(255, 255, 255);
            entities.push(text);
        });

        currentY -= rowHeight;

        // Draw data rows
        tableData.rows.forEach(row => {
            row.forEach((cell, colIndex) => {
                const x = startX + colWidths.slice(0, colIndex).reduce((a, b) => a + b, 0);

                // Cell border
                const rect = new McDbPolyline();
                rect.addVertexAt(new McGePoint3d(x, currentY, 0));
                rect.addVertexAt(new McGePoint3d(x + colWidths[colIndex], currentY, 0));
                rect.addVertexAt(new McGePoint3d(x + colWidths[colIndex], currentY - rowHeight, 0));
                rect.addVertexAt(new McGePoint3d(x, currentY - rowHeight, 0));
                rect.isClosed = true;
                rect.trueColor = new McCmColor(255, 255, 255);
                entities.push(rect);

                // Cell text
                const text = new McDbText();
                text.textString = cell.toString();
                text.height = 10;
                text.position = new McGePoint3d(x + colWidths[colIndex] / 2, currentY - rowHeight / 2, 0);
                text.horizontalMode = McDb.TextHorzMode.kTextCenter;
                text.verticalMode = McDb.TextVertMode.kTextVertMid;
                text.alignmentPoint = text.position;
                text.trueColor = new McCmColor(0, 255, 255);
                entities.push(text);
            });
            currentY -= rowHeight;
        });

        // Draw summary
        if (tableData.summary) {
            currentY -= 10;
            const summaryText = new McDbText();
            summaryText.textString = `混凝土: ${tableData.summary.concrete}, HPB300: ${tableData.summary.hpb300}, HRB400: ${tableData.summary.hrb400}`;
            summaryText.height = 12;
            summaryText.position = new McGePoint3d(startX + 10, currentY - 15, 0);
            summaryText.trueColor = new McCmColor(255, 255, 0);
            entities.push(summaryText);
        }

        return entities;
    },

    /**
     * Draw title block
     */
    drawTitleBlock: function (projectInfo, position) {
        const MX = window.MxCAD || {};
        const { McDbPolyline, McGePoint3d, McCmColor, McDbText, McDb } = MX;
        const entities = [];

        const startX = position.x;
        const startY = position.y;
        const width = 300;
        const height = 150;

        // Outer border
        const border = new McDbPolyline();
        border.addVertexAt(new McGePoint3d(startX, startY, 0));
        border.addVertexAt(new McGePoint3d(startX + width, startY, 0));
        border.addVertexAt(new McGePoint3d(startX + width, startY - height, 0));
        border.addVertexAt(new McGePoint3d(startX, startY - height, 0));
        border.isClosed = true;
        border.trueColor = new McCmColor(255, 255, 255);
        entities.push(border);

        // Add project info text
        const infoItems = [
            { label: "项目名称:", value: projectInfo.projectName, y: -20 },
            { label: "图纸名称:", value: projectInfo.drawingTitle, y: -40 },
            { label: "图号:", value: projectInfo.drawingNumber, y: -60 },
            { label: "比例:", value: projectInfo.scale, y: -80 },
            { label: "设计:", value: projectInfo.designer, y: -100 },
            { label: "校核:", value: projectInfo.checker, y: -100, x: 150 },
            { label: "审批:", value: projectInfo.approver, y: -120 },
            { label: "日期:", value: projectInfo.date, y: -120, x: 150 }
        ];

        infoItems.forEach(item => {
            const text = new McDbText();
            text.textString = item.label + " " + item.value;
            text.height = 10;
            text.position = new McGePoint3d(startX + (item.x || 10), startY + item.y, 0);
            text.trueColor = new McCmColor(255, 255, 255);
            entities.push(text);
        });

        return entities;
    },

    /**
     * Draw construction notes
     */
    drawNotes: function (notes, position) {
        const MX = window.MxCAD || {};
        const { McDbText, McGePoint3d, McCmColor } = MX;
        const entities = [];

        const startX = position.x;
        const startY = position.y;
        const lineHeight = 20;

        notes.forEach((note, index) => {
            const text = new McDbText();
            text.textString = note;
            text.height = 10;
            text.position = new McGePoint3d(startX, startY - index * lineHeight, 0);
            text.trueColor = new McCmColor(255, 255, 255);
            entities.push(text);
        });

        return entities;
    }
};
