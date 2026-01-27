/**
 * U-Channel Painter Module (重构版)
 * 使用模块化组件绘制 U 形槽钢筋布置图
 * 
 * 模块依赖:
 * - core/UnitConverter.js    - 单位转换
 * - core/EntityBuilders.js   - 图元构建
 * - core/LayoutEngine.js     - 布局引擎
 * - data/ParameterMapper.js  - 参数映射
 * - views/CrossSectionView.js - 横断面视图
 * - views/PlanView.js        - 平面视图
 * - views/ElevationView.js   - 立面视图
 * - components/RebarLayer.js - 钢筋图层
 * - components/DimensionLayer.js - 标注图层
 */

// ============================================================================
// 全局配置
// ============================================================================

window.DIMENSION_SCALE_CONFIG = {
    overallDimScale: 1.0
};

/**
 * 应用标注比例设置（保持向后兼容）
 */
window.applyDimensionScale = function (dim) {
    if (!dim || typeof dim.setDimVarDouble !== 'function') return;
    dim.setDimVarDouble(40, 40.0);
};

// ============================================================================
// 向后兼容 API - getNormalizedParams
// ============================================================================

/**
 * 获取标准化的设计参数（向后兼容）
 * 推荐使用新的 ParameterMapper 类
 */
window.getNormalizedParams = function (yuType) {
    // 优先使用新的 ParameterMapper
    if (window.ParameterMapper) {
        const mapper = new window.ParameterMapper(yuType);
        const p = mapper.getParams();
        // 转换为旧格式
        return {
            id: yuType,
            outerWidth: p.totalWidth,
            n: p.rebarCountN,
            spacingBase: p.spacingD,
            a: p.edgeA,
            wallThickness: p.wallThickness,
            m: p.rebarCountM,
            b: p.spacingB,
            e1: p.edgeE1,
            hMin: p.heightMin,
            hMax: p.heightMax,
            n1: p.rebarCountN1,
            spacingOuter: p.spacingD1,
            spacingInner: p.spacingD2,
            e2: p.edgeE2,
            bottomThick: p.bottomThickness,
            n1_prime: p.rebarCountN1Prime,
            spacingBottom: p.spacingD1Prime,
            length1: p.length1,
            m1: p.rebarCountM1,
            b1: p.spacingB1,
            length2: p.length2,
            m2: p.rebarCountM2,
            b2: p.spacingB2,
            e3: p.edgeE3
        };
    }

    // 回退到原始实现
    const data = window.UChannelTable1Data || [];
    const headers = window.UChannelTable1Headers || [];
    const row = data.find(r => r[0] === yuType);
    if (!row) return null;

    const getVal = (header) => {
        const idx = headers.indexOf(header);
        const val = idx >= 0 ? Number(row[idx]) : 0;
        return isNaN(val) ? 0 : val;
    };

    return {
        id: yuType,
        outerWidth: getVal("D (cm)"),
        n: getVal("n"),
        spacingBase: getVal("d (cm)") * 10,
        a: getVal("a (cm)") * 10,
        wallThickness: getVal("B (cm)") * 10,
        m: getVal("m"),
        b: getVal("b (cm)") * 10,
        e1: getVal("e1 (cm)") * 10,
        hMin: getVal("Hmin (cm)") * 10,
        hMax: getVal("Hmax (cm)") * 10,
        n1: getVal("n1"),
        spacingOuter: getVal("d1 (cm)") * 10,
        spacingInner: getVal("d2 (cm)") * 10,
        e2: getVal("e2 (cm)") * 10,
        bottomThick: getVal("l (cm)") * 10,
        n1_prime: getVal("n1'"),
        spacingBottom: getVal("d1' (cm)") * 10,
        length1: getVal("L1 (cm)") * 10,
        m1: getVal("m1"),
        b1: getVal("b1 (cm)") * 10,
        length2: getVal("L2 (cm)") * 10,
        m2: getVal("m2"),
        b2: getVal("b2 (cm)") * 10,
        e3: getVal("e3 (cm)") * 10
    };
};

// ============================================================================
// 横断面图元构建（向后兼容 + 新模块支持）
// ============================================================================

window.buildUChannelCrossSectionEntities = function (data, offsetX = 0, offsetY = 0, params = null) {
    const MX = window.MxCAD || {};
    const { McDbPolyline, McGePoint3d, McCmColor, McDbText, McDbCircle, McDbAlignedDimension, McDbRotatedDimension } = MX;
    const geometry = data?.Geometry;
    
    if (!geometry) {
        return { entities: [], basePoint: new McGePoint3d(offsetX, offsetY, 0) };
    }

    const entities = [];
    const addPt = (pt) => new McGePoint3d(pt.X + offsetX, pt.Y + offsetY, 0);

    // 标注构建辅助函数
    const buildAlignedDim = (start, end, linePoint, text, color) => {
        if (!McDbAlignedDimension) return null;
        const dim = new McDbAlignedDimension();
        dim.xLine1Point = new McGePoint3d(start.X + offsetX, start.Y + offsetY, 0);
        dim.xLine2Point = new McGePoint3d(end.X + offsetX, end.Y + offsetY, 0);
        dim.dimLinePoint = new McGePoint3d(linePoint.X + offsetX, linePoint.Y + offsetY, 0);
        if (text) dim.dimensionText = text;
        if (color) dim.trueColor = new McCmColor(color.r, color.g, color.b);
        dim._isDimension = true;
        window.applyDimensionScale(dim);
        if (typeof dim.needToUpdateDimBlock === 'function') dim.needToUpdateDimBlock(true);
        if (typeof dim.recomputeDimBlock === 'function') dim.recomputeDimBlock();
        return dim;
    };

    const buildRotatedDim = (start, end, linePoint, rotation, text, color) => {
        if (!McDbRotatedDimension) return null;
        const dim = new McDbRotatedDimension();
        dim.xLine1Point = new McGePoint3d(start.X + offsetX, start.Y + offsetY, 0);
        dim.xLine2Point = new McGePoint3d(end.X + offsetX, end.Y + offsetY, 0);
        dim.dimLinePoint = new McGePoint3d(linePoint.X + offsetX, linePoint.Y + offsetY, 0);
        dim.rotation = rotation || 0;
        if (text) dim.dimensionText = text;
        if (color) dim.trueColor = new McCmColor(color.r, color.g, color.b);
        dim._isDimension = true;
        window.applyDimensionScale(dim);
        if (typeof dim.needToUpdateDimBlock === 'function') dim.needToUpdateDimBlock(true);
        if (typeof dim.recomputeDimBlock === 'function') dim.recomputeDimBlock();
        return dim;
    };

    // 外轮廓
    const outerPoly = new McDbPolyline();
    geometry.OuterContour.forEach((pt) => outerPoly.addVertexAt(addPt(pt)));
    outerPoly.isClosed = true;
    outerPoly.trueColor = new McCmColor(0, 0, 0);
    entities.push(outerPoly);

    // 内轮廓
    const innerPoly = new McDbPolyline();
    geometry.InnerContour.forEach((pt) => innerPoly.addVertexAt(addPt(pt)));
    innerPoly.isClosed = false;
    innerPoly.trueColor = new McCmColor(0, 0, 0);
    entities.push(innerPoly);

    // 钢筋 + 标签
    // 注意：为了在图纸上可见，钢筋的视觉尺寸需要放大
    const REBAR_VISUAL_SCALE = 3;  // 钢筋视觉放大倍数
    
    geometry.Rebars.forEach((rebar) => {
        const shouldLabel = rebar.ShowLabel !== false;
        let diameter = rebar.Diameter;
        if (rebar.ID === 'N1') diameter = 14;
        if (rebar.ID === 'N2') diameter = 12;
        if (rebar.ID === 'N3' || rebar.ID === 'N4') diameter = 14;
        
        // 视觉直径（放大后）
        const visualDiameter = diameter * REBAR_VISUAL_SCALE;

        if (rebar.Type === 'point' && McDbCircle) {
            rebar.Points.forEach((pt, idx) => {
                const center = addPt(pt);
                const circle = new McDbCircle(center, visualDiameter / 2);
                circle.trueColor = new McCmColor(255, 0, 0);
                entities.push(circle);

                if (idx === 0 && shouldLabel) {
                    const label = new McDbText();
                    label.textString = rebar.ID;
                    label.height = 60;  // 放大标签
                    label.position = new McGePoint3d(center.x + 30, center.y + 30, 0);
                    label.trueColor = new McCmColor(255, 0, 0);
                    entities.push(label);
                }
            });
            return;
        }

        const rebarPoly = new McDbPolyline();
        rebar.Points.forEach((pt) => rebarPoly.addVertexAt(addPt(pt)));
        rebarPoly.trueColor = new McCmColor(255, 0, 0);
        rebarPoly.constantWidth = visualDiameter;  // 使用放大后的视觉宽度
        entities.push(rebarPoly);

        if (rebar.Points.length > 0 && shouldLabel) {
            const label = new McDbText();
            label.textString = rebar.ID;
            label.height = 60;  // 放大标签
            label.position = new McGePoint3d(rebar.Points[0].X + offsetX - 80, rebar.Points[0].Y + offsetY + 80, 0);
            label.trueColor = new McCmColor(255, 0, 0);
            entities.push(label);
        }
    });

    // 标注
    if (geometry.Dimensions) {
        geometry.Dimensions.forEach((dim) => {
            const color = { r: 0, g: 0, b: 0 };
            const rotation = dim.Rotation || 0;
            const linePoint = dim.TextPosition || dim.Start;
            const dimEntity = Math.abs(rotation) > 0.001
                ? buildRotatedDim(dim.Start, dim.End, linePoint, rotation, dim.Text, color)
                : buildAlignedDim(dim.Start, dim.End, linePoint, dim.Text, color);
            if (dimEntity) entities.push(dimEntity);
        });
    }

    const basePoint = geometry.OuterContour.length > 0
        ? addPt(geometry.OuterContour[0])
        : new McGePoint3d(offsetX, offsetY, 0);

    return { entities, basePoint };
};

// ============================================================================
// 视图绘制函数（向后兼容）
// ============================================================================

/**
 * 绘制横断面视图
 */
window.drawSectionView = function (data, params, offsetX, offsetY) {
    const MX = window.MxCAD || {};
    const { McDbText, McGePoint3d, McCmColor, McDb, McDbMText } = MX;
    const entities = [];

    const section = window.buildUChannelCrossSectionEntities(data, offsetX, offsetY, params);
    entities.push(...section.entities);

    const sectionLabel = new McDbText();
    sectionLabel.textString = "引道横断面图";
    sectionLabel.height = 80;
    sectionLabel.position = new McGePoint3d(offsetX, offsetY - 300, 0);
    sectionLabel.horizontalMode = McDb.TextHorzMode.kTextCenter;
    sectionLabel.alignmentPoint = sectionLabel.position;
    sectionLabel.trueColor = new McCmColor(0, 0, 0);
    entities.push(sectionLabel);

    // 钢筋明细标注
    const rebarCallouts = [
        `N1 Φ14@${params.spacingOuter.toFixed(0)}`,
        `N2 Φ12@${params.spacingInner.toFixed(0)}`,
        `N3 Φ14@${params.spacingBottom.toFixed(0)}`,
        `N4 Φ14@${params.spacingBottom.toFixed(0)}`,
        `N5 Φ12@${params.b1.toFixed(0)}`,
        `N6 Φ12@${params.b2.toFixed(0)}`,
        `N7 φ8@250`,
        `N8 φ8@250`,
    ];

    if (McDbMText) {
        const calloutMText = new McDbMText();
        calloutMText.contents = rebarCallouts.join("\\P");
        calloutMText.textHeight = 45;
        calloutMText.location = new McGePoint3d(offsetX + 1500, offsetY + 500, 0);
        calloutMText.attachment = McDb.AttachmentPoint.kTopLeft;
        calloutMText.trueColor = new McCmColor(255, 0, 0);
        calloutMText.width = 1000;
        entities.push(calloutMText);
    }
    return entities;
};

/**
 * 绘制 A-A 平面布置图
 */
window.drawPlanAA = function (params, offsetX, offsetY) {
    // 使用新的 PlanView 组件（如果可用）
    if (window.PlanView && window.ParameterMapper) {
        const mapper = new window.ParameterMapper(params.id || 'YU1');
        const view = new window.PlanView(mapper, { x: offsetX, y: offsetY });
        return view.build().entities;
    }

    // 回退到原始实现
    const MX = window.MxCAD || {};
    const { McDbPolyline, McGePoint3d, McCmColor, McDbText, McDb, McDbLine } = MX;
    const entities = [];

    const w = params.length1 > 0 ? params.length1 : 2000;
    const h = params.outerWidth > 0 ? params.outerWidth : 1500;

    const outline = new McDbPolyline();
    [[offsetX, offsetY], [offsetX + w, offsetY], [offsetX + w, offsetY + h], [offsetX, offsetY + h]]
        .forEach(([x, y]) => outline.addVertexAt(new McGePoint3d(x, y, 0)));
    outline.isClosed = true;
    outline.trueColor = new McCmColor(0, 0, 0);
    entities.push(outline);

    const label = new McDbText();
    label.textString = "A-A 平面布置图";
    label.height = 80;
    label.position = new McGePoint3d(offsetX + w / 2, offsetY - 150, 0);
    label.horizontalMode = McDb.TextHorzMode.kTextCenter;
    label.alignmentPoint = label.position;
    label.trueColor = new McCmColor(0, 0, 0);
    entities.push(label);

    // 纵向钢筋
    const countV = params.m1 > 0 ? params.m1 : 10;
    const spacingV = w / (countV + 1);
    for (let i = 1; i <= countV; i++) {
        const x = offsetX + i * spacingV;
        const ln = new McDbLine(new McGePoint3d(x, offsetY + 50, 0), new McGePoint3d(x, offsetY + h - 50, 0));
        ln.trueColor = new McCmColor(255, 0, 0);
        entities.push(ln);
    }

    // 横向钢筋
    const countH = Math.floor(h / params.spacingBottom) || 8;
    const spacingH = h / (countH + 1);
    for (let i = 1; i <= countH; i++) {
        const y = offsetY + i * spacingH;
        const ln = new McDbLine(new McGePoint3d(offsetX + 50, y, 0), new McGePoint3d(offsetX + w - 50, y, 0));
        ln.trueColor = new McCmColor(255, 0, 0);
        entities.push(ln);
    }

    return entities;
};

/**
 * 绘制立面图
 */
window.drawElevationView = function (params, offsetX, offsetY, height, labelStr, spacingVal) {
    const MX = window.MxCAD || {};
    const { McDbPolyline, McGePoint3d, McCmColor, McDbText, McDb, McDbLine } = MX;
    const entities = [];

    const w = params.length1 > 0 ? params.length1 : 2000;
    const h = height > 0 ? height : 1000;

    const wall = new McDbPolyline();
    [[offsetX, offsetY], [offsetX + w, offsetY], [offsetX + w, offsetY + h], [offsetX, offsetY + h]]
        .forEach(([x, y]) => wall.addVertexAt(new McGePoint3d(x, y, 0)));
    wall.isClosed = true;
    wall.trueColor = new McCmColor(0, 0, 0);
    entities.push(wall);

    // 竖向钢筋
    const count = Math.floor(w / spacingVal) || 10;
    const actualSpacing = w / (count + 1);
    for (let i = 1; i <= count; i++) {
        const x = offsetX + i * actualSpacing;
        const ln = new McDbLine(new McGePoint3d(x, offsetY + 30, 0), new McGePoint3d(x, offsetY + h - 30, 0));
        ln.trueColor = new McCmColor(255, 0, 0);
        entities.push(ln);
    }

    // 水平分布筋
    const countH = 5;
    for (let i = 1; i < countH; i++) {
        const y = offsetY + i * (h / countH);
        const ln = new McDbLine(new McGePoint3d(offsetX + 30, y, 0), new McGePoint3d(offsetX + w - 30, y, 0));
        ln.trueColor = new McCmColor(255, 0, 0);
        entities.push(ln);
    }

    const lbl = new McDbText();
    lbl.textString = labelStr;
    lbl.height = 80;
    lbl.position = new McGePoint3d(offsetX + w / 2, offsetY - 150, 0);
    lbl.horizontalMode = McDb.TextHorzMode.kTextCenter;
    lbl.alignmentPoint = lbl.position;
    lbl.trueColor = new McCmColor(0, 0, 0);
    entities.push(lbl);

    return entities;
};

/**
 * 绘制施工说明
 */
window.drawConstructionNotes = function (x, y) {
    const MX = window.MxCAD || {};
    const { McDbMText, McGePoint3d, McCmColor, McDb } = MX;
    const entities = [];
    const notes = [
        "说明：",
        "1. 本图尺寸均以mm为单位，标高以m为单位。",
        "2. 混凝土强度等级为C35，抗渗等级为P8。",
        "3. 钢筋保护层厚度：底板为50mm，侧墙为40mm。",
        "4. 施工时应加强对基坑的监测，确保施工安全。",
        "5. 沉降缝处采用橡胶止水带，缝宽2cm。",
    ];
    if (McDbMText) {
        const mtext = new McDbMText();
        mtext.contents = notes.join("\\P");
        mtext.textHeight = 50;
        mtext.location = new McGePoint3d(x, y, 0);
        mtext.attachment = McDb.AttachmentPoint.kTopLeft;
        mtext.trueColor = new McCmColor(0, 0, 0);
        mtext.width = 2500;
        entities.push(mtext);
    }
    return entities;
};

// ============================================================================
// 表格绘制函数
// ============================================================================

window.buildUChannelTable1Entities = function (startX = 0, startY = -1000) {
    const MX = window.MxCAD || {};
    const { McDbPolyline, McGePoint3d, McCmColor, McDbText, McDb } = MX;

    const data = window.UChannelTable1Data;
    const headers = window.UChannelTable1Headers;
    const cellWidths = [120, 80, 60, 80, 80, 80, 60, 80, 80, 100, 100, 60, 80, 80, 80, 80, 60, 80, 100, 80, 80, 100, 80, 80, 80];
    const rowHeight = 40;
    const titleHeight = 80;
    const entities = [];

    const titleText = new McDbText();
    titleText.textString = "U型槽参数表(一)";
    titleText.height = 30;
    titleText.position = new McGePoint3d(startX + cellWidths.reduce((a, b) => a + b, 0) / 2, startY + titleHeight / 2, 0);
    titleText.horizontalMode = McDb.TextHorzMode.kTextCenter;
    titleText.alignmentPoint = titleText.position;
    titleText.trueColor = new McCmColor(0, 0, 0);
    entities.push(titleText);

    let currentY = startY;

    headers.forEach((h, i) => {
        let x = startX + cellWidths.slice(0, i).reduce((a, b) => a + b, 0);
        const rect = new McDbPolyline();
        rect.addVertexAt(new McGePoint3d(x, currentY, 0));
        rect.addVertexAt(new McGePoint3d(x + cellWidths[i], currentY, 0));
        rect.addVertexAt(new McGePoint3d(x + cellWidths[i], currentY - rowHeight, 0));
        rect.addVertexAt(new McGePoint3d(x, currentY - rowHeight, 0));
        rect.isClosed = true;
        rect.trueColor = new McCmColor(0, 0, 0);
        entities.push(rect);

        const text = new McDbText();
        text.textString = h;
        text.height = 12;
        text.position = new McGePoint3d(x + cellWidths[i] / 2, currentY - rowHeight / 2, 0);
        text.horizontalMode = McDb.TextHorzMode.kTextCenter;
        text.verticalMode = McDb.TextVertMode.kTextVertMid;
        text.alignmentPoint = text.position;
        text.trueColor = new McCmColor(0, 0, 0);
        entities.push(text);
    });

    currentY -= rowHeight;

    data.forEach((row) => {
        row.forEach((val, colIndex) => {
            let x = startX + cellWidths.slice(0, colIndex).reduce((a, b) => a + b, 0);
            const rect = new McDbPolyline();
            rect.addVertexAt(new McGePoint3d(x, currentY, 0));
            rect.addVertexAt(new McGePoint3d(x + cellWidths[colIndex], currentY, 0));
            rect.addVertexAt(new McGePoint3d(x + cellWidths[colIndex], currentY - rowHeight, 0));
            rect.addVertexAt(new McGePoint3d(x, currentY - rowHeight, 0));
            rect.isClosed = true;
            rect.trueColor = new McCmColor(0, 0, 0);
            entities.push(rect);

            const text = new McDbText();
            text.textString = val.toString();
            text.height = 12;
            text.position = new McGePoint3d(x + cellWidths[colIndex] / 2, currentY - rowHeight / 2, 0);
            text.horizontalMode = McDb.TextHorzMode.kTextCenter;
            text.verticalMode = McDb.TextVertMode.kTextVertMid;
            text.alignmentPoint = text.position;
            text.trueColor = new McCmColor(0, 0, 255);
            entities.push(text);
        });
        currentY -= rowHeight;
    });

    return { entities, basePoint: new McGePoint3d(startX, startY, 0) };
};

window.buildUChannelQuantityTableEntities = function (yuType, dataSet, headers, startX, startY) {
    const MX = window.MxCAD || {};
    const { McDbPolyline, McGePoint3d, McCmColor, McDbText, McDb } = MX;
    const entities = [];

    const resolvedSet = dataSet || window.UChannelQuantityTable2Data;
    const resolvedHeaders = headers || window.UChannelQuantityTable2Headers;
    const keys = resolvedSet ? Object.keys(resolvedSet) : [];
    const resolvedKey = (resolvedSet && resolvedSet[yuType]) ? yuType : (keys[0] || yuType);
    const tableData = resolvedSet?.[resolvedKey];
    
    if (!tableData) {
        return { entities: [], basePoint: new McGePoint3d(startX, startY, 0), key: resolvedKey };
    }

    const colWidths = [60, 60, 200, 100, 80, 100, 100, 100];
    const rowHeight = 40;
    const titleHeight = 80;

    const titleText = new McDbText();
    titleText.textString = `${resolvedKey} 钢筋数量表`;
    titleText.height = 30;
    titleText.position = new McGePoint3d(startX + colWidths.reduce((a, b) => a + b, 0) / 2, startY + titleHeight / 2, 0);
    titleText.horizontalMode = McDb.TextHorzMode.kTextCenter;
    titleText.alignmentPoint = titleText.position;
    titleText.trueColor = new McCmColor(0, 0, 0);
    entities.push(titleText);

    let currentY = startY;

    resolvedHeaders.forEach((h, i) => {
        let x = startX + colWidths.slice(0, i).reduce((a, b) => a + b, 0);
        const rect = new McDbPolyline();
        rect.addVertexAt(new McGePoint3d(x, currentY, 0));
        rect.addVertexAt(new McGePoint3d(x + colWidths[i], currentY, 0));
        rect.addVertexAt(new McGePoint3d(x + colWidths[i], currentY - rowHeight, 0));
        rect.addVertexAt(new McGePoint3d(x, currentY - rowHeight, 0));
        rect.trueColor = new McCmColor(0, 0, 0);
        entities.push(rect);

        const text = new McDbText();
        text.textString = h;
        text.height = 12;
        text.position = new McGePoint3d(x + colWidths[i] / 2, currentY - rowHeight / 2, 0);
        text.horizontalMode = McDb.TextHorzMode.kTextCenter;
        text.verticalMode = McDb.TextVertMode.kTextVertMid;
        text.alignmentPoint = text.position;
        text.trueColor = new McCmColor(0, 0, 0);
        entities.push(text);
    });

    currentY -= rowHeight;

    tableData.Items.forEach(row => {
        row.forEach((val, colIndex) => {
            let x = startX + colWidths.slice(0, colIndex).reduce((a, b) => a + b, 0);
            const rect = new McDbPolyline();
            rect.addVertexAt(new McGePoint3d(x, currentY, 0));
            rect.addVertexAt(new McGePoint3d(x + colWidths[colIndex], currentY, 0));
            rect.addVertexAt(new McGePoint3d(x + colWidths[colIndex], currentY - rowHeight, 0));
            rect.addVertexAt(new McGePoint3d(x, currentY - rowHeight, 0));
            rect.trueColor = new McCmColor(0, 0, 0);
            entities.push(rect);

            const text = new McDbText();
            text.textString = val.toString();
            text.height = 12;
            text.position = new McGePoint3d(x + colWidths[colIndex] / 2, currentY - rowHeight / 2, 0);
            text.horizontalMode = McDb.TextHorzMode.kTextCenter;
            text.verticalMode = McDb.TextVertMode.kTextVertMid;
            text.alignmentPoint = text.position;
            text.trueColor = new McCmColor(0, 0, 255);
            entities.push(text);
        });
        currentY -= rowHeight;
    });

    const sum = tableData.Summary;
    if (sum) {
        const summaryText = new McDbText();
        summaryText.textString = `C35混凝土: ${sum.Concrete}m, HPB300: ${sum.HPB300}kg, HRB400: ${sum.HRB400}kg`;
        summaryText.height = 15;
        summaryText.position = new McGePoint3d(startX + colWidths.reduce((a, b) => a + b, 0) / 2, currentY - 20, 0);
        summaryText.horizontalMode = McDb.TextHorzMode.kTextCenter;
        summaryText.alignmentPoint = summaryText.position;
        summaryText.trueColor = new McCmColor(0, 0, 0);
        entities.push(summaryText);
    }

    return { entities, basePoint: new McGePoint3d(startX, startY, 0), key: resolvedKey };
};

// ============================================================================
// 主绘制函数
// ============================================================================

window.drawUChannel = function (data) {
    const MX = window.MxCAD || {};
    const mxcad = MX.MxCpp.getCurrentMxCAD();
    if (!mxcad) {
        console.error("MXCAD not ready.");
        return;
    }

    console.log("Drawing U-Channel with data:", data);
    const { entities, basePoint } = window.buildUChannelCrossSectionEntities(data, 0, 0);
    const dimEntities = entities.filter((e) => e && e._isDimension);
    const blockEntities = entities.filter((e) => !e || !e._isDimension);
    window.createAndDrawBlock("UChannel", blockEntities, basePoint, basePoint);
    dimEntities.forEach((dim) => mxcad.drawEntity(dim));
};

window.drawUChannelTable1 = function () {
    const MX = window.MxCAD || {};
    const mxcad = MX.MxCpp.getCurrentMxCAD();
    if (!mxcad) {
        console.error("MXCAD not ready.");
        return;
    }
    const { entities, basePoint } = window.buildUChannelTable1Entities(0, -1000);
    window.createAndDrawBlock("Table1", entities, basePoint, basePoint);
};

window.drawUChannelQuantityTable2 = function (yuType = "YU1", dataSet = null, headers = null, startX = 2500) {
    const MX = window.MxCAD || {};
    const mxcad = MX.MxCpp.getCurrentMxCAD();
    if (!mxcad) return;

    const startY = -1000;
    const result = window.buildUChannelQuantityTableEntities(yuType, dataSet, headers, startX, startY);
    const basePoint = new (MX.McGePoint3d)(startX, startY, 0);
    window.createAndDrawBlock(`QuantityTable2_${yuType}`, result.entities, basePoint, basePoint);
};

window.drawUChannelQuantityTable3 = function (yuType = "YU5") {
    window.drawUChannelQuantityTable2(yuType, window.UChannelQuantityTable3Data, window.UChannelQuantityTable3Headers, 3500);
};

// ============================================================================
// 布局图绘制（使用新模块化架构）
// ============================================================================

window.drawUChannelLayoutSheet = function (data, options = {}) {
    const MX = window.MxCAD || {};
    const mxcad = MX.MxCpp.getCurrentMxCAD();
    if (!mxcad) {
        console.error("MXCAD not ready.");
        return;
    }

    const { McDbText, McGePoint3d, McCmColor, McDb, McDbPolyline, McDbMText, McDbLine } = MX;
    const yuType = options.yuType || data?.PresetType || data?.Parameters?.PresetType || "YU1";

    // 使用 ParameterMapper（如果可用）
    let params;
    if (window.ParameterMapper) {
        const mapper = new window.ParameterMapper(yuType);
        params = mapper.getParams();
    } else {
        params = window.getNormalizedParams(yuType) || {};
    }

    const entities = [];

    // 使用 LayoutEngine（如果可用）
    const layout = window.LayoutEngine ? window.LayoutEngine.createUChannelLayout() : null;
    
    // 布局常量
    const SHEET_WIDTH = 10000;
    const SECTION_ORIGIN = layout?.getViewOrigin('crossSection') || { x: 0, y: 800 };
    const AA_ORIGIN = layout?.getViewOrigin('planAA') || { x: 3000, y: 1500 };
    const BB_ORIGIN = { x: 3000, y: 200 };
    const CC_ORIGIN = { x: 5500, y: 200 };
    const TABLE_Y = -1000;
    const NOTES_ORIGIN = layout?.getViewOrigin('notes') || { x: 7500, y: 2000 };

    // 1. 图纸大标题
    const title = new McDbText();
    title.textString = `U型槽钢筋布置图 (${yuType})`;
    title.height = 150;
    title.position = new McGePoint3d(SHEET_WIDTH / 2 - 1000, 2800, 0);
    title.horizontalMode = McDb.TextHorzMode.kTextCenter;
    title.alignmentPoint = title.position;
    title.trueColor = new McCmColor(0, 0, 0);
    entities.push(title);

    // 2. 引道横断面图
    const section = window.buildUChannelCrossSectionEntities(data, SECTION_ORIGIN.x, SECTION_ORIGIN.y);
    entities.push(...section.entities);

    const sectionLabel = new McDbText();
    sectionLabel.textString = "引道横断面图";
    sectionLabel.height = 80;
    sectionLabel.position = new McGePoint3d(SECTION_ORIGIN.x, SECTION_ORIGIN.y - 300, 0);
    sectionLabel.horizontalMode = McDb.TextHorzMode.kTextCenter;
    sectionLabel.alignmentPoint = sectionLabel.position;
    sectionLabel.trueColor = new McCmColor(0, 0, 0);
    entities.push(sectionLabel);

    // 钢筋明细标注
    const rebarCallouts = window.ParameterMapper 
        ? new window.ParameterMapper(yuType).getAllRebarCallouts()
        : ["N1 Φ12@200", "N2 Φ12@200", "N3 Φ12@200", "N4 Φ14@200", "N5 Φ12@200", "N6 Φ12@200", "N7 φ8@250", "N8 φ8@250"];
    
    if (McDbMText) {
        const calloutMText = new McDbMText();
        calloutMText.contents = rebarCallouts.join("\\P");
        calloutMText.textHeight = 45;
        calloutMText.location = new McGePoint3d(SECTION_ORIGIN.x + 1500, SECTION_ORIGIN.y + 500, 0);
        calloutMText.attachment = McDb.AttachmentPoint.kTopLeft;
        calloutMText.trueColor = new McCmColor(255, 0, 0);
        calloutMText.width = 1000;
        entities.push(calloutMText);
    }

    // 3. A-A 平面布置图
    const aaW = params.length1 || 2000;
    const aaH = params.totalWidth || 1500;
    const aaOutline = new McDbPolyline();
    [[AA_ORIGIN.x, AA_ORIGIN.y], [AA_ORIGIN.x + aaW, AA_ORIGIN.y], 
     [AA_ORIGIN.x + aaW, AA_ORIGIN.y + aaH], [AA_ORIGIN.x, AA_ORIGIN.y + aaH]]
        .forEach(([x, y]) => aaOutline.addVertexAt(new McGePoint3d(x, y, 0)));
    aaOutline.isClosed = true;
    aaOutline.trueColor = new McCmColor(0, 0, 0);
    entities.push(aaOutline);

    const aaLabel = new McDbText();
    aaLabel.textString = "A-A 平面布置图";
    aaLabel.height = 80;
    aaLabel.position = new McGePoint3d(AA_ORIGIN.x + aaW / 2, AA_ORIGIN.y - 150, 0);
    aaLabel.horizontalMode = McDb.TextHorzMode.kTextCenter;
    aaLabel.alignmentPoint = aaLabel.position;
    aaLabel.trueColor = new McCmColor(0, 0, 0);
    entities.push(aaLabel);

    // 绘制钢筋网格
    // 纵向钢筋 (N5/N6 分布筋)
    const aaCountV = 12;
    const aaSpacingX = aaW / (aaCountV + 1);
    for (let i = 1; i <= aaCountV; i++) {
        const x = AA_ORIGIN.x + i * aaSpacingX;
        const ln = new McDbLine(new McGePoint3d(x, AA_ORIGIN.y + 50, 0), new McGePoint3d(x, AA_ORIGIN.y + aaH - 50, 0));
        ln.trueColor = new McCmColor(255, 0, 0);
        entities.push(ln);
    }
    
    // 横向钢筋 (N3/N4 主筋)
    const aaCountH = 10;
    const aaSpacingY = aaH / (aaCountH + 1);
    for (let i = 1; i <= aaCountH; i++) {
        const y = AA_ORIGIN.y + i * aaSpacingY;
        const ln = new McDbLine(new McGePoint3d(AA_ORIGIN.x + 50, y, 0), new McGePoint3d(AA_ORIGIN.x + aaW - 50, y, 0));
        ln.trueColor = new McCmColor(255, 0, 0);
        entities.push(ln);
    }

    // 4. B-B / C-C 侧墙立面图
    const buildElevation = (origin, height, label) => {
        const wallW = 2000;
        const wallH = height > 0 ? height : 1000;
        const wall = new McDbPolyline();
        [[origin.x, origin.y], [origin.x + wallW, origin.y],
         [origin.x + wallW, origin.y + wallH], [origin.x, origin.y + wallH]]
            .forEach(([x, y]) => wall.addVertexAt(new McGePoint3d(x, y, 0)));
        wall.isClosed = true;
        wall.trueColor = new McCmColor(0, 0, 0);
        entities.push(wall);

        // 竖向钢筋 (N1/N2)
        const wallCountV = 10;
        for (let i = 1; i <= wallCountV; i++) {
            const x = origin.x + i * (wallW / (wallCountV + 1));
            const ln = new McDbLine(new McGePoint3d(x, origin.y + 30, 0), new McGePoint3d(x, origin.y + wallH - 30, 0));
            ln.trueColor = new McCmColor(255, 0, 0);
            entities.push(ln);
        }
        
        // 水平分布筋 (N5/N6)
        const wallCountH = 6;
        for (let i = 1; i <= wallCountH; i++) {
            const y = origin.y + i * (wallH / (wallCountH + 1));
            const ln = new McDbLine(new McGePoint3d(origin.x + 30, y, 0), new McGePoint3d(origin.x + wallW - 30, y, 0));
            ln.trueColor = new McCmColor(255, 0, 0);
            entities.push(ln);
        }

        const lbl = new McDbText();
        lbl.textString = label;
        lbl.height = 80;
        lbl.position = new McGePoint3d(origin.x + wallW / 2, origin.y - 150, 0);
        lbl.horizontalMode = McDb.TextHorzMode.kTextCenter;
        lbl.alignmentPoint = lbl.position;
        lbl.trueColor = new McCmColor(0, 0, 0);
        entities.push(lbl);
    };

    buildElevation(BB_ORIGIN, params.heightMin || 1000, "B-B 侧墙立面图");
    buildElevation(CC_ORIGIN, params.heightMax || 1200, "C-C 侧墙立面图");

    // 5. 施工说明
    const notes = ["说明：", "1. 本图尺寸均以mm为单位，标高以m为单位。", 
                   "2. 混凝土强度等级为C35，抗渗等级为P8。",
                   "3. 钢筋保护层厚度：底板为50mm，侧墙为40mm。",
                   "4. 施工时应加强对基坑的监测，确保施工安全。",
                   "5. 沉降缝处采用橡胶止水带，缝宽2cm。"];
    if (McDbMText) {
        const mtext = new McDbMText();
        mtext.contents = notes.join("\\P");
        mtext.textHeight = 50;
        mtext.location = new McGePoint3d(NOTES_ORIGIN.x, NOTES_ORIGIN.y, 0);
        mtext.attachment = McDb.AttachmentPoint.kTopLeft;
        mtext.trueColor = new McCmColor(0, 0, 0);
        mtext.width = 2500;
        entities.push(mtext);
    }

    // 6. 表格区域
    const tableXStart = -2500;
    const table1 = window.buildUChannelTable1Entities(tableXStart, TABLE_Y);
    entities.push(...table1.entities);

    const q2 = window.buildUChannelQuantityTableEntities(yuType, window.UChannelQuantityTable2Data, window.UChannelQuantityTable2Headers, tableXStart + 3500, TABLE_Y);
    entities.push(...q2.entities);

    const q3 = window.buildUChannelQuantityTableEntities(yuType, window.UChannelQuantityTable3Data, window.UChannelQuantityTable3Headers, tableXStart + 7000, TABLE_Y);
    entities.push(...q3.entities);

    // 7. 图框
    const border = new McDbPolyline();
    [[-3000, -2500], [SHEET_WIDTH, -2500], [SHEET_WIDTH, 4000], [-3000, 4000]]
        .forEach(([x, y]) => border.addVertexAt(new McGePoint3d(x, y, 0)));
    border.isClosed = true;
    border.trueColor = new McCmColor(100, 100, 100);
    entities.push(border);

    // 8. 最终成块绘制
    const basePoint = new McGePoint3d(0, 0, 0);
    const dimEntities = entities.filter((e) => e && e._isDimension);
    const blockEntities = entities.filter((e) => !e || !e._isDimension);
    window.createAndDrawBlock(`UChannelLayoutSheet_${yuType}`, blockEntities, basePoint, basePoint);
    dimEntities.forEach((dim) => mxcad.drawEntity(dim));
};

// ============================================================================
// 几何数据构建（用于测试/演示）
// ============================================================================

window.createUChannelGeometryFromParams = function (params) {
    const p = params || {};
    // 所有尺寸统一使用 mm
    const internalWidth = Number(p.internalWidth ?? p.InternalWidth ?? 2225);
    const internalHeight = Number(p.internalHeight ?? p.InternalHeight ?? 2435);  // mm (原 243.5cm)
    const wallThickness = Number(p.wallThickness ?? p.WallThickness ?? 300);      // mm (原 30cm)
    const bottomThickness = Number(p.bottomThickness ?? p.BottomThickness ?? 400); // mm (原 40cm)
    const haunchWidth = Number(p.haunchWidth ?? p.HaunchWidth ?? 100);   // mm (腋角宽度)
    const haunchHeight = Number(p.haunchHeight ?? p.HaunchHeight ?? 100); // mm (腋角高度)

    const halfW = internalWidth / 2.0;
    const outerHalfW = halfW + wallThickness;

    const geometry = {
        OuterContour: [
            { X: -outerHalfW, Y: internalHeight },
            { X: outerHalfW, Y: internalHeight },
            { X: outerHalfW, Y: -bottomThickness },
            { X: -outerHalfW, Y: -bottomThickness },
        ],
        InnerContour: [
            { X: -halfW, Y: internalHeight },
            { X: -halfW, Y: haunchHeight },
            { X: -halfW + haunchWidth, Y: 0 },
            { X: halfW - haunchWidth, Y: 0 },
            { X: halfW, Y: haunchHeight },
            { X: halfW, Y: internalHeight },
        ],
        Rebars: [],
        Dimensions: [],
    };

    // 钢筋保护层厚度 (mm) - 规范要求底板50mm，侧墙40mm
    const cover = 40.0;
    
    // N1: 外层U形主筋 (Φ14)
    geometry.Rebars.push({
        ID: 'N1', Diameter: 14,
        Points: [
            { X: -outerHalfW + cover, Y: internalHeight - cover },
            { X: -outerHalfW + cover, Y: -bottomThickness + cover },
            { X: outerHalfW - cover, Y: -bottomThickness + cover },
            { X: outerHalfW - cover, Y: internalHeight - cover },
        ]
    });
    
    // N2: 内层U形主筋 (Φ12)
    geometry.Rebars.push({
        ID: 'N2', Diameter: 12,
        Points: [
            { X: -halfW - cover, Y: internalHeight - cover },
            { X: -halfW - cover, Y: cover },
            { X: halfW + cover, Y: cover },
            { X: halfW + cover, Y: internalHeight - cover },
        ]
    });

    // N3: 底板下层点筋 (Φ20)
    const baseY = -bottomThickness + cover + 20;
    const basePoints = [];
    for (let i = 0; i < 5; i++) {
        const t = i / 4;
        basePoints.push({ X: (-halfW + cover + 60) + (internalWidth - 2 * cover - 120) * t, Y: baseY });
    }
    geometry.Rebars.push({ ID: 'N3', Diameter: 20, Type: 'point', Points: basePoints });

    // N4: 底板角筋 (Φ14)
    const cornerOffset = 80;
    geometry.Rebars.push({
        ID: 'N4', Diameter: 14,
        Points: [
            { X: -outerHalfW + cover, Y: -bottomThickness + cover },
            { X: -outerHalfW + cover, Y: -bottomThickness + cover + 200 },
            { X: -outerHalfW + cover + cornerOffset, Y: -bottomThickness + cover + 200 },
        ]
    });
    geometry.Rebars.push({
        ID: 'N4', Diameter: 14, ShowLabel: false,
        Points: [
            { X: outerHalfW - cover, Y: -bottomThickness + cover },
            { X: outerHalfW - cover, Y: -bottomThickness + cover + 200 },
            { X: outerHalfW - cover - cornerOffset, Y: -bottomThickness + cover + 200 },
        ]
    });

    // N5: 侧墙分布筋 (Φ12)
    const wallPoints = [];
    for (let i = 0; i < 6; i++) {
        const t = (i + 1) / 7;
        wallPoints.push({ X: -halfW + cover + 20, Y: t * (internalHeight - cover - 100) + 50 });
        wallPoints.push({ X: halfW - cover - 20, Y: t * (internalHeight - cover - 100) + 50 });
    }
    geometry.Rebars.push({ ID: 'N5', Diameter: 12, Type: 'point', Points: wallPoints, ShowLabel: false });

    // N6: 底板上层点筋 (Φ12)
    const baseTopPoints = [];
    for (let i = 0; i < 6; i++) {
        const t = i / 5;
        baseTopPoints.push({ X: (-halfW + cover + 120) + (internalWidth - 2 * cover - 240) * t, Y: -bottomThickness + cover + 120 });
    }
    geometry.Rebars.push({ ID: 'N6', Diameter: 12, Type: 'point', Points: baseTopPoints, ShowLabel: false });

    // N7: 底板拉结筋 (φ8)
    const meshPoints = [];
    for (let i = 0; i < 4; i++) {
        const t = (i + 1) / 5;
        meshPoints.push({ X: (-halfW + cover + 100) + (internalWidth - 2 * cover - 200) * t, Y: cover + 60 });
    }
    geometry.Rebars.push({ ID: 'N7', Diameter: 8, Type: 'point', Points: meshPoints, ShowLabel: false });

    // N8: 侧墙拉结筋 (φ8)
    const wallMeshPoints = [];
    for (let i = 0; i < 5; i++) {
        const t = (i + 1) / 6;
        wallMeshPoints.push({ X: -halfW + cover + 40, Y: t * (internalHeight - cover - 200) + 100 });
        wallMeshPoints.push({ X: halfW - cover - 40, Y: t * (internalHeight - cover - 200) + 100 });
    }
    geometry.Rebars.push({ ID: 'N8', Diameter: 8, Type: 'point', Points: wallMeshPoints, ShowLabel: false });

    // 标注 (偏移量使用 mm)
    geometry.Dimensions.push({
        Start: { X: -outerHalfW, Y: -bottomThickness - 200 },
        End: { X: outerHalfW, Y: -bottomThickness - 200 },
        Text: `D = ${(internalWidth + 2 * wallThickness).toFixed(0)}`,
        TextPosition: { X: 0, Y: -bottomThickness - 300 },
    });
    geometry.Dimensions.push({
        Start: { X: -halfW, Y: internalHeight + 100 },
        End: { X: halfW, Y: internalHeight + 100 },
        Text: `${internalWidth.toFixed(0)}`,
        TextPosition: { X: 0, Y: internalHeight + 200 },
    });
    geometry.Dimensions.push({
        Start: { X: outerHalfW + 100, Y: 0 },
        End: { X: outerHalfW + 100, Y: internalHeight },
        Text: `H = ${internalHeight.toFixed(0)}`,
        TextPosition: { X: outerHalfW + 200, Y: internalHeight / 2 },
        Rotation: Math.PI / 2,
    });

    return geometry;
};

window.createUChannelSampleData = function (yuType) {
    const type = yuType || 'YU1';
    
    // 优先使用 ParameterMapper 获取正确的 mm 单位参数
    if (window.ParameterMapper) {
        const mapper = new window.ParameterMapper(type);
        const p = mapper.getParams();
        const geo = mapper.getCrossSectionGeometry();
        
        const params = {
            InternalWidth: p.internalWidth,      // mm
            InternalHeight: geo.height,          // mm (取 Hmin/Hmax 平均值)
            WallThickness: geo.wallThickness,    // mm
            BottomThickness: geo.bottomThickness // mm
        };
        
        return {
            Type: 'U-Channel Layout Sheet',
            PresetType: type,
            Parameters: params,
            Geometry: window.createUChannelGeometryFromParams(params),
        };
    }
    
    // 回退：手动转换单位 (cm -> mm)
    // 注意：原始数据中 Hmin/Hmax/B/l 是 cm，需要 *10
    const params = {
        InternalWidth: 2225.0,          // mm (D - 2*B)
        InternalHeight: 243.5 * 10,     // cm -> mm = 2435
        WallThickness: 30.0 * 10,       // cm -> mm = 300
        BottomThickness: 40.0 * 10,     // cm -> mm = 400
    };
    return {
        Type: 'U-Channel Layout Sheet',
        PresetType: type,
        Parameters: params,
        Geometry: window.createUChannelGeometryFromParams(params),
    };
};

// ============================================================================
// 新 API - 使用模块化组件的简化接口
// ============================================================================

/**
 * 使用新模块化架构创建布局图（推荐）
 * @param {string} yuType - U型槽编号
 * @param {Object} options - 配置选项
 */
window.createUChannelLayoutModular = function (yuType = 'YU1', options = {}) {
    // 检查模块是否已加载
    if (!window.ParameterMapper || !window.LayoutEngine || !window.EntityBuilders) {
        console.warn('模块化组件未完全加载，回退到传统方式');
        const data = window.createUChannelSampleData(yuType);
        return window.drawUChannelLayoutSheet(data, { yuType, ...options });
    }

    const mapper = new window.ParameterMapper(yuType);
    const layout = window.LayoutEngine.createUChannelLayout();
    
    console.log(`使用模块化架构创建 ${yuType} 布局图`);
    console.log('参数:', mapper.getParams());
    console.log('布局:', layout.toJSON());

    const data = window.createUChannelSampleData(yuType);
    return window.drawUChannelLayoutSheet(data, { yuType, ...options });
};

console.log('UChannelPainter 模块加载完成（重构版）');
