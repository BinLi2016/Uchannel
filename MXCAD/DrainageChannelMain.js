/**
 * Main Drainage Channel Drawing Function
 * Orchestrates all components to create the complete drawing
 */

window.drawDrainageChannelReinforcement = function (data, __retryAttempt) {
    const MX = window.MxCAD || {};
    const attempt = typeof __retryAttempt === 'number' ? __retryAttempt : 0;
    let mxcad = null;
    try {
        mxcad = MX.MxCpp && typeof MX.MxCpp.getCurrentMxCAD === 'function' ? MX.MxCpp.getCurrentMxCAD() : null;
    } catch (e) {
        mxcad = null;
    }
    if (!mxcad) {
        if (attempt < 60) {
            setTimeout(() => window.drawDrainageChannelReinforcement(data, attempt + 1), 250);
        } else {
            console.error("MXCAD not ready (timeout).", { attempt });
        }
        return;
    }

    console.log("Drawing Drainage Channel Reinforcement...");

    const painter = window.DrainageChannelPainter;

    // If C# provided structured data, use the motor-lane pipeline.
    const hasMotorLanePayload = !!(data && data.section && data.cover && data.channelTable && data.coverTable);

    const payload = hasMotorLanePayload
        ? data
        : {
            // Backwards-compatible demo defaults.
            section: null,
            cover: null,
            channelTable: null,
            coverTable: null,
            projectInfo: window.DrainageChannelProjectInfo,
            notes: window.DrainageChannelNotes,
            _legacy: {
                params: window.DrainageChannelParams,
                rebarSpecs: window.DrainageChannelRebar,
                dimTable: window.DrainageChannelDimTable,
                rebarSchedule: window.DrainageChannelRebarSchedule,
                geomCalc: window.DrainageChannelGeometry,
            }
        };

    // Define layout positions
    const layout = {
        channelSection: { x: 0, y: 900 },
        coverDetail: { x: 1200, y: 900 },
        coverRebar: { x: 2400, y: 900 },
        // Keep table/title/notes in positive Y so zoomAll reliably includes them.
        // (Some MXCAD builds appear to under-estimate extents with large negative coordinates.)
        channelTable: { x: 0, y: 400 },
        coverTable: { x: 1300, y: 400 },
        bending: { x: 2400, y: 420 },
        // Keep title block close to tables so it stays in view after zoom.
        titleBlock: { x: 2250, y: 420 },
        notes: { x: 0, y: 180 }
    };

    // Collect all entities
    const allEntities = [];

    if (hasMotorLanePayload) {
        console.log("Drawing motor-lane drainage channel views...");

        allEntities.push(...painter.drawMotorLaneChannelSection(payload.section, layout.channelSection, 1.0));
        allEntities.push(...painter.drawMotorLaneCoverDetail(payload.cover, layout.coverDetail, 1.0));
        allEntities.push(...painter.drawMotorLaneCoverRebarPlan(payload.cover, payload.coverTable, layout.coverRebar, 1.0));

        allEntities.push(...painter.drawRebarQuantityTable(payload.channelTable, layout.channelTable));
        allEntities.push(...painter.drawRebarQuantityTable(payload.coverTable, layout.coverTable));

        allEntities.push(...painter.drawBendingDiagrams(
            [payload.channelTable.items?.[0], payload.channelTable.items?.[1]].filter(Boolean),
            layout.bending,
            1.0
        ));
    } else {
        // Legacy demo pipeline.
        const { params, rebarSpecs, dimTable, rebarSchedule, geomCalc } = payload._legacy;

        const crossSection = geomCalc.calculateCrossSectionPoints(params);
        const crossSectionRebar = geomCalc.calculateCrossSectionRebar(params, rebarSpecs);
        const crossSectionDims = geomCalc.calculateCrossSectionDimensions(params);
        const planView = geomCalc.calculatePlanViewPoints(params);
        const planViewRebar = geomCalc.calculatePlanViewRebar(params, rebarSpecs);

        allEntities.push(...painter.drawCrossSectionView(
            crossSection,
            crossSectionRebar,
            crossSectionDims,
            { x: 0, y: 600 },
            1.0
        ));

        allEntities.push(...painter.drawPlanView(
            planView,
            planViewRebar,
            { x: 2000, y: 800 },
            0.8
        ));

        allEntities.push(...painter.drawDimensionTable(dimTable, { x: 2000, y: 200 }));
        allEntities.push(...painter.drawRebarScheduleTable(rebarSchedule, { x: 0, y: -200 }));
    }

    // Draw title block
    console.log("Drawing title block...");
    const titleBlockEntities = painter.drawTitleBlock(
        payload.projectInfo,
        layout.titleBlock
    );
    allEntities.push(...titleBlockEntities);

    // Draw construction notes
    console.log("Drawing construction notes...");
    const notesEntities = painter.drawNotes(payload.notes, layout.notes);
    allEntities.push(...notesEntities);

    // Create unified block
    const dimEntities = allEntities.filter((e) => e && e._isDimension);
    const blockEntities = allEntities.filter((e) => !e || !e._isDimension);
    console.log(`Creating block with ${blockEntities.length} entities...`);
    const basePoint = new MX.McGePoint3d(0, 0, 0);
    window.createAndDrawBlock("DrainageChannelDrawing", blockEntities, basePoint, basePoint);
    dimEntities.forEach((dim) => mxcad.drawEntity(dim));

    // Zoom to fit
    console.log("Zooming to fit...");
    setTimeout(() => {
        mxcad.zoomAll();
        mxcad.updateDisplay();
    }, 100);

    console.log("Drainage Channel Reinforcement drawing complete!");
};

window.drawDrainageMotorLaneFromSample = async function () {
    try {
        const resp = await fetch('DrainageMotorLaneSample.json');
        const payload = await resp.json();
        window.drawDrainageChannelReinforcement(payload);
    } catch (e) {
        console.error('Failed to load DrainageMotorLaneSample.json', e);
    }
};
