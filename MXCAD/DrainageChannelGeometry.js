/**
 * Drainage Channel Geometry Calculator
 * Calculates all geometric points for different views
 */

window.DrainageChannelGeometry = {

    /**
     * Calculate cross-section contour points
     * Returns outer and inner contours for the channel cross-section
     */
    calculateCrossSectionPoints: function (params) {
        const { internalWidth, internalHeight, wallThickness, baseThickness } = params;

        // Calculate total dimensions
        const totalWidth = internalWidth + 2 * wallThickness;
        const totalHeight = internalHeight + baseThickness;

        // Outer contour (clockwise from bottom-left)
        const outerContour = [
            { x: 0, y: 0 },                           // Bottom-left
            { x: totalWidth, y: 0 },                  // Bottom-right
            { x: totalWidth, y: totalHeight },        // Top-right
            { x: 0, y: totalHeight }                  // Top-left
        ];

        // Inner contour (clockwise from bottom-left of cavity)
        const innerContour = [
            { x: wallThickness, y: baseThickness },                    // Bottom-left
            { x: wallThickness + internalWidth, y: baseThickness },    // Bottom-right
            { x: wallThickness + internalWidth, y: totalHeight },      // Top-right (open top)
            { x: wallThickness, y: totalHeight }                       // Top-left (open top)
        ];

        return { outerContour, innerContour, totalWidth, totalHeight };
    },

    /**
     * Calculate rebar positions for cross-section view
     */
    calculateCrossSectionRebar: function (params, rebarSpecs) {
        const { internalWidth, internalHeight, wallThickness, baseThickness, concreteCover } = params;
        const rebars = [];

        // Left wall vertical rebars
        const leftWallX = concreteCover;
        const leftWallRebars = this.distributeRebarsAlongLine(
            { x: leftWallX, y: baseThickness + concreteCover },
            { x: leftWallX, y: baseThickness + internalHeight - concreteCover },
            rebarSpecs.wallMainRebar.spacing,
            rebarSpecs.wallMainRebar.mark,
            rebarSpecs.wallMainRebar.diameter
        );
        rebars.push(...leftWallRebars);

        // Right wall vertical rebars
        const rightWallX = wallThickness + internalWidth + wallThickness - concreteCover;
        const rightWallRebars = this.distributeRebarsAlongLine(
            { x: rightWallX, y: baseThickness + concreteCover },
            { x: rightWallX, y: baseThickness + internalHeight - concreteCover },
            rebarSpecs.wallMainRebar.spacing,
            rebarSpecs.wallMainRebar.mark,
            rebarSpecs.wallMainRebar.diameter
        );
        rebars.push(...rightWallRebars);

        // Base slab bottom rebars (longitudinal direction - shown as circles in cross-section)
        const baseY = concreteCover;
        const baseRebars = this.distributeRebarsAlongLine(
            { x: wallThickness + concreteCover, y: baseY },
            { x: wallThickness + internalWidth - concreteCover, y: baseY },
            rebarSpecs.baseMainRebar.spacing,
            rebarSpecs.baseMainRebar.mark,
            rebarSpecs.baseMainRebar.diameter
        );
        rebars.push(...baseRebars);

        // Base slab top rebars
        const baseTopY = baseThickness - concreteCover;
        const baseTopRebars = this.distributeRebarsAlongLine(
            { x: wallThickness + concreteCover, y: baseTopY },
            { x: wallThickness + internalWidth - concreteCover, y: baseTopY },
            rebarSpecs.baseMainRebar.spacing,
            rebarSpecs.baseMainRebar.mark,
            rebarSpecs.baseMainRebar.diameter
        );
        rebars.push(...baseTopRebars);

        return rebars;
    },

    /**
     * Calculate plan view points (cover plate)
     */
    calculatePlanViewPoints: function (params) {
        const { coverPlateWidth, coverPlateLength, drainageOpenings } = params;

        // Cover plate outline
        const outline = [
            { x: 0, y: 0 },
            { x: coverPlateWidth, y: 0 },
            { x: coverPlateWidth, y: coverPlateLength },
            { x: 0, y: coverPlateLength }
        ];

        // Drainage openings
        const openings = drainageOpenings.map(opening => ({
            x: opening.x,
            y: opening.y,
            width: opening.width,
            height: opening.height,
            points: [
                { x: opening.x, y: opening.y },
                { x: opening.x + opening.width, y: opening.y },
                { x: opening.x + opening.width, y: opening.y + opening.height },
                { x: opening.x, y: opening.y + opening.height }
            ]
        }));

        return { outline, openings };
    },

    /**
     * Calculate rebar grid for plan view
     */
    calculatePlanViewRebar: function (params, rebarSpecs) {
        const { coverPlateWidth, coverPlateLength, concreteCover } = params;
        const rebars = [];

        // Longitudinal rebars (running along length)
        const numLongitudinal = Math.floor((coverPlateWidth - 2 * concreteCover) / rebarSpecs.coverPlateMain.spacing) + 1;
        for (let i = 0; i < numLongitudinal; i++) {
            const x = concreteCover + i * rebarSpecs.coverPlateMain.spacing;
            rebars.push({
                type: 'line',
                start: { x: x, y: concreteCover },
                end: { x: x, y: coverPlateLength - concreteCover },
                mark: rebarSpecs.coverPlateMain.mark,
                diameter: rebarSpecs.coverPlateMain.diameter
            });
        }

        // Transverse rebars (running across width)
        const numTransverse = Math.floor((coverPlateLength - 2 * concreteCover) / rebarSpecs.coverPlateDistribution.spacing) + 1;
        for (let i = 0; i < numTransverse; i++) {
            const y = concreteCover + i * rebarSpecs.coverPlateDistribution.spacing;
            rebars.push({
                type: 'line',
                start: { x: concreteCover, y: y },
                end: { x: coverPlateWidth - concreteCover, y: y },
                mark: rebarSpecs.coverPlateDistribution.mark,
                diameter: rebarSpecs.coverPlateDistribution.diameter
            });
        }

        return rebars;
    },

    /**
     * Helper: Distribute rebars along a line
     */
    distributeRebarsAlongLine: function (start, end, spacing, mark, diameter) {
        const rebars = [];
        const dx = end.x - start.x;
        const dy = end.y - start.y;
        const length = Math.sqrt(dx * dx + dy * dy);
        const numRebars = Math.floor(length / spacing) + 1;

        for (let i = 0; i < numRebars; i++) {
            const t = i / Math.max(numRebars - 1, 1);
            rebars.push({
                type: 'point',
                x: start.x + t * dx,
                y: start.y + t * dy,
                mark: mark,
                diameter: diameter
            });
        }

        return rebars;
    },

    /**
     * Calculate dimension lines for cross-section
     */
    calculateCrossSectionDimensions: function (params) {
        const { internalWidth, internalHeight, wallThickness, baseThickness } = params;
        const totalWidth = internalWidth + 2 * wallThickness;
        const totalHeight = internalHeight + baseThickness;
        const offset = 50; // Offset for dimension lines

        const dimensions = [];

        // Total width dimension (bottom)
        dimensions.push({
            type: 'horizontal',
            start: { x: 0, y: -offset },
            end: { x: totalWidth, y: -offset },
            text: totalWidth.toString(),
            textPosition: { x: totalWidth / 2, y: -offset - 20 }
        });

        // Internal width dimension
        dimensions.push({
            type: 'horizontal',
            start: { x: wallThickness, y: -offset - 60 },
            end: { x: wallThickness + internalWidth, y: -offset - 60 },
            text: internalWidth.toString(),
            textPosition: { x: wallThickness + internalWidth / 2, y: -offset - 80 }
        });

        // Total height dimension (right side)
        dimensions.push({
            type: 'vertical',
            start: { x: totalWidth + offset, y: 0 },
            end: { x: totalWidth + offset, y: totalHeight },
            text: totalHeight.toString(),
            textPosition: { x: totalWidth + offset + 30, y: totalHeight / 2 }
        });

        // Internal height dimension
        dimensions.push({
            type: 'vertical',
            start: { x: totalWidth + offset + 60, y: baseThickness },
            end: { x: totalWidth + offset + 60, y: totalHeight },
            text: internalHeight.toString(),
            textPosition: { x: totalWidth + offset + 90, y: baseThickness + internalHeight / 2 }
        });

        // Wall thickness dimension (left)
        dimensions.push({
            type: 'vertical',
            start: { x: -offset, y: baseThickness },
            end: { x: -offset, y: totalHeight },
            text: wallThickness.toString(),
            textPosition: { x: -offset - 30, y: baseThickness + (totalHeight - baseThickness) / 2 }
        });

        // Base thickness dimension
        dimensions.push({
            type: 'vertical',
            start: { x: -offset - 60, y: 0 },
            end: { x: -offset - 60, y: baseThickness },
            text: baseThickness.toString(),
            textPosition: { x: -offset - 90, y: baseThickness / 2 }
        });

        return dimensions;
    }
};
