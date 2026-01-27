/**
 * CrossSectionView.js - 横断面视图模块
 * 绘制 U 形槽的横断面图（引道横断面图）
 */

/**
 * 横断面视图类
 */
class CrossSectionView {
    /**
     * @param {ParameterMapper} params - 参数映射器实例
     * @param {Object} origin - 视图原点 {x, y}
     */
    constructor(params, origin = { x: 0, y: 0 }) {
        this.params = params;
        this.origin = origin;
        this.offsetX = origin.x;
        this.offsetY = origin.y;
        
        // 获取几何参数
        this.geo = params.getCrossSectionGeometry();
        this.p = params.getParams();
        
        // 图元集合
        this.entities = [];
        
        // 获取 EntityBuilders
        this.builders = window.EntityBuilders;
    }

    /**
     * 构建所有图元
     * @returns {Object} {entities, basePoint}
     */
    build() {
        this.entities = [];
        
        // 构建各层图元
        this.buildOuterContour();
        this.buildInnerContour();
        this.buildRebars();
        this.buildDimensions();
        this.buildLabel();
        
        return {
            entities: this.entities,
            basePoint: this.builders.createPoint(this.offsetX, this.offsetY)
        };
    }

    /**
     * 构建外轮廓
     */
    buildOuterContour() {
        const g = this.geo;
        const height = g.height;
        
        // 外轮廓点序列（顺时针从左上角开始）
        const outerPoints = [
            { x: -g.outerHalfWidth, y: height },
            { x: g.outerHalfWidth, y: height },
            { x: g.outerHalfWidth, y: -g.bottomThickness },
            { x: -g.outerHalfWidth, y: -g.bottomThickness }
        ];

        const polyline = this.builders.createClosedPolyline(outerPoints, {
            color: this.builders.colors.black,
            offsetX: this.offsetX,
            offsetY: this.offsetY
        });

        if (polyline) {
            this.entities.push(polyline);
        }
    }

    /**
     * 构建内轮廓（含腋角）
     */
    buildInnerContour() {
        const g = this.geo;
        const height = g.height;
        
        // 内轮廓点序列（开放多段线）
        const innerPoints = [
            { x: -g.halfWidth, y: height },
            { x: -g.halfWidth, y: g.haunchHeight },
            { x: -g.halfWidth + g.haunchWidth, y: 0 },
            { x: g.halfWidth - g.haunchWidth, y: 0 },
            { x: g.halfWidth, y: g.haunchHeight },
            { x: g.halfWidth, y: height }
        ];

        const polyline = this.builders.createOpenPolyline(innerPoints, {
            color: this.builders.colors.black,
            offsetX: this.offsetX,
            offsetY: this.offsetY
        });

        if (polyline) {
            this.entities.push(polyline);
        }
    }

    /**
     * 构建钢筋
     */
    buildRebars() {
        const g = this.geo;
        const c = g.cover;
        const height = g.height;

        // N1: 外层 U 形主筋
        this._buildRebarN1(g, c, height);
        
        // N2: 内层 U 形主筋
        this._buildRebarN2(g, c, height);
        
        // N3: 底板下层主筋（点表示）
        this._buildRebarN3(g, c);
        
        // N4: 底板角筋
        this._buildRebarN4(g, c);
        
        // N5/N6: 侧墙分布筋（点表示）
        this._buildDistributionRebars(g, c, height);
        
        // N7/N8: 拉结筋（点表示）
        this._buildTieRebars(g, c, height);
    }

    /**
     * 构建 N1 外层 U 形主筋
     */
    _buildRebarN1(g, c, height) {
        const spec = this.params.getRebarSpec('N1');
        
        const n1Points = [
            { x: -g.outerHalfWidth + c, y: height - c },
            { x: -g.outerHalfWidth + c, y: -g.bottomThickness + c },
            { x: g.outerHalfWidth - c, y: -g.bottomThickness + c },
            { x: g.outerHalfWidth - c, y: height - c }
        ];

        const rebar = this.builders.createRebarLine('N1', n1Points, spec.diameter, {
            color: this.builders.colors.red,
            offsetX: this.offsetX,
            offsetY: this.offsetY
        });

        if (rebar) {
            this.entities.push(rebar);
            
            // 添加标签
            const label = this.builders.createRebarLabel('N1', 
                n1Points[0].x - 15, 
                n1Points[0].y + 15,
                { offsetX: this.offsetX, offsetY: this.offsetY }
            );
            if (label) this.entities.push(label);
        }
    }

    /**
     * 构建 N2 内层 U 形主筋
     */
    _buildRebarN2(g, c, height) {
        const spec = this.params.getRebarSpec('N2');
        
        const n2Points = [
            { x: -g.halfWidth - c, y: height - c },
            { x: -g.halfWidth - c, y: c },
            { x: g.halfWidth + c, y: c },
            { x: g.halfWidth + c, y: height - c }
        ];

        const rebar = this.builders.createRebarLine('N2', n2Points, spec.diameter, {
            color: this.builders.colors.red,
            offsetX: this.offsetX,
            offsetY: this.offsetY
        });

        if (rebar) {
            this.entities.push(rebar);
            
            // 添加标签
            const label = this.builders.createRebarLabel('N2', 
                n2Points[0].x + 10, 
                n2Points[0].y - 30,
                { offsetX: this.offsetX, offsetY: this.offsetY }
            );
            if (label) this.entities.push(label);
        }
    }

    /**
     * 构建 N3 底板下层主筋
     */
    _buildRebarN3(g, c) {
        const spec = this.params.getRebarSpec('N3');
        const baseY = -g.bottomThickness + c + 2;
        const points = [];
        const count = 5;

        for (let i = 0; i < count; i++) {
            const t = i / (count - 1);
            points.push({
                x: (-g.halfWidth + c + 6) + (g.halfWidth * 2 - 2 * c - 12) * t,
                y: baseY
            });
        }

        const circles = this.builders.createRebarPoints('N3', points, spec.diameter, {
            color: this.builders.colors.red,
            offsetX: this.offsetX,
            offsetY: this.offsetY
        });

        this.entities.push(...circles);

        // 标签
        if (points.length > 0) {
            const label = this.builders.createRebarLabel('N3',
                points[0].x + 10,
                points[0].y + 10,
                { offsetX: this.offsetX, offsetY: this.offsetY }
            );
            if (label) this.entities.push(label);
        }
    }

    /**
     * 构建 N4 底板角筋
     */
    _buildRebarN4(g, c) {
        const spec = this.params.getRebarSpec('N4');
        const cornerOffset = 8;

        // 左下角筋
        const leftPoints = [
            { x: -g.outerHalfWidth + c, y: -g.bottomThickness + c },
            { x: -g.outerHalfWidth + c, y: -g.bottomThickness + c + 20 },
            { x: -g.outerHalfWidth + c + cornerOffset, y: -g.bottomThickness + c + 20 }
        ];

        const leftRebar = this.builders.createRebarLine('N4', leftPoints, spec.diameter, {
            color: this.builders.colors.red,
            offsetX: this.offsetX,
            offsetY: this.offsetY
        });
        if (leftRebar) {
            this.entities.push(leftRebar);
            
            // 标签
            const label = this.builders.createRebarLabel('N4',
                leftPoints[0].x - 15,
                leftPoints[0].y + 15,
                { offsetX: this.offsetX, offsetY: this.offsetY }
            );
            if (label) this.entities.push(label);
        }

        // 右下角筋（无标签）
        const rightPoints = [
            { x: g.outerHalfWidth - c, y: -g.bottomThickness + c },
            { x: g.outerHalfWidth - c, y: -g.bottomThickness + c + 20 },
            { x: g.outerHalfWidth - c - cornerOffset, y: -g.bottomThickness + c + 20 }
        ];

        const rightRebar = this.builders.createRebarLine('N4', rightPoints, spec.diameter, {
            color: this.builders.colors.red,
            offsetX: this.offsetX,
            offsetY: this.offsetY
        });
        if (rightRebar) this.entities.push(rightRebar);
    }

    /**
     * 构建分布筋 (N5/N6)
     */
    _buildDistributionRebars(g, c, height) {
        const points = [];
        const count = 4;

        for (let i = 0; i < count; i++) {
            const t = (i + 1) / (count + 1);
            // 左侧墙
            points.push({ x: -g.halfWidth + c + 2, y: t * (height - c - 10) });
            // 右侧墙
            points.push({ x: g.halfWidth - c - 2, y: t * (height - c - 10) });
        }

        const circles = this.builders.createRebarPoints('N5', points, 12, {
            color: this.builders.colors.red,
            offsetX: this.offsetX,
            offsetY: this.offsetY
        });

        this.entities.push(...circles);
    }

    /**
     * 构建拉结筋 (N7/N8)
     */
    _buildTieRebars(g, c, height) {
        // N7: 侧墙拉结筋
        const wallTiePoints = [];
        for (let i = 0; i < 3; i++) {
            const t = (i + 1) / 4;
            wallTiePoints.push({ x: -g.halfWidth + c + 4, y: t * (height - c - 20) });
            wallTiePoints.push({ x: g.halfWidth - c - 4, y: t * (height - c - 20) });
        }

        const wallTies = this.builders.createRebarPoints('N7', wallTiePoints, 8, {
            color: this.builders.colors.red,
            offsetX: this.offsetX,
            offsetY: this.offsetY
        });
        this.entities.push(...wallTies);

        // N8: 底板拉结筋
        const baseTiePoints = [];
        for (let i = 0; i < 4; i++) {
            const t = (i + 1) / 5;
            baseTiePoints.push({
                x: (-g.halfWidth + c + 10) + (g.halfWidth * 2 - 2 * c - 20) * t,
                y: c + 6
            });
        }

        const baseTies = this.builders.createRebarPoints('N8', baseTiePoints, 8, {
            color: this.builders.colors.red,
            offsetX: this.offsetX,
            offsetY: this.offsetY
        });
        this.entities.push(...baseTies);
    }

    /**
     * 构建标注
     */
    buildDimensions() {
        const g = this.geo;
        const p = this.p;
        const height = g.height;

        // 底部总宽度标注 (D)
        const dimWidth = this.builders.createAlignedDimension(
            { x: -g.outerHalfWidth, y: -g.bottomThickness - 50 },
            { x: g.outerHalfWidth, y: -g.bottomThickness - 50 },
            { x: 0, y: -g.bottomThickness - 65 },
            {
                text: `D = ${p.totalWidth.toFixed(1)}`,
                offsetX: this.offsetX,
                offsetY: this.offsetY
            }
        );
        if (dimWidth) this.entities.push(dimWidth);

        // 内净宽度标注
        const dimInner = this.builders.createAlignedDimension(
            { x: -g.halfWidth, y: height + 20 },
            { x: g.halfWidth, y: height + 20 },
            { x: 0, y: height + 35 },
            {
                text: `${p.internalWidth.toFixed(1)}`,
                offsetX: this.offsetX,
                offsetY: this.offsetY
            }
        );
        if (dimInner) this.entities.push(dimInner);

        // 右侧高度标注 (H)
        const dimHeight = this.builders.createRotatedDimension(
            { x: g.outerHalfWidth + 20, y: 0 },
            { x: g.outerHalfWidth + 20, y: height },
            { x: g.outerHalfWidth + 35, y: height / 2 },
            Math.PI / 2,
            {
                text: `H = ${height.toFixed(1)}`,
                offsetX: this.offsetX,
                offsetY: this.offsetY
            }
        );
        if (dimHeight) this.entities.push(dimHeight);
    }

    /**
     * 构建视图标签
     */
    buildLabel() {
        const label = this.builders.createCenteredText(
            '引道横断面图',
            this.offsetX,
            this.offsetY - 300,
            { height: 80 }
        );
        if (label) this.entities.push(label);
    }

    /**
     * 构建钢筋明细标注
     * @returns {Object|null} MText 实体
     */
    buildRebarCallouts() {
        const callouts = this.params.getAllRebarCallouts();
        const MX = window.MxCAD || {};
        
        const mtext = this.builders.createMText(callouts, this.offsetX + 1500, this.offsetY + 500, {
            height: 45,
            width: 1000,
            color: this.builders.colors.red,
            attachment: MX.McDb?.AttachmentPoint?.kTopLeft
        });

        if (mtext) {
            this.entities.push(mtext);
        }

        return mtext;
    }

    /**
     * 获取所有图元
     * @returns {Array}
     */
    getEntities() {
        return this.entities;
    }

    /**
     * 获取标注图元（用于单独处理）
     * @returns {Array}
     */
    getDimensionEntities() {
        return this.entities.filter(e => e && e._isDimension);
    }

    /**
     * 获取非标注图元（用于创建块）
     * @returns {Array}
     */
    getBlockEntities() {
        return this.entities.filter(e => !e || !e._isDimension);
    }
}

// 导出为全局变量
if (typeof window !== 'undefined') {
    window.CrossSectionView = CrossSectionView;
}

// 支持 CommonJS 模块导出
if (typeof module !== 'undefined' && module.exports) {
    module.exports = CrossSectionView;
}
