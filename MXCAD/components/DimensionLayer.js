/**
 * DimensionLayer.js - 标注图层管理模块
 * 管理尺寸标注实体的创建和组织
 */

/**
 * 标注图层类
 */
class DimensionLayer {
    constructor() {
        // 标注实体集合
        this.dimensions = [];
        
        // 标注分组（按位置）
        this.groups = {
            top: [],
            bottom: [],
            left: [],
            right: [],
            other: []
        };
        
        // 获取 EntityBuilders
        this.builders = window.EntityBuilders;
        
        // 默认偏移量
        this.defaultOffset = 50;
    }

    /**
     * 添加标注实体
     * @param {Object} dim - 标注实体
     * @param {string} position - 位置 ('top'|'bottom'|'left'|'right'|'other')
     */
    addDimension(dim, position = 'other') {
        if (!dim) return;
        
        this.dimensions.push(dim);
        
        if (this.groups[position]) {
            this.groups[position].push(dim);
        } else {
            this.groups.other.push(dim);
        }
    }

    /**
     * 创建水平对齐标注并添加到图层
     * @param {Object} start - 起点 {x, y}
     * @param {Object} end - 终点 {x, y}
     * @param {number} offset - 标注线偏移量（正值向上，负值向下）
     * @param {Object} options - 配置选项
     * @returns {Object} 标注实体
     */
    createHorizontalDimension(start, end, offset, options = {}) {
        const lineY = start.y + offset;
        const textX = (start.x + end.x) / 2;
        
        const dim = this.builders.createAlignedDimension(
            { x: start.x, y: lineY },
            { x: end.x, y: lineY },
            { x: textX, y: lineY + Math.sign(offset) * 15 },
            options
        );

        const position = offset > 0 ? 'top' : 'bottom';
        this.addDimension(dim, position);
        
        return dim;
    }

    /**
     * 创建垂直对齐标注并添加到图层
     * @param {Object} start - 起点 {x, y}
     * @param {Object} end - 终点 {x, y}
     * @param {number} offset - 标注线偏移量（正值向右，负值向左）
     * @param {Object} options - 配置选项
     * @returns {Object} 标注实体
     */
    createVerticalDimension(start, end, offset, options = {}) {
        const lineX = start.x + offset;
        const textY = (start.y + end.y) / 2;
        
        const dim = this.builders.createRotatedDimension(
            { x: lineX, y: start.y },
            { x: lineX, y: end.y },
            { x: lineX + Math.sign(offset) * 15, y: textY },
            Math.PI / 2,
            options
        );

        const position = offset > 0 ? 'right' : 'left';
        this.addDimension(dim, position);
        
        return dim;
    }

    /**
     * 创建宽度标注（底部）
     * @param {number} x1 - 起点 X
     * @param {number} x2 - 终点 X
     * @param {number} baseY - 基准 Y 坐标
     * @param {Object} options - 配置选项
     * @returns {Object} 标注实体
     */
    createWidthDimension(x1, x2, baseY, options = {}) {
        const offset = -(options.offset || this.defaultOffset);
        return this.createHorizontalDimension(
            { x: x1, y: baseY },
            { x: x2, y: baseY },
            offset,
            options
        );
    }

    /**
     * 创建高度标注（右侧）
     * @param {number} y1 - 起点 Y
     * @param {number} y2 - 终点 Y
     * @param {number} baseX - 基准 X 坐标
     * @param {Object} options - 配置选项
     * @returns {Object} 标注实体
     */
    createHeightDimension(y1, y2, baseX, options = {}) {
        const offset = options.offset || this.defaultOffset;
        return this.createVerticalDimension(
            { x: baseX, y: y1 },
            { x: baseX, y: y2 },
            offset,
            options
        );
    }

    /**
     * 批量创建矩形边界标注
     * @param {Object} bounds - 边界 {x, y, width, height}
     * @param {Object} options - 配置选项
     * @returns {Array} 标注实体数组
     */
    createBoundsDimensions(bounds, options = {}) {
        const dims = [];
        const { x, y, width, height } = bounds;
        const offset = options.offset || this.defaultOffset;

        // 底部宽度标注
        if (options.showWidth !== false) {
            const widthDim = this.createWidthDimension(
                x, x + width, y,
                { ...options, text: options.widthText, offset }
            );
            dims.push(widthDim);
        }

        // 右侧高度标注
        if (options.showHeight !== false) {
            const heightDim = this.createHeightDimension(
                y, y + height, x + width,
                { ...options, text: options.heightText, offset }
            );
            dims.push(heightDim);
        }

        return dims.filter(d => d !== null);
    }

    /**
     * 获取所有标注实体
     * @returns {Array}
     */
    getAllEntities() {
        return this.dimensions;
    }

    /**
     * 获取指定位置的标注
     * @param {string} position - 位置
     * @returns {Array}
     */
    getDimensionsByPosition(position) {
        return this.groups[position] || [];
    }

    /**
     * 统计标注数量
     * @returns {Object}
     */
    count() {
        const counts = { total: this.dimensions.length };
        for (const [position, dims] of Object.entries(this.groups)) {
            counts[position] = dims.length;
        }
        return counts;
    }

    /**
     * 清空图层
     */
    clear() {
        this.dimensions = [];
        this.groups = {
            top: [],
            bottom: [],
            left: [],
            right: [],
            other: []
        };
    }

    /**
     * 设置默认偏移量
     * @param {number} offset - 偏移量
     */
    setDefaultOffset(offset) {
        this.defaultOffset = offset;
    }

    /**
     * 过滤并返回仅标注实体（用于单独绘制）
     * @param {Array} entities - 混合实体数组
     * @returns {Array} 仅标注实体
     */
    static filterDimensions(entities) {
        return entities.filter(e => e && e._isDimension);
    }

    /**
     * 过滤并返回非标注实体（用于创建块）
     * @param {Array} entities - 混合实体数组
     * @returns {Array} 非标注实体
     */
    static filterNonDimensions(entities) {
        return entities.filter(e => !e || !e._isDimension);
    }
}

// 导出为全局变量
if (typeof window !== 'undefined') {
    window.DimensionLayer = DimensionLayer;
}

// 支持 CommonJS 模块导出
if (typeof module !== 'undefined' && module.exports) {
    module.exports = DimensionLayer;
}
