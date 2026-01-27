/**
 * EntityBuilders.js - 图元构建器模块
 * 提供可复用的 MXCAD 图元创建函数
 */

const EntityBuilders = {
    // 默认颜色配置
    colors: {
        black: { r: 0, g: 0, b: 0 },
        red: { r: 255, g: 0, b: 0 },
        blue: { r: 0, g: 0, b: 255 },
        green: { r: 0, g: 255, b: 0 },
        gray: { r: 100, g: 100, b: 100 },
        yellow: { r: 255, g: 255, b: 0 }
    },

    // 默认标注比例
    dimScale: 40.0,

    /**
     * 获取 MXCAD 模块引用
     * @returns {Object} MxCAD 模块
     */
    getMxCAD() {
        return window.MxCAD || {};
    },

    /**
     * 创建 McCmColor 实例
     * @param {Object} color - 颜色对象 {r, g, b}
     * @returns {Object} McCmColor 实例
     */
    createColor(color) {
        const { McCmColor } = this.getMxCAD();
        if (!McCmColor) return null;
        return new McCmColor(color.r, color.g, color.b);
    },

    /**
     * 创建 3D 点
     * @param {number} x - X 坐标
     * @param {number} y - Y 坐标
     * @param {number} z - Z 坐标 (默认 0)
     * @returns {Object} McGePoint3d 实例
     */
    createPoint(x, y, z = 0) {
        const { McGePoint3d } = this.getMxCAD();
        if (!McGePoint3d) return null;
        return new McGePoint3d(x, y, z);
    },

    /**
     * 创建闭合多段线
     * @param {Array} points - 点数组 [{x, y}, ...] 或 [[x, y], ...]
     * @param {Object} options - 配置选项
     * @param {Object} options.color - 颜色
     * @param {number} options.width - 线宽
     * @param {number} options.offsetX - X 偏移
     * @param {number} options.offsetY - Y 偏移
     * @returns {Object} McDbPolyline 实例
     */
    createClosedPolyline(points, options = {}) {
        const { McDbPolyline } = this.getMxCAD();
        if (!McDbPolyline) return null;

        const {
            color = this.colors.black,
            width = 0,
            offsetX = 0,
            offsetY = 0
        } = options;

        const polyline = new McDbPolyline();
        
        points.forEach(pt => {
            const x = (pt.x !== undefined ? pt.x : pt[0]) + offsetX;
            const y = (pt.y !== undefined ? pt.y : pt[1]) + offsetY;
            polyline.addVertexAt(this.createPoint(x, y));
        });

        polyline.isClosed = true;
        polyline.trueColor = this.createColor(color);
        
        if (width > 0) {
            polyline.constantWidth = width;
        }

        return polyline;
    },

    /**
     * 创建开放多段线
     * @param {Array} points - 点数组
     * @param {Object} options - 配置选项
     * @returns {Object} McDbPolyline 实例
     */
    createOpenPolyline(points, options = {}) {
        const { McDbPolyline } = this.getMxCAD();
        if (!McDbPolyline) return null;

        const {
            color = this.colors.black,
            width = 0,
            offsetX = 0,
            offsetY = 0
        } = options;

        const polyline = new McDbPolyline();
        
        points.forEach(pt => {
            const x = (pt.x !== undefined ? pt.x : pt.X !== undefined ? pt.X : pt[0]) + offsetX;
            const y = (pt.y !== undefined ? pt.y : pt.Y !== undefined ? pt.Y : pt[1]) + offsetY;
            polyline.addVertexAt(this.createPoint(x, y));
        });

        polyline.isClosed = false;
        polyline.trueColor = this.createColor(color);
        
        if (width > 0) {
            polyline.constantWidth = width;
        }

        return polyline;
    },

    /**
     * 创建矩形
     * @param {number} x - 左下角 X
     * @param {number} y - 左下角 Y
     * @param {number} width - 宽度
     * @param {number} height - 高度
     * @param {Object} options - 配置选项
     * @returns {Object} McDbPolyline 实例
     */
    createRectangle(x, y, width, height, options = {}) {
        return this.createClosedPolyline([
            [x, y],
            [x + width, y],
            [x + width, y + height],
            [x, y + height]
        ], options);
    },

    /**
     * 创建直线
     * @param {Object} start - 起点 {x, y}
     * @param {Object} end - 终点 {x, y}
     * @param {Object} options - 配置选项
     * @returns {Object} McDbLine 实例
     */
    createLine(start, end, options = {}) {
        const { McDbLine } = this.getMxCAD();
        if (!McDbLine) return null;

        const {
            color = this.colors.black,
            offsetX = 0,
            offsetY = 0
        } = options;

        const line = new McDbLine(
            this.createPoint(start.x + offsetX, start.y + offsetY),
            this.createPoint(end.x + offsetX, end.y + offsetY)
        );
        line.trueColor = this.createColor(color);

        return line;
    },

    /**
     * 创建圆
     * @param {number} centerX - 圆心 X
     * @param {number} centerY - 圆心 Y
     * @param {number} radius - 半径
     * @param {Object} options - 配置选项
     * @returns {Object} McDbCircle 实例
     */
    createCircle(centerX, centerY, radius, options = {}) {
        const { McDbCircle } = this.getMxCAD();
        if (!McDbCircle) return null;

        const {
            color = this.colors.red,
            offsetX = 0,
            offsetY = 0
        } = options;

        const circle = new McDbCircle(
            this.createPoint(centerX + offsetX, centerY + offsetY),
            radius
        );
        circle.trueColor = this.createColor(color);

        return circle;
    },

    /**
     * 创建单行文本
     * @param {string} text - 文本内容
     * @param {number} x - X 位置
     * @param {number} y - Y 位置
     * @param {Object} options - 配置选项
     * @returns {Object} McDbText 实例
     */
    createText(text, x, y, options = {}) {
        const { McDbText, McDb } = this.getMxCAD();
        if (!McDbText) return null;

        const {
            height = 30,
            color = this.colors.black,
            horizontalMode = null,
            verticalMode = null,
            offsetX = 0,
            offsetY = 0
        } = options;

        const textEntity = new McDbText();
        textEntity.textString = text;
        textEntity.height = height;
        textEntity.position = this.createPoint(x + offsetX, y + offsetY);
        textEntity.trueColor = this.createColor(color);

        if (McDb && horizontalMode !== null) {
            textEntity.horizontalMode = horizontalMode;
            textEntity.alignmentPoint = textEntity.position;
        }

        if (McDb && verticalMode !== null) {
            textEntity.verticalMode = verticalMode;
        }

        return textEntity;
    },

    /**
     * 创建居中文本
     * @param {string} text - 文本内容
     * @param {number} x - X 位置
     * @param {number} y - Y 位置
     * @param {Object} options - 配置选项
     * @returns {Object} McDbText 实例
     */
    createCenteredText(text, x, y, options = {}) {
        const { McDb } = this.getMxCAD();
        return this.createText(text, x, y, {
            ...options,
            horizontalMode: McDb?.TextHorzMode?.kTextCenter,
            verticalMode: McDb?.TextVertMode?.kTextVertMid
        });
    },

    /**
     * 创建多行文本
     * @param {string|Array} content - 文本内容（字符串或行数组）
     * @param {number} x - X 位置
     * @param {number} y - Y 位置
     * @param {Object} options - 配置选项
     * @returns {Object} McDbMText 实例
     */
    createMText(content, x, y, options = {}) {
        const { McDbMText, McDb } = this.getMxCAD();
        if (!McDbMText) return null;

        const {
            height = 50,
            width = 1000,
            color = this.colors.black,
            attachment = McDb?.AttachmentPoint?.kTopLeft,
            offsetX = 0,
            offsetY = 0
        } = options;

        const mtext = new McDbMText();
        mtext.contents = Array.isArray(content) ? content.join('\\P') : content;
        mtext.textHeight = height;
        mtext.location = this.createPoint(x + offsetX, y + offsetY);
        mtext.trueColor = this.createColor(color);
        mtext.width = width;
        
        if (attachment) {
            mtext.attachment = attachment;
        }

        return mtext;
    },

    /**
     * 应用标注比例设置
     * @param {Object} dim - 标注实体
     */
    applyDimensionScale(dim) {
        if (!dim || typeof dim.setDimVarDouble !== 'function') return;
        dim.setDimVarDouble(40, this.dimScale); // DIMSCALE
    },

    /**
     * 创建对齐标注
     * @param {Object} start - 起点 {x, y}
     * @param {Object} end - 终点 {x, y}
     * @param {Object} linePoint - 标注线位置 {x, y}
     * @param {Object} options - 配置选项
     * @returns {Object} McDbAlignedDimension 实例
     */
    createAlignedDimension(start, end, linePoint, options = {}) {
        const { McDbAlignedDimension } = this.getMxCAD();
        if (!McDbAlignedDimension) return null;

        const {
            text = null,
            color = this.colors.black,
            offsetX = 0,
            offsetY = 0
        } = options;

        const dim = new McDbAlignedDimension();
        dim.xLine1Point = this.createPoint(start.x + offsetX, start.y + offsetY);
        dim.xLine2Point = this.createPoint(end.x + offsetX, end.y + offsetY);
        dim.dimLinePoint = this.createPoint(linePoint.x + offsetX, linePoint.y + offsetY);
        
        if (text) {
            dim.dimensionText = text;
        }
        
        dim.trueColor = this.createColor(color);
        dim._isDimension = true;
        
        this.applyDimensionScale(dim);
        
        if (typeof dim.needToUpdateDimBlock === 'function') {
            dim.needToUpdateDimBlock(true);
        }
        if (typeof dim.recomputeDimBlock === 'function') {
            dim.recomputeDimBlock();
        }

        return dim;
    },

    /**
     * 创建旋转标注
     * @param {Object} start - 起点 {x, y}
     * @param {Object} end - 终点 {x, y}
     * @param {Object} linePoint - 标注线位置 {x, y}
     * @param {number} rotation - 旋转角度（弧度）
     * @param {Object} options - 配置选项
     * @returns {Object} McDbRotatedDimension 实例
     */
    createRotatedDimension(start, end, linePoint, rotation, options = {}) {
        const { McDbRotatedDimension } = this.getMxCAD();
        if (!McDbRotatedDimension) return null;

        const {
            text = null,
            color = this.colors.black,
            offsetX = 0,
            offsetY = 0
        } = options;

        const dim = new McDbRotatedDimension();
        dim.xLine1Point = this.createPoint(start.x + offsetX, start.y + offsetY);
        dim.xLine2Point = this.createPoint(end.x + offsetX, end.y + offsetY);
        dim.dimLinePoint = this.createPoint(linePoint.x + offsetX, linePoint.y + offsetY);
        dim.rotation = rotation || 0;
        
        if (text) {
            dim.dimensionText = text;
        }
        
        dim.trueColor = this.createColor(color);
        dim._isDimension = true;
        
        this.applyDimensionScale(dim);
        
        if (typeof dim.needToUpdateDimBlock === 'function') {
            dim.needToUpdateDimBlock(true);
        }
        if (typeof dim.recomputeDimBlock === 'function') {
            dim.recomputeDimBlock();
        }

        return dim;
    },

    /**
     * 根据旋转角度自动选择标注类型
     * @param {Object} start - 起点
     * @param {Object} end - 终点
     * @param {Object} linePoint - 标注线位置
     * @param {number} rotation - 旋转角度（弧度）
     * @param {Object} options - 配置选项
     * @returns {Object} 标注实例
     */
    createDimension(start, end, linePoint, rotation = 0, options = {}) {
        if (Math.abs(rotation) > 0.001) {
            return this.createRotatedDimension(start, end, linePoint, rotation, options);
        }
        return this.createAlignedDimension(start, end, linePoint, options);
    },

    /**
     * 创建钢筋线（带宽度的多段线）
     * @param {string} id - 钢筋编号
     * @param {Array} points - 点数组
     * @param {number} diameter - 直径
     * @param {Object} options - 配置选项
     * @returns {Object} McDbPolyline 实例
     */
    createRebarLine(id, points, diameter, options = {}) {
        const {
            color = this.colors.red,
            offsetX = 0,
            offsetY = 0
        } = options;

        const polyline = this.createOpenPolyline(points, {
            color,
            width: diameter,
            offsetX,
            offsetY
        });

        if (polyline) {
            polyline._rebarId = id;
            polyline._rebarDiameter = diameter;
        }

        return polyline;
    },

    /**
     * 创建钢筋点（圆形表示）
     * @param {string} id - 钢筋编号
     * @param {Array} points - 点数组
     * @param {number} diameter - 直径
     * @param {Object} options - 配置选项
     * @returns {Array} McDbCircle 实例数组
     */
    createRebarPoints(id, points, diameter, options = {}) {
        const {
            color = this.colors.red,
            offsetX = 0,
            offsetY = 0
        } = options;

        return points.map(pt => {
            const x = (pt.x !== undefined ? pt.x : pt.X !== undefined ? pt.X : pt[0]);
            const y = (pt.y !== undefined ? pt.y : pt.Y !== undefined ? pt.Y : pt[1]);
            const circle = this.createCircle(x, y, diameter / 2, {
                color,
                offsetX,
                offsetY
            });
            if (circle) {
                circle._rebarId = id;
                circle._rebarDiameter = diameter;
            }
            return circle;
        }).filter(c => c !== null);
    },

    /**
     * 创建钢筋网格
     * @param {Object} bounds - 边界 {x, y, width, height}
     * @param {number} spacing - 间距
     * @param {string} direction - 方向 'horizontal' | 'vertical'
     * @param {number} diameter - 直径
     * @param {Object} options - 配置选项
     * @returns {Array} 直线实体数组
     */
    createRebarGrid(bounds, spacing, direction, diameter, options = {}) {
        const {
            color = this.colors.red,
            margin = 30,
            offsetX = 0,
            offsetY = 0
        } = options;

        const lines = [];
        const { x, y, width, height } = bounds;

        if (direction === 'vertical') {
            const count = Math.floor((width - 2 * margin) / spacing) || 10;
            const actualSpacing = (width - 2 * margin) / (count + 1);
            
            for (let i = 1; i <= count; i++) {
                const lineX = x + margin + i * actualSpacing;
                const line = this.createLine(
                    { x: lineX, y: y + margin },
                    { x: lineX, y: y + height - margin },
                    { color, offsetX, offsetY }
                );
                if (line) lines.push(line);
            }
        } else {
            const count = Math.floor((height - 2 * margin) / spacing) || 10;
            const actualSpacing = (height - 2 * margin) / (count + 1);
            
            for (let i = 1; i <= count; i++) {
                const lineY = y + margin + i * actualSpacing;
                const line = this.createLine(
                    { x: x + margin, y: lineY },
                    { x: x + width - margin, y: lineY },
                    { color, offsetX, offsetY }
                );
                if (line) lines.push(line);
            }
        }

        return lines;
    },

    /**
     * 创建钢筋标签
     * @param {string} id - 钢筋编号
     * @param {number} x - X 位置
     * @param {number} y - Y 位置
     * @param {Object} options - 配置选项
     * @returns {Object} McDbText 实例
     */
    createRebarLabel(id, x, y, options = {}) {
        return this.createText(id, x, y, {
            height: options.height || 30,
            color: options.color || this.colors.red,
            offsetX: options.offsetX || 0,
            offsetY: options.offsetY || 0
        });
    }
};

// 导出为全局变量
if (typeof window !== 'undefined') {
    window.EntityBuilders = EntityBuilders;
}

// 支持 CommonJS 模块导出
if (typeof module !== 'undefined' && module.exports) {
    module.exports = EntityBuilders;
}
