/**
 * PlanView.js - 平面布置视图模块
 * 绘制 A-A 底板平面布置图
 */

/**
 * 平面布置视图类
 */
class PlanView {
    /**
     * @param {ParameterMapper} params - 参数映射器实例
     * @param {Object} origin - 视图原点 {x, y}
     * @param {Object} options - 配置选项
     */
    constructor(params, origin = { x: 0, y: 0 }, options = {}) {
        this.params = params;
        this.origin = origin;
        this.offsetX = origin.x;
        this.offsetY = origin.y;
        
        // 获取参数
        this.p = params.getParams();
        
        // 视图配置
        this.options = {
            showVerticalBars: true,     // 是否显示纵向钢筋
            showHorizontalBars: true,   // 是否显示横向钢筋
            showDimensions: true,       // 是否显示标注
            showLabel: true,            // 是否显示视图标签
            labelText: 'A-A 平面布置图',
            ...options
        };
        
        // 计算尺寸
        this.width = this.p.length1 > 0 ? this.p.length1 : 2000;
        this.height = this.p.totalWidth > 0 ? this.p.totalWidth : 1500;
        
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
        this.buildOutline();
        
        if (this.options.showVerticalBars) {
            this.buildVerticalRebars();
        }
        
        if (this.options.showHorizontalBars) {
            this.buildHorizontalRebars();
        }
        
        if (this.options.showDimensions) {
            this.buildDimensions();
        }
        
        if (this.options.showLabel) {
            this.buildLabel();
        }
        
        return {
            entities: this.entities,
            basePoint: this.builders.createPoint(this.offsetX, this.offsetY)
        };
    }

    /**
     * 构建外轮廓矩形
     */
    buildOutline() {
        const outline = this.builders.createRectangle(
            this.offsetX,
            this.offsetY,
            this.width,
            this.height,
            { color: this.builders.colors.black }
        );

        if (outline) {
            this.entities.push(outline);
        }
    }

    /**
     * 构建纵向钢筋 (N5/N6 分布筋)
     */
    buildVerticalRebars() {
        const count = this.p.rebarCountM1 > 0 ? Math.min(this.p.rebarCountM1, 20) : 10;
        const spacing = this.width / (count + 1);
        const margin = 50;

        for (let i = 1; i <= count; i++) {
            const x = this.offsetX + i * spacing;
            const line = this.builders.createLine(
                { x: x - this.offsetX, y: margin },
                { x: x - this.offsetX, y: this.height - margin },
                {
                    color: this.builders.colors.red,
                    offsetX: this.offsetX,
                    offsetY: this.offsetY
                }
            );
            if (line) this.entities.push(line);
        }
    }

    /**
     * 构建横向钢筋 (N3/N4 主筋)
     */
    buildHorizontalRebars() {
        const spec = this.params.getRebarSpec('N3');
        const count = Math.floor(this.height / spec.spacing) || 8;
        const spacing = this.height / (count + 1);
        const margin = 50;

        for (let i = 1; i <= count; i++) {
            const y = this.offsetY + i * spacing;
            const line = this.builders.createLine(
                { x: margin, y: y - this.offsetY },
                { x: this.width - margin, y: y - this.offsetY },
                {
                    color: this.builders.colors.red,
                    offsetX: this.offsetX,
                    offsetY: this.offsetY
                }
            );
            if (line) this.entities.push(line);
        }
    }

    /**
     * 构建标注
     */
    buildDimensions() {
        // 底部宽度标注 (L1)
        const dimWidth = this.builders.createAlignedDimension(
            { x: 0, y: -50 },
            { x: this.width, y: -50 },
            { x: this.width / 2, y: -80 },
            {
                text: `L1 = ${this.width.toFixed(0)}`,
                offsetX: this.offsetX,
                offsetY: this.offsetY
            }
        );
        if (dimWidth) this.entities.push(dimWidth);

        // 右侧高度标注 (D)
        const dimHeight = this.builders.createRotatedDimension(
            { x: this.width + 50, y: 0 },
            { x: this.width + 50, y: this.height },
            { x: this.width + 80, y: this.height / 2 },
            Math.PI / 2,
            {
                text: `D = ${this.height.toFixed(0)}`,
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
            this.options.labelText,
            this.offsetX + this.width / 2,
            this.offsetY - 150,
            { height: 80 }
        );
        if (label) this.entities.push(label);
    }

    /**
     * 获取所有图元
     * @returns {Array}
     */
    getEntities() {
        return this.entities;
    }

    /**
     * 获取视图尺寸
     * @returns {Object} {width, height}
     */
    getSize() {
        return {
            width: this.width,
            height: this.height
        };
    }

    /**
     * 获取视图边界
     * @returns {Object} {minX, minY, maxX, maxY}
     */
    getBounds() {
        return {
            minX: this.offsetX,
            minY: this.offsetY,
            maxX: this.offsetX + this.width,
            maxY: this.offsetY + this.height
        };
    }
}

// 导出为全局变量
if (typeof window !== 'undefined') {
    window.PlanView = PlanView;
}

// 支持 CommonJS 模块导出
if (typeof module !== 'undefined' && module.exports) {
    module.exports = PlanView;
}
