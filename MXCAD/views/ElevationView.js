/**
 * ElevationView.js - 立面视图模块
 * 绘制侧墙立面图 (B-B / C-C)
 */

/**
 * 立面视图类
 */
class ElevationView {
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
            viewType: 'BB',             // 'BB' 或 'CC'
            showVerticalBars: true,     // 是否显示竖向钢筋
            showHorizontalBars: true,   // 是否显示水平分布筋
            showDimensions: true,       // 是否显示标注
            showLabel: true,            // 是否显示视图标签
            showDetailWindow: false,    // 是否显示详图窗口（仅 CC 视图）
            labelText: null,            // 自定义标签文本
            ...options
        };
        
        // 计算尺寸
        this.width = this.p.length1 > 0 ? this.p.length1 : 2000;
        this.height = this._getHeight();
        
        // 图元集合
        this.entities = [];
        
        // 获取 EntityBuilders
        this.builders = window.EntityBuilders;
    }

    /**
     * 根据视图类型获取高度
     * @private
     * @returns {number}
     */
    _getHeight() {
        if (this.options.viewType === 'BB') {
            return this.p.heightMin > 0 ? this.p.heightMin : 1000;
        } else {
            return this.p.heightMax > 0 ? this.p.heightMax : 1200;
        }
    }

    /**
     * 获取视图标签文本
     * @private
     * @returns {string}
     */
    _getLabelText() {
        if (this.options.labelText) {
            return this.options.labelText;
        }
        return this.options.viewType === 'BB' ? 'B-B 侧墙立面图' : 'C-C 侧墙立面图';
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
        
        if (this.options.showDetailWindow && this.options.viewType === 'CC') {
            this.buildDetailWindow();
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
     * 构建墙体外轮廓
     */
    buildOutline() {
        const wall = this.builders.createRectangle(
            this.offsetX,
            this.offsetY,
            this.width,
            this.height,
            { color: this.builders.colors.black }
        );

        if (wall) {
            this.entities.push(wall);
        }
    }

    /**
     * 构建竖向钢筋 (N1/N2)
     */
    buildVerticalRebars() {
        const spec = this.params.getRebarSpec('N1');
        const count = Math.floor(this.width / spec.spacing) || 10;
        const actualSpacing = this.width / (count + 1);
        const margin = 30;

        for (let i = 1; i <= count; i++) {
            const x = this.offsetX + i * actualSpacing;
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
     * 构建水平分布筋 (N5/N6)
     */
    buildHorizontalRebars() {
        const countH = 5;
        const margin = 30;

        for (let i = 1; i < countH; i++) {
            const y = this.offsetY + i * (this.height / countH);
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
     * 构建详图窗口（仅用于 C-C 视图）
     */
    buildDetailWindow() {
        // 详图窗口位置（右下角）
        const windowWidth = this.width * 0.3;
        const windowHeight = this.height * 0.3;
        const windowX = this.offsetX + this.width - windowWidth - 100;
        const windowY = this.offsetY + 100;

        // 窗口矩形（白色填充效果用线框表示）
        const window = this.builders.createRectangle(
            windowX,
            windowY,
            windowWidth,
            windowHeight,
            { color: this.builders.colors.gray }
        );

        if (window) {
            this.entities.push(window);
        }

        // 详图标签
        const detailLabel = this.builders.createCenteredText(
            '详图',
            windowX + windowWidth / 2,
            windowY + windowHeight + 30,
            { height: 40, color: this.builders.colors.black }
        );
        if (detailLabel) this.entities.push(detailLabel);
    }

    /**
     * 构建标注
     */
    buildDimensions() {
        // 顶部宽度标注 (L1/L2)
        const lengthParam = this.options.viewType === 'BB' ? 'L1' : 'L2';
        const dimWidth = this.builders.createAlignedDimension(
            { x: 0, y: this.height + 50 },
            { x: this.width, y: this.height + 50 },
            { x: this.width / 2, y: this.height + 80 },
            {
                text: `${lengthParam} = ${this.width.toFixed(0)}`,
                offsetX: this.offsetX,
                offsetY: this.offsetY
            }
        );
        if (dimWidth) this.entities.push(dimWidth);

        // 右侧高度标注 (Hmin/Hmax)
        const heightParam = this.options.viewType === 'BB' ? 'Hmin' : 'Hmax';
        const dimHeight = this.builders.createRotatedDimension(
            { x: this.width + 50, y: 0 },
            { x: this.width + 50, y: this.height },
            { x: this.width + 80, y: this.height / 2 },
            Math.PI / 2,
            {
                text: `${heightParam} = ${this.height.toFixed(0)}`,
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
            this._getLabelText(),
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

    /**
     * 创建 B-B 视图的工厂方法
     * @param {ParameterMapper} params - 参数映射器
     * @param {Object} origin - 原点
     * @returns {ElevationView}
     */
    static createBBView(params, origin) {
        return new ElevationView(params, origin, {
            viewType: 'BB',
            labelText: 'B-B 侧墙立面图'
        });
    }

    /**
     * 创建 C-C 视图的工厂方法
     * @param {ParameterMapper} params - 参数映射器
     * @param {Object} origin - 原点
     * @param {boolean} showDetail - 是否显示详图窗口
     * @returns {ElevationView}
     */
    static createCCView(params, origin, showDetail = true) {
        return new ElevationView(params, origin, {
            viewType: 'CC',
            labelText: 'C-C 侧墙立面图',
            showDetailWindow: showDetail
        });
    }
}

// 导出为全局变量
if (typeof window !== 'undefined') {
    window.ElevationView = ElevationView;
}

// 支持 CommonJS 模块导出
if (typeof module !== 'undefined' && module.exports) {
    module.exports = ElevationView;
}
