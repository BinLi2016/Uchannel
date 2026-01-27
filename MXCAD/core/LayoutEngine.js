/**
 * LayoutEngine.js - 布局引擎模块
 * 管理图纸中视图区域的定位和布局
 */

/**
 * 视图区域类
 */
class ViewRegion {
    /**
     * @param {string} name - 区域名称
     * @param {Object} bounds - 边界配置
     */
    constructor(name, bounds) {
        this.name = name;
        this.x = bounds.x || 0;
        this.y = bounds.y || 0;
        this.width = bounds.width || bounds.w || 0;
        this.height = bounds.height || bounds.h || 0;
    }

    /**
     * 获取区域原点
     * @returns {Object} {x, y}
     */
    get origin() {
        return { x: this.x, y: this.y };
    }

    /**
     * 获取区域中心点
     * @returns {Object} {x, y}
     */
    get center() {
        return {
            x: this.x + this.width / 2,
            y: this.y + this.height / 2
        };
    }

    /**
     * 获取边界框
     * @returns {Object} {minX, minY, maxX, maxY}
     */
    get bounds() {
        return {
            minX: this.x,
            minY: this.y,
            maxX: this.x + this.width,
            maxY: this.y + this.height
        };
    }

    /**
     * 检查点是否在区域内
     * @param {number} px - X 坐标
     * @param {number} py - Y 坐标
     * @returns {boolean}
     */
    containsPoint(px, py) {
        const b = this.bounds;
        return px >= b.minX && px <= b.maxX && py >= b.minY && py <= b.maxY;
    }
}

/**
 * 布局引擎类
 */
class LayoutEngine {
    /**
     * @param {Object} sheetSize - 图纸尺寸
     */
    constructor(sheetSize = { width: 10000, height: 6000 }) {
        this.sheetWidth = sheetSize.width;
        this.sheetHeight = sheetSize.height;
        this.regions = new Map();
        this.scale = 1.0;
    }

    /**
     * 定义视图区域
     * @param {string} name - 区域名称
     * @param {Object} bounds - 边界配置 {x, y, width/w, height/h}
     * @returns {ViewRegion}
     */
    defineRegion(name, bounds) {
        const region = new ViewRegion(name, bounds);
        this.regions.set(name, region);
        return region;
    }

    /**
     * 获取视图区域
     * @param {string} name - 区域名称
     * @returns {ViewRegion|null}
     */
    getRegion(name) {
        return this.regions.get(name) || null;
    }

    /**
     * 获取视图原点
     * @param {string} name - 区域名称
     * @returns {Object|null} {x, y}
     */
    getViewOrigin(name) {
        const region = this.getRegion(name);
        return region ? region.origin : null;
    }

    /**
     * 获取视图中心
     * @param {string} name - 区域名称
     * @returns {Object|null} {x, y}
     */
    getViewCenter(name) {
        const region = this.getRegion(name);
        return region ? region.center : null;
    }

    /**
     * 设置比例尺
     * @param {number} scale - 比例因子
     */
    setScale(scale) {
        this.scale = scale;
    }

    /**
     * 应用比例变换到坐标
     * @param {number} x - X 坐标
     * @param {number} y - Y 坐标
     * @returns {Object} {x, y}
     */
    applyScale(x, y) {
        return {
            x: x * this.scale,
            y: y * this.scale
        };
    }

    /**
     * 获取所有区域名称
     * @returns {Array<string>}
     */
    getRegionNames() {
        return Array.from(this.regions.keys());
    }

    /**
     * 遍历所有区域
     * @param {Function} callback - 回调函数 (region, name) => void
     */
    forEachRegion(callback) {
        this.regions.forEach((region, name) => callback(region, name));
    }

    /**
     * 计算图纸边界
     * @returns {Object} {minX, minY, maxX, maxY}
     */
    getSheetBounds() {
        let minX = Infinity, minY = Infinity;
        let maxX = -Infinity, maxY = -Infinity;

        this.regions.forEach(region => {
            const b = region.bounds;
            minX = Math.min(minX, b.minX);
            minY = Math.min(minY, b.minY);
            maxX = Math.max(maxX, b.maxX);
            maxY = Math.max(maxY, b.maxY);
        });

        return { minX, minY, maxX, maxY };
    }

    /**
     * 创建 U 形槽钢筋布置图标准布局
     * @returns {LayoutEngine}
     */
    static createUChannelLayout() {
        const layout = new LayoutEngine({ width: 10000, height: 6000 });
        
        // 主视图区域 - 2x2 布局
        // 左上：横断面图
        layout.defineRegion('crossSection', {
            x: 0,
            y: 800,
            width: 2500,
            height: 1500
        });
        
        // 右上：B-B 侧墙立面图
        layout.defineRegion('elevationBB', {
            x: 3000,
            y: 1500,
            width: 2500,
            height: 1200
        });
        
        // 左下：A-A 平面布置图
        layout.defineRegion('planAA', {
            x: 0,
            y: -1500,
            width: 2500,
            height: 2000
        });
        
        // 右下：C-C 立面图 + 详图
        layout.defineRegion('elevationCC', {
            x: 3000,
            y: -1500,
            width: 2500,
            height: 2000
        });

        // 辅助区域
        // 表格区域
        layout.defineRegion('tables', {
            x: -2500,
            y: -2500,
            width: 9000,
            height: 800
        });

        // 施工说明区域
        layout.defineRegion('notes', {
            x: 6000,
            y: 1500,
            width: 2500,
            height: 800
        });

        // 标题栏区域
        layout.defineRegion('titleBlock', {
            x: 4000,
            y: 2500,
            width: 4000,
            height: 300
        });

        // 图框区域
        layout.defineRegion('border', {
            x: -3000,
            y: -2500,
            width: 13000,
            height: 5500
        });

        return layout;
    }

    /**
     * 创建简化布局（仅用于单视图测试）
     * @returns {LayoutEngine}
     */
    static createSimpleLayout() {
        const layout = new LayoutEngine({ width: 5000, height: 3000 });
        
        layout.defineRegion('main', {
            x: 0,
            y: 0,
            width: 3000,
            height: 2000
        });

        layout.defineRegion('tables', {
            x: 0,
            y: -1000,
            width: 3000,
            height: 500
        });

        return layout;
    }

    /**
     * 导出布局配置为 JSON
     * @returns {Object}
     */
    toJSON() {
        const regions = {};
        this.regions.forEach((region, name) => {
            regions[name] = {
                x: region.x,
                y: region.y,
                width: region.width,
                height: region.height
            };
        });
        
        return {
            sheetWidth: this.sheetWidth,
            sheetHeight: this.sheetHeight,
            scale: this.scale,
            regions
        };
    }

    /**
     * 从 JSON 恢复布局配置
     * @param {Object} json - 配置对象
     * @returns {LayoutEngine}
     */
    static fromJSON(json) {
        const layout = new LayoutEngine({
            width: json.sheetWidth,
            height: json.sheetHeight
        });
        layout.scale = json.scale || 1.0;
        
        for (const [name, bounds] of Object.entries(json.regions || {})) {
            layout.defineRegion(name, bounds);
        }
        
        return layout;
    }
}

// 视图位置常量（兼容旧代码）
const ViewPositions = {
    SECTION_ORIGIN: { x: 0, y: 800 },
    AA_ORIGIN: { x: 3000, y: 1500 },
    BB_ORIGIN: { x: 3000, y: 200 },
    CC_ORIGIN: { x: 5500, y: 200 },
    TABLE_Y: -1000,
    NOTES_ORIGIN: { x: 7500, y: 2000 }
};

// 导出为全局变量
if (typeof window !== 'undefined') {
    window.LayoutEngine = LayoutEngine;
    window.ViewRegion = ViewRegion;
    window.ViewPositions = ViewPositions;
}

// 支持 CommonJS 模块导出
if (typeof module !== 'undefined' && module.exports) {
    module.exports = { LayoutEngine, ViewRegion, ViewPositions };
}
