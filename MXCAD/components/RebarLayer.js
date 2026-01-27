/**
 * RebarLayer.js - 钢筋图层管理模块
 * 管理钢筋实体的创建、组织和数量统计
 */

/**
 * 钢筋图层类
 */
class RebarLayer {
    constructor() {
        // 钢筋实体集合，按 ID 分组
        this.rebarGroups = new Map();
        
        // 所有钢筋实体
        this.entities = [];
        
        // 钢筋规格缓存
        this.specs = new Map();
        
        // 获取 EntityBuilders
        this.builders = window.EntityBuilders;
    }

    /**
     * 添加钢筋实体
     * @param {string} id - 钢筋编号 (N1-N8)
     * @param {Object|Array} entity - 钢筋实体或实体数组
     */
    addRebar(id, entity) {
        if (!this.rebarGroups.has(id)) {
            this.rebarGroups.set(id, []);
        }

        if (Array.isArray(entity)) {
            this.rebarGroups.get(id).push(...entity);
            this.entities.push(...entity);
        } else if (entity) {
            this.rebarGroups.get(id).push(entity);
            this.entities.push(entity);
        }
    }

    /**
     * 设置钢筋规格
     * @param {string} id - 钢筋编号
     * @param {Object} spec - 规格 {diameter, spacing, grade, quantity}
     */
    setSpec(id, spec) {
        this.specs.set(id, spec);
    }

    /**
     * 获取钢筋规格
     * @param {string} id - 钢筋编号
     * @returns {Object|null}
     */
    getSpec(id) {
        return this.specs.get(id) || null;
    }

    /**
     * 创建线型钢筋并添加到图层
     * @param {string} id - 钢筋编号
     * @param {Array} points - 点数组
     * @param {number} diameter - 直径
     * @param {Object} options - 配置选项
     * @returns {Object} 钢筋实体
     */
    createLineRebar(id, points, diameter, options = {}) {
        const rebar = this.builders.createRebarLine(id, points, diameter, options);
        if (rebar) {
            this.addRebar(id, rebar);
        }
        return rebar;
    }

    /**
     * 创建点型钢筋（圆形）并添加到图层
     * @param {string} id - 钢筋编号
     * @param {Array} points - 点数组
     * @param {number} diameter - 直径
     * @param {Object} options - 配置选项
     * @returns {Array} 钢筋实体数组
     */
    createPointRebars(id, points, diameter, options = {}) {
        const rebars = this.builders.createRebarPoints(id, points, diameter, options);
        if (rebars && rebars.length > 0) {
            this.addRebar(id, rebars);
        }
        return rebars;
    }

    /**
     * 创建钢筋网格并添加到图层
     * @param {string} id - 钢筋编号
     * @param {Object} bounds - 边界 {x, y, width, height}
     * @param {number} spacing - 间距
     * @param {string} direction - 方向 'horizontal' | 'vertical'
     * @param {number} diameter - 直径
     * @param {Object} options - 配置选项
     * @returns {Array} 钢筋实体数组
     */
    createRebarGrid(id, bounds, spacing, direction, diameter, options = {}) {
        const lines = this.builders.createRebarGrid(bounds, spacing, direction, diameter, options);
        if (lines && lines.length > 0) {
            this.addRebar(id, lines);
        }
        return lines;
    }

    /**
     * 获取指定编号的钢筋实体
     * @param {string} id - 钢筋编号
     * @returns {Array}
     */
    getRebarsByID(id) {
        return this.rebarGroups.get(id) || [];
    }

    /**
     * 获取所有钢筋实体
     * @returns {Array}
     */
    getAllEntities() {
        return this.entities;
    }

    /**
     * 获取钢筋编号列表
     * @returns {Array<string>}
     */
    getRebarIDs() {
        return Array.from(this.rebarGroups.keys());
    }

    /**
     * 统计钢筋数量
     * @param {string} id - 钢筋编号（可选，不传则统计所有）
     * @returns {Object} {id: count, ...}
     */
    countRebars(id = null) {
        if (id) {
            const rebars = this.getRebarsByID(id);
            return { [id]: rebars.length };
        }

        const counts = {};
        this.rebarGroups.forEach((rebars, rebarId) => {
            counts[rebarId] = rebars.length;
        });
        return counts;
    }

    /**
     * 计算钢筋总重量
     * @param {string} id - 钢筋编号（可选）
     * @returns {number} 总重量 (kg)
     */
    calculateWeight(id = null) {
        const ids = id ? [id] : this.getRebarIDs();
        let totalWeight = 0;

        ids.forEach(rebarId => {
            const spec = this.specs.get(rebarId);
            if (!spec) return;

            const rebars = this.getRebarsByID(rebarId);
            const diameter = spec.diameter;
            const weightPerMeter = (diameter * diameter) / 162.0; // kg/m

            rebars.forEach(rebar => {
                // 估算长度（简化计算）
                let length = 0;
                if (rebar._rebarDiameter) {
                    // 如果实体有长度信息
                    length = spec.length || 1000; // 默认 1m
                }
                totalWeight += (length / 1000) * weightPerMeter;
            });
        });

        return totalWeight;
    }

    /**
     * 清空图层
     */
    clear() {
        this.rebarGroups.clear();
        this.entities = [];
        this.specs.clear();
    }

    /**
     * 创建钢筋标签
     * @param {string} id - 钢筋编号
     * @param {number} x - X 位置
     * @param {number} y - Y 位置
     * @param {Object} options - 配置选项
     * @returns {Object} 文本实体
     */
    createLabel(id, x, y, options = {}) {
        return this.builders.createRebarLabel(id, x, y, options);
    }

    /**
     * 导出钢筋明细表数据
     * @returns {Array<Object>}
     */
    exportSchedule() {
        const schedule = [];
        
        this.rebarGroups.forEach((rebars, id) => {
            const spec = this.specs.get(id) || {};
            schedule.push({
                id,
                diameter: spec.diameter || 12,
                grade: spec.grade || 'HRB400',
                spacing: spec.spacing || 200,
                quantity: rebars.length,
                description: spec.description || ''
            });
        });

        return schedule;
    }

    /**
     * 从 ParameterMapper 批量设置规格
     * @param {ParameterMapper} params - 参数映射器
     */
    setSpecsFromParams(params) {
        const ids = ['N1', 'N2', 'N3', 'N4', 'N5', 'N6', 'N7', 'N8'];
        ids.forEach(id => {
            const spec = params.getRebarSpec(id);
            if (spec) {
                this.setSpec(id, spec);
            }
        });
    }
}

// 导出为全局变量
if (typeof window !== 'undefined') {
    window.RebarLayer = RebarLayer;
}

// 支持 CommonJS 模块导出
if (typeof module !== 'undefined' && module.exports) {
    module.exports = RebarLayer;
}
