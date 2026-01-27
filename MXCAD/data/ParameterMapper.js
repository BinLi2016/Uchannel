/**
 * ParameterMapper.js - 参数映射器模块
 * 从参数表加载数据并标准化为统一单位（毫米）
 */

/**
 * 参数映射器类
 */
class ParameterMapper {
    /**
     * @param {string} yuType - U型槽编号 (如 "YU1")
     */
    constructor(yuType) {
        this.yuType = yuType;
        this.raw = null;
        this.normalized = null;
        this._loadFromTable();
    }

    /**
     * 从全局参数表加载原始数据
     * @private
     */
    _loadFromTable() {
        const data = window.UChannelTable1Data || [];
        const headers = window.UChannelTable1Headers || [];
        const row = data.find(r => r[0] === this.yuType);

        if (!row) {
            console.warn(`ParameterMapper: 未找到 ${this.yuType} 的参数数据`);
            this.raw = {};
            this.normalized = this._getDefaults();
            return;
        }

        // 构建原始参数对象
        this.raw = {};
        headers.forEach((header, idx) => {
            const val = Number(row[idx]);
            this.raw[header] = isNaN(val) ? row[idx] : val;
        });

        // 标准化参数（转换为 mm）
        this.normalized = this._normalize();
    }

    /**
     * 获取默认参数值
     * @private
     * @returns {Object}
     */
    _getDefaults() {
        return {
            id: this.yuType,
            totalWidth: 2285,           // mm
            wallThickness: 300,         // mm
            bottomThickness: 400,       // mm
            heightMin: 2191,            // mm
            heightMax: 2435,            // mm
            internalWidth: 2225,        // mm (D - 2*B)
            length1: 2000,              // mm
            length2: 2000,              // mm
            rebarCountN: 181,           // 数量
            rebarCountN1: 18,           // 数量
            rebarCountN1Prime: 3,       // 数量
            rebarCountM: 2,             // 数量
            rebarCountM1: 159,          // 数量
            rebarCountM2: 159,          // 数量
            spacingD: 125,              // mm
            spacingD1: 128,             // mm
            spacingD2: 115,             // mm
            spacingD1Prime: 80,         // mm
            spacingB: 70,               // mm
            spacingB1: 125,             // mm
            spacingB2: 125,             // mm
            edgeA: 65,                  // mm
            edgeE1: 80,                 // mm
            edgeE2: 80,                 // mm
            edgeE3: 62.5                // mm
        };
    }

    /**
     * 将原始参数标准化为毫米
     * @private
     * @returns {Object}
     */
    _normalize() {
        const r = this.raw;
        const getVal = (header, defaultVal = 0) => {
            const val = r[header];
            return (typeof val === 'number' && !isNaN(val)) ? val : defaultVal;
        };

        // 注意：D、L1、L2 的数据实际上已经是 mm
        // 其他 (cm) 标记的参数需要 *10 转换

        const totalWidth = getVal('D (cm)', 2285);  // 实际已是 mm
        const wallThickness = getVal('B (cm)', 30) * 10;  // cm -> mm
        const bottomThickness = getVal('l (cm)', 40) * 10;  // cm -> mm
        
        return {
            // 基本标识
            id: this.yuType,
            
            // 主要尺寸
            totalWidth: totalWidth,                                    // D - 总宽度
            wallThickness: wallThickness,                              // B - 侧壁厚度
            bottomThickness: bottomThickness,                          // l - 底板厚度
            internalWidth: totalWidth - 2 * wallThickness,             // 内净宽
            heightMin: getVal('Hmin (cm)', 219.1) * 10,                // Hmin
            heightMax: getVal('Hmax (cm)', 243.5) * 10,                // Hmax
            
            // 长度尺寸
            length1: getVal('L1 (cm)', 2000),                          // L1 - 实际已是 mm
            length2: getVal('L2 (cm)', 2000),                          // L2 - 实际已是 mm
            
            // 钢筋数量
            rebarCountN: getVal('n', 181),                             // n - 主筋数量
            rebarCountN1: getVal('n1', 18),                            // n1 - 侧墙筋数量
            rebarCountN1Prime: getVal("n1'", 3),                       // n1' - 底板筋数量
            rebarCountM: getVal('m', 2),                               // m - 分布筋排数
            rebarCountM1: getVal('m1', 159),                           // m1 - L1方向分布筋数
            rebarCountM2: getVal('m2', 159),                           // m2 - L2方向分布筋数
            
            // 钢筋间距
            spacingD: getVal('d (cm)', 12.5) * 10,                     // d - 基础间距
            spacingD1: getVal('d1 (cm)', 12.8) * 10,                   // d1 - 外侧筋间距
            spacingD2: getVal('d2 (cm)', 11.5) * 10,                   // d2 - 内侧筋间距
            spacingD1Prime: getVal("d1' (cm)", 8.0) * 10,              // d1' - 底板筋间距
            spacingB: getVal('b (cm)', 7.0) * 10,                      // b - 分布筋间距
            spacingB1: getVal('b1 (cm)', 12.5) * 10,                   // b1 - L1方向分布筋间距
            spacingB2: getVal('b2 (cm)', 12.5) * 10,                   // b2 - L2方向分布筋间距
            
            // 边距
            edgeA: getVal('a (cm)', 6.5) * 10,                         // a - 端部边距
            edgeE1: getVal('e1 (cm)', 8.0) * 10,                       // e1 - 侧墙边距
            edgeE2: getVal('e2 (cm)', 8.0) * 10,                       // e2 - 底板边距
            edgeE3: getVal('e3 (cm)', 6.25) * 10                       // e3 - 其他边距
        };
    }

    /**
     * 获取标准化参数
     * @returns {Object}
     */
    getParams() {
        return this.normalized;
    }

    /**
     * 获取单个参数值
     * @param {string} key - 参数名
     * @param {*} defaultVal - 默认值
     * @returns {*}
     */
    get(key, defaultVal = 0) {
        return this.normalized[key] !== undefined ? this.normalized[key] : defaultVal;
    }

    /**
     * 获取钢筋规格
     * @param {string} rebarId - 钢筋编号 (N1-N8)
     * @returns {Object} {diameter, spacing, grade}
     */
    getRebarSpec(rebarId) {
        const p = this.normalized;
        
        // 钢筋规格映射（根据设计规范）
        const specs = {
            N1: {
                diameter: 14,
                spacing: p.spacingD1,
                grade: 'HRB400',
                description: '侧墙外层竖向主筋'
            },
            N2: {
                diameter: 12,
                spacing: p.spacingD2,
                grade: 'HRB400',
                description: '侧墙内层竖向主筋'
            },
            N3: {
                diameter: 20,
                spacing: p.spacingD1Prime,
                grade: 'HRB400',
                description: '底板下层主筋'
            },
            N4: {
                diameter: 14,
                spacing: p.spacingD1Prime,
                grade: 'HRB400',
                description: '底板上层主筋'
            },
            N5: {
                diameter: 12,
                spacing: p.spacingB1,
                grade: 'HRB400',
                description: 'L1方向分布筋'
            },
            N6: {
                diameter: 12,
                spacing: p.spacingB2,
                grade: 'HRB400',
                description: 'L2方向分布筋'
            },
            N7: {
                diameter: 8,
                spacing: 250,
                grade: 'HPB300',
                description: '侧墙拉结筋'
            },
            N8: {
                diameter: 8,
                spacing: 250,
                grade: 'HPB300',
                description: '底板拉结筋'
            }
        };

        return specs[rebarId] || { diameter: 12, spacing: 200, grade: 'HRB400' };
    }

    /**
     * 获取所有钢筋规格列表
     * @returns {Array<Object>}
     */
    getAllRebarSpecs() {
        return ['N1', 'N2', 'N3', 'N4', 'N5', 'N6', 'N7', 'N8'].map(id => ({
            id,
            ...this.getRebarSpec(id)
        }));
    }

    /**
     * 格式化钢筋标注文本
     * @param {string} rebarId - 钢筋编号
     * @returns {string} 如 "N1 Φ14@128"
     */
    formatRebarCallout(rebarId) {
        const spec = this.getRebarSpec(rebarId);
        const symbol = spec.diameter >= 10 ? 'Φ' : 'φ';  // 大直径用大写
        return `${rebarId} ${symbol}${spec.diameter}@${spec.spacing.toFixed(0)}`;
    }

    /**
     * 获取所有钢筋标注文本
     * @returns {Array<string>}
     */
    getAllRebarCallouts() {
        return ['N1', 'N2', 'N3', 'N4', 'N5', 'N6', 'N7', 'N8'].map(id => 
            this.formatRebarCallout(id)
        );
    }

    /**
     * 计算横断面几何参数
     * @returns {Object}
     */
    getCrossSectionGeometry() {
        const p = this.normalized;
        const halfW = p.internalWidth / 2;
        const outerHalfW = halfW + p.wallThickness;
        const height = (p.heightMin + p.heightMax) / 2;  // 取平均高度
        const haunchWidth = 100;   // 腋角宽度 (mm)
        const haunchHeight = 100;  // 腋角高度 (mm)
        const cover = 40;          // 保护层厚度 (mm)

        return {
            halfWidth: halfW,
            outerHalfWidth: outerHalfW,
            height: height,
            bottomThickness: p.bottomThickness,
            wallThickness: p.wallThickness,
            haunchWidth: haunchWidth,
            haunchHeight: haunchHeight,
            cover: cover
        };
    }

    /**
     * 导出为 JSON 格式
     * @returns {Object}
     */
    toJSON() {
        return {
            yuType: this.yuType,
            raw: this.raw,
            normalized: this.normalized
        };
    }

    /**
     * 静态方法：检查参数表是否已加载
     * @returns {boolean}
     */
    static isTableLoaded() {
        return Array.isArray(window.UChannelTable1Data) && 
               window.UChannelTable1Data.length > 0;
    }

    /**
     * 静态方法：获取所有可用的 YU 类型
     * @returns {Array<string>}
     */
    static getAvailableTypes() {
        const data = window.UChannelTable1Data || [];
        return data.map(row => row[0]).filter(Boolean);
    }
}

// 导出为全局变量
if (typeof window !== 'undefined') {
    window.ParameterMapper = ParameterMapper;
}

// 支持 CommonJS 模块导出
if (typeof module !== 'undefined' && module.exports) {
    module.exports = ParameterMapper;
}
