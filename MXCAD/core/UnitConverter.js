/**
 * UnitConverter.js - 单位转换工具模块
 * 用于统一处理 cm/mm/m 之间的单位转换
 */

const UnitConverter = {
    // 转换因子常量
    CM_TO_MM: 10,
    M_TO_MM: 1000,
    MM_TO_CM: 0.1,
    MM_TO_M: 0.001,

    /**
     * 厘米转毫米
     * @param {number} cm - 厘米值
     * @returns {number} 毫米值
     */
    cmToMm(cm) {
        return cm * this.CM_TO_MM;
    },

    /**
     * 毫米转厘米
     * @param {number} mm - 毫米值
     * @returns {number} 厘米值
     */
    mmToCm(mm) {
        return mm * this.MM_TO_CM;
    },

    /**
     * 米转毫米
     * @param {number} m - 米值
     * @returns {number} 毫米值
     */
    mToMm(m) {
        return m * this.M_TO_MM;
    },

    /**
     * 毫米转米
     * @param {number} mm - 毫米值
     * @returns {number} 米值
     */
    mmToM(mm) {
        return mm * this.MM_TO_M;
    },

    /**
     * 根据参数表头判断单位并转换为毫米
     * 参数表中的单位规则：
     * - D (cm) 实际数据为 mm，不需要转换
     * - 其他 (cm) 标记的参数需要 *10 转换
     * @param {string} header - 参数表头
     * @param {number} value - 原始值
     * @returns {number} 毫米值
     */
    normalizeToMm(header, value) {
        if (typeof value !== 'number' || isNaN(value)) {
            return 0;
        }
        
        // D (cm) 和 L1/L2 (cm) 数据实际已经是 mm
        if (header === 'D (cm)' || header === 'L1 (cm)' || header === 'L2 (cm)') {
            return value;
        }
        
        // 其他带 (cm) 的参数需要转换
        if (header.includes('(cm)')) {
            return this.cmToMm(value);
        }
        
        // 无单位的参数（如 n, m 等计数）直接返回
        return value;
    },

    /**
     * 批量转换参数对象中的值为毫米
     * @param {Object} rawParams - 原始参数对象 {header: value}
     * @returns {Object} 转换后的参数对象
     */
    normalizeParams(rawParams) {
        const result = {};
        for (const [header, value] of Object.entries(rawParams)) {
            result[header] = this.normalizeToMm(header, value);
        }
        return result;
    },

    /**
     * 格式化标注文本中的尺寸值
     * @param {number} valueMm - 毫米值
     * @param {string} unit - 目标单位 ('mm' | 'cm' | 'm')
     * @param {number} decimals - 小数位数
     * @returns {string} 格式化后的文本
     */
    formatDimension(valueMm, unit = 'mm', decimals = 1) {
        let converted;
        switch (unit) {
            case 'cm':
                converted = this.mmToCm(valueMm);
                break;
            case 'm':
                converted = this.mmToM(valueMm);
                break;
            default:
                converted = valueMm;
        }
        return converted.toFixed(decimals);
    }
};

// 导出为全局变量（兼容浏览器环境）
if (typeof window !== 'undefined') {
    window.UnitConverter = UnitConverter;
}

// 支持 CommonJS 模块导出
if (typeof module !== 'undefined' && module.exports) {
    module.exports = UnitConverter;
}
