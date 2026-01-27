/**
 * Drainage Channel Data Structure
 * Parameters for 泵站非机动车道排水边沟及盖板配筋图
 */

// Geometry parameters (all dimensions in mm)
window.DrainageChannelParams = {
    // Channel cross-section
    internalWidth: 600,           // Internal width
    internalHeight: 400,          // Internal height
    wallThickness: 80,            // Side wall thickness
    baseThickness: 100,           // Bottom slab thickness

    // Cover plate
    coverPlateWidth: 800,         // Total cover plate width
    coverPlateThickness: 80,      // Cover plate thickness
    coverPlateLength: 1000,       // Cover plate length (longitudinal)

    // Drainage openings in cover plate
    drainageOpenings: [
        { x: 200, y: 250, width: 120, height: 80 },
        { x: 400, y: 250, width: 120, height: 80 },
        { x: 600, y: 250, width: 120, height: 80 }
    ],

    // Concrete cover
    concreteCover: 25,            // Protective layer thickness

    // Slope (if any)
    sideSlope: 0,                 // Side slope ratio (0 = vertical walls)
};

// Rebar specifications
window.DrainageChannelRebar = {
    // Main reinforcement in walls
    wallMainRebar: {
        diameter: 12,             // mm
        spacing: 150,             // mm
        grade: "HRB400",
        mark: "①"
    },

    // Distribution bars in walls
    wallDistribution: {
        diameter: 10,
        spacing: 200,
        grade: "HRB400",
        mark: "②"
    },

    // Base slab main reinforcement
    baseMainRebar: {
        diameter: 14,
        spacing: 150,
        grade: "HRB400",
        mark: "③"
    },

    // Base slab distribution bars
    baseDistribution: {
        diameter: 10,
        spacing: 200,
        grade: "HRB400",
        mark: "④"
    },

    // Cover plate main reinforcement
    coverPlateMain: {
        diameter: 12,
        spacing: 150,
        grade: "HRB400",
        mark: "⑤"
    },

    // Cover plate distribution bars
    coverPlateDistribution: {
        diameter: 10,
        spacing: 200,
        grade: "HRB400",
        mark: "⑥"
    },

    // Stirrups/ties
    stirrups: {
        diameter: 8,
        spacing: 200,
        grade: "HPB300",
        mark: "⑦"
    }
};

// Dimension table data
window.DrainageChannelDimTable = {
    title: "主要尺寸表",
    headers: ["序号", "部位", "尺寸(mm)", "备注"],
    rows: [
        ["1", "渠道内宽", "600", ""],
        ["2", "渠道内高", "400", ""],
        ["3", "侧墙厚度", "80", ""],
        ["4", "底板厚度", "100", ""],
        ["5", "盖板厚度", "80", ""],
        ["6", "盖板宽度", "800", ""],
        ["7", "排水孔尺寸", "120×80", "3处"],
        ["8", "保护层厚度", "25", ""]
    ]
};

// Rebar schedule data
window.DrainageChannelRebarSchedule = {
    title: "钢筋数量表",
    headers: ["编号", "直径\n(mm)", "间距\n(mm)", "单根长度\n(mm)", "数量\n(根/m)", "单位重量\n(kg/m)"],
    rows: [
        ["①", "12", "150", "850", "7", "5.3"],
        ["②", "10", "200", "600", "5", "3.1"],
        ["③", "14", "150", "900", "7", "7.4"],
        ["④", "10", "200", "700", "5", "3.5"],
        ["⑤", "12", "150", "1100", "7", "6.6"],
        ["⑥", "10", "200", "900", "5", "4.5"],
        ["⑦", "8", "200", "1800", "5", "4.5"]
    ],
    summary: {
        concrete: "0.45m³/m",
        hpb300: "4.5kg/m",
        hrb400: "30.4kg/m"
    }
};

// Project information for title block
window.DrainageChannelProjectInfo = {
    projectName: "泵站工程",
    drawingTitle: "非机动车道排水边沟及盖板配筋图",
    drawingNumber: "SZ-PS-001",
    scale: "1:20",
    designer: "张工",
    checker: "李工",
    approver: "王工",
    date: "2026.01",
    company: "市政设计院"
};

// Construction notes and specifications
window.DrainageChannelNotes = [
    "1. 混凝土强度等级：C35",
    "2. 钢筋保护层厚度：25mm",
    "3. 钢筋连接采用绑扎搭接，搭接长度按规范要求",
    "4. 盖板排水孔间距均匀布置",
    "5. 施工时应确保钢筋位置准确，保护层厚度符合要求",
    "6. 混凝土浇筑应分层振捣密实"
];
