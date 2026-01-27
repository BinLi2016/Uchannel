// Anti-collision curb stone (防撞侧石) sample data.

window.CurbStoneNotes = [
  '1. 本图尺寸单位为cm。',
  '2. 混凝土保护层厚度未注明时为3.5cm。',
  '3. 钢筋间距如与钢管位置冲突可适当调整。',
];

window.CurbStoneQuantityHeaders = ['编号', '图式', '直径(mm)', '每根长(m)', '根数', '总长(m)', '单位重(kg/m)', '总重(kg)'];

window.CurbStoneQuantityTable = {
  title: '防撞侧石钢筋数量表',
  headers: window.CurbStoneQuantityHeaders,
  items: [
    {
      id: 'N1',
      diameterMm: 14,
      lengthPerBarM: 2.344,
      quantity: 8,
      unitWeightKgPerM: 1.208,
      totalLengthM: 18.75,
      totalWeightKg: 22.65,
      bendShape: {
        pointsMm: [
          { x: 0, y: 0 },
          { x: 63, y: 0 },
          { x: 63, y: 1239 },
          { x: 223, y: 1239 },
        ],
        segmentDimsMmText: ['6.3', '123.9', '16'],
      },
    },
    {
      id: 'N2',
      diameterMm: 14,
      lengthPerBarM: 1.465,
      quantity: 8,
      unitWeightKgPerM: 1.208,
      totalLengthM: 11.72,
      totalWeightKg: 14.16,
      bendShape: {
        pointsMm: [
          { x: 0, y: 0 },
          { x: 200, y: 0 },
          { x: 200, y: 360 },
          { x: 945, y: 360 },
          { x: 1105, y: 360 },
        ],
        segmentDimsMmText: ['20', '36.0', '74.5', '16'],
      },
    },
    {
      id: 'N3',
      diameterMm: 12,
      lengthPerBarM: 1.0,
      quantity: 17,
      unitWeightKgPerM: 0.888,
      totalLengthM: 17.0,
      totalWeightKg: 15.10,
      bendShape: {
        pointsMm: [
          { x: 0, y: 0 },
          { x: 1000, y: 0 },
        ],
        segmentDimsMmText: ['100'],
      },
    },
    {
      id: 'N4-1',
      diameterMm: 8,
      lengthPerBarM: 0.204,
      quantity: 4,
      unitWeightKgPerM: 0.395,
      totalLengthM: 0.82,
      totalWeightKg: 0.32,
      bendShape: {
        pointsMm: [
          { x: 0, y: 0 },
          { x: 60, y: 0 },
          { x: 60, y: 94 },
          { x: 110, y: 94 },
        ],
        segmentDimsMmText: ['6', '9.4', '5'],
      },
    },
    {
      id: 'N4',
      diameterMm: 8,
      lengthPerBarM: 0.274,
      quantity: 12,
      unitWeightKgPerM: 0.395,
      totalLengthM: 3.29,
      totalWeightKg: 1.30,
      bendShape: {
        pointsMm: [
          { x: 0, y: 0 },
          { x: 60, y: 0 },
          { x: 60, y: 164 },
          { x: 110, y: 164 },
        ],
        segmentDimsMmText: ['6', '16.4', '5'],
      },
    },
  ],
  concreteText: 'C35混凝土: 0.307m³',
  totalSteelText: 'HPB300: 1.62kg, HRB400: 51.9kg, 钢材: 9.99kg',
};

window.CurbStoneProjectInfo = {
  projectName: '市政工程',
  drawingTitle: '防撞侧石结构及钢筋布置图',
  drawingNumber: 'CURB-001',
  scale: '1:??',
  designer: '',
  checker: '',
  approver: '',
  date: '',
  company: '',
};
