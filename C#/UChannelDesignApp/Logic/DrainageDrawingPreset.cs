using System;
using UChannelDesignApp.Models;

namespace UChannelDesignApp.Logic
{
    public static class DrainageDrawingPreset
    {
        // Weight formula: kg/m = 0.00617 * d^2 (d in mm)
        private static double UnitWeightKgPerM(double diameterMm)
        {
            return 0.00617 * diameterMm * diameterMm;
        }

        public static (DrainageChannelSectionParams Section, DrainageCoverPlateParams Cover, DrainageRebarTable ChannelTable, DrainageRebarTable CoverTable, DrainageProjectInfo ProjectInfo, string[] Notes) MotorLane()
        {
            // Reference: DrawingShot/机动车道排水沟边坡及盖板配筋图.png
            // Dimensions in mm.
            var section = new DrainageChannelSectionParams
            {
                OuterWidthMm = 600,
                OuterHeightMm = 600,
                InnerWidthMm = 400,
                InnerHeightMm = 450,
                WallThicknessMm = 100,
                BaseThicknessMm = 150,
                TopOuterLipWidthMm = 40,
                TopSeatWidthMm = 60,
                TopSeatDepthMm = 100,
            };

            var cover = new DrainageCoverPlateParams
            {
                WidthMm = 500,
                LengthMm = 490,
                ThicknessMm = 100,
                BearingMm = 50,
            };

            var channelItems = new[]
            {
                new DrainageRebarScheduleItem
                {
                    Id = "N1",
                    DiameterText = "Φ18",
                    DiameterMm = 18,
                    LengthPerBarM = 1.580,
                    Quantity = 10,
                    UnitWeightKgPerM = Math.Round(UnitWeightKgPerM(18), 3),
                    ShapeKind = "U",
                    BendShape = new DrainageRebarBendShape
                    {
                        PointsMm = new[]
                        {
                            new DrainagePoint2D(0, 0),
                            new DrainagePoint2D(0, 510),
                            new DrainagePoint2D(560, 510),
                            new DrainagePoint2D(560, 0),
                        },
                        SegmentDimsMmText = new[] { "510", "560", "510" },
                    },
                },
                new DrainageRebarScheduleItem
                {
                    Id = "N2",
                    DiameterText = "Φ12",
                    DiameterMm = 12,
                    LengthPerBarM = 0.650,
                    Quantity = 20,
                    UnitWeightKgPerM = Math.Round(UnitWeightKgPerM(12), 3),
                    ShapeKind = "Bracket",
                    BendShape = new DrainageRebarBendShape
                    {
                        PointsMm = new[]
                        {
                            new DrainagePoint2D(0, 0),
                            new DrainagePoint2D(140, 0),
                            new DrainagePoint2D(140, 410),
                            new DrainagePoint2D(280, 410),
                        },
                        SegmentDimsMmText = new[] { "140", "410", "140" },
                    },
                },
                new DrainageRebarScheduleItem
                {
                    Id = "N3",
                    DiameterText = "Φ12",
                    DiameterMm = 12,
                    LengthPerBarM = 1.000,
                    Quantity = 12,
                    UnitWeightKgPerM = Math.Round(UnitWeightKgPerM(12), 3),
                    ShapeKind = "Straight",
                    BendShape = new DrainageRebarBendShape
                    {
                        PointsMm = new[]
                        {
                            new DrainagePoint2D(0, 0),
                            new DrainagePoint2D(1000, 0),
                        },
                        SegmentDimsMmText = new[] { "1000" },
                    },
                },
            };

            var coverItems = new[]
            {
                new DrainageRebarScheduleItem
                {
                    Id = "M1",
                    DiameterText = "Φ16",
                    DiameterMm = 16,
                    LengthPerBarM = 0.460,
                    Quantity = 14,
                    UnitWeightKgPerM = Math.Round(UnitWeightKgPerM(16), 3),
                    ShapeKind = "Straight",
                    BendShape = new DrainageRebarBendShape
                    {
                        PointsMm = new[] { new DrainagePoint2D(0, 0), new DrainagePoint2D(460, 0) },
                        SegmentDimsMmText = new[] { "460" },
                    },
                },
                new DrainageRebarScheduleItem
                {
                    Id = "M2",
                    DiameterText = "Φ16",
                    DiameterMm = 16,
                    LengthPerBarM = 0.440,
                    Quantity = 10,
                    UnitWeightKgPerM = Math.Round(UnitWeightKgPerM(16), 3),
                    ShapeKind = "Straight",
                    BendShape = new DrainageRebarBendShape
                    {
                        PointsMm = new[] { new DrainagePoint2D(0, 0), new DrainagePoint2D(440, 0) },
                        SegmentDimsMmText = new[] { "440" },
                    },
                },
                new DrainageRebarScheduleItem
                {
                    Id = "M3",
                    DiameterText = "φ8",
                    DiameterMm = 8,
                    LengthPerBarM = 0.080,
                    Quantity = 12,
                    UnitWeightKgPerM = Math.Round(UnitWeightKgPerM(8), 3),
                    ShapeKind = "Straight",
                    BendShape = new DrainageRebarBendShape
                    {
                        PointsMm = new[] { new DrainagePoint2D(0, 0), new DrainagePoint2D(80, 0) },
                        SegmentDimsMmText = new[] { "80" },
                    },
                },
            };

            var channelTable = new DrainageRebarTable
            {
                Title = "水沟钢筋数量表",
                Headers = new[] { "编号", "图式", "直径(mm)", "每根长(m)", "根数", "总长(m)", "单位重(kg/m)", "总重(kg)" },
                Items = channelItems,
                ConcreteText = "C40混凝土: 0.13m³/m",
                TotalSteelText = "钢筋总重: 53.77kg",
            };

            var coverTable = new DrainageRebarTable
            {
                Title = "水沟盖板钢筋数量表",
                Headers = new[] { "编号", "图式", "直径(mm)", "每根长(m)", "根数", "总长(m)", "单位重(kg/m)", "总重(kg)" },
                Items = coverItems,
                ConcreteText = "C40混凝土: 0.049m³/块",
                TotalSteelText = "钢筋总重: 17.48kg",
            };

            var projectInfo = new DrainageProjectInfo
            {
                ProjectName = "市政工程",
                DrawingTitle = "机动车道排水沟边坡及盖板配筋图",
                DrawingNumber = "PS-DRAIN-001",
                Scale = "1:10",
                Designer = "",
                Checker = "",
                Approver = "",
                Date = DateTime.Now.ToString("yyyy.MM"),
                Company = "",
            };

            var notes = new[]
            {
                "1. 钢筋直径及详图单位为mm, 其余尺寸单位为cm。",
                "2. 水流面涂聚氨酯防水涂料。",
                "3. φ为HPB300钢筋, Φ为HRB400钢筋。",
                "4. 水沟及盖板混凝土均采用C40。",
                "5. 纵向钢筋间距≤20cm, 箍筋竖向间距≤40cm(未注明时)。",
                "6. 侧墙伸缩缝处设3cm厚沥青木板。",
                "7. 适用于除ZU9-2、ZU10-2、YU9、YU10以外的机动车道U形排水沟。",
            };

            return (section, cover, channelTable, coverTable, projectInfo, notes);
        }
    }
}
