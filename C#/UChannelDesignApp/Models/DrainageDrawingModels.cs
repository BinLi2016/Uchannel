using System;

namespace UChannelDesignApp.Models
{
    public sealed class DrainageChannelSectionParams
    {
        // Geometry is expressed in mm for drawing coordinates.
        public double OuterWidthMm { get; init; }
        public double OuterHeightMm { get; init; }
        public double InnerWidthMm { get; init; }
        public double InnerHeightMm { get; init; }
        public double WallThicknessMm { get; init; }
        public double BaseThicknessMm { get; init; }

        // Cover bearing notch at the top of wall.
        public double TopOuterLipWidthMm { get; init; }
        public double TopSeatWidthMm { get; init; }
        public double TopSeatDepthMm { get; init; }
    }

    public sealed class DrainageCoverPlateParams
    {
        public double WidthMm { get; init; }
        public double LengthMm { get; init; }
        public double ThicknessMm { get; init; }
        public double BearingMm { get; init; }
    }

    public sealed class DrainageRebarBendShape
    {
        // Simple polyline path in mm for the bending diagram.
        public required DrainagePoint2D[] PointsMm { get; init; }
        public required string[] SegmentDimsMmText { get; init; }
    }

    public sealed class DrainageRebarScheduleItem
    {
        public required string Id { get; init; }
        public required string DiameterText { get; init; }
        public double DiameterMm { get; init; }

        public double LengthPerBarM { get; init; }
        public int Quantity { get; init; }

        public double TotalLengthM => LengthPerBarM * Quantity;

        // Unit weight kg/m (rounded for table display).
        public double UnitWeightKgPerM { get; init; }
        public double TotalWeightKg => TotalLengthM * UnitWeightKgPerM;

        public required string ShapeKind { get; init; } // e.g. "U", "Bracket", "Straight"
        public DrainageRebarBendShape? BendShape { get; init; }
    }

    public sealed class DrainageRebarTable
    {
        public required string Title { get; init; }
        public required string[] Headers { get; init; }
        public required DrainageRebarScheduleItem[] Items { get; init; }
        public required string ConcreteText { get; init; }
        public required string TotalSteelText { get; init; }
    }

    public sealed class DrainageProjectInfo
    {
        public required string ProjectName { get; init; }
        public required string DrawingTitle { get; init; }
        public required string DrawingNumber { get; init; }
        public required string Scale { get; init; }
        public required string Designer { get; init; }
        public required string Checker { get; init; }
        public required string Approver { get; init; }
        public required string Date { get; init; }
        public required string Company { get; init; }
    }

    public readonly record struct DrainagePoint2D(double X, double Y);
}
