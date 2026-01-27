using System;

namespace UChannelDesignApp.Models
{
    public class UChannelParameters
    {
        // Geometric Parameters (cm)
        public double InternalWidth { get; set; } // D-2B
        public double InternalHeight { get; set; } // H
        public double WallThickness { get; set; } // B
        public double BottomThickness { get; set; } // T
        public double HaunchWidth { get; set; } = 10.0; // cm
        public double HaunchHeight { get; set; } = 10.0; // cm

        // Structural Parameters (m)
        public double WallThicknessM => WallThickness / 100.0;
        public double BottomThicknessM => BottomThickness / 100.0;
        public double InternalWidthM => InternalWidth / 100.0;
        public double InternalHeightM => InternalHeight / 100.0;

        // Soil & Load Properties
        public double SoilDensity { get; set; } = 18.0; // r1 (kN/m3)
        public double SaturatedSoilDensity { get; set; } = 20.0; // r2 (kN/m3)
        public double InternalAngle { get; set; } = 23.0; // φ (Degree)
        public double WallFrictionAngle { get; set; } = 11.5; // δ (Degree)
        public double SurchargeLoad { get; set; } = 10.0; // q (kN/m2)
        public double SubgradeModulus { get; set; } = 20000.0; // k (kN/m3)

        // Materials
        public double ConcreteStrength { get; set; } = 16.7; // fcd for C35 (MPa)
        public double SteelStrength { get; set; } = 360.0; // fyd for HRB400 (MPa)

        // Partial Factors (Structural Standards)
        public double ImportanceFactor { get; set; } = 1.05; // r0
        public double EarthPressureFactor { get; set; } = 1.4; // γt
        public double WaterPressureFactor { get; set; } = 1.0; // γs
        public double LiveLoadFactor { get; set; } = 1.4; // γQ

        // Water Level Information
        public double GroundwaterDepth { get; set; } = 100.0; // Relative to top (cm)
        public bool IsUnderwater => GroundwaterDepth < InternalHeight;
    }
}
