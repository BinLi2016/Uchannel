using System;
using UChannelDesignApp.Models;

namespace UChannelDesignApp.Logic
{
    public class StructuralEngine
    {
        public static double CalculateCoulombKa(double phiDeg, double deltaDeg, double alphaDeg, double betaDeg)
        {
            double phi = phiDeg * Math.PI / 180.0;
            double delta = deltaDeg * Math.PI / 180.0;
            double alpha = alphaDeg * Math.PI / 180.0;
            double beta = betaDeg * Math.PI / 180.0;

            double numerator = Math.Pow(Math.Cos(phi - alpha), 2);
            double denominatorPart1 = Math.Pow(Math.Cos(alpha), 2) * Math.Cos(alpha + delta);
            double denominatorPart2 = 1.0 + Math.Sqrt(Math.Sin(phi + delta) * Math.Sin(phi - beta) / (Math.Cos(alpha + delta) * Math.Cos(alpha - beta)));

            return numerator / (denominatorPart1 * Math.Pow(denominatorPart2, 2));
        }

        public static DesignResult Verify(UChannelParameters p)
        {
            // 1. Earth Pressure Coefficient
            double Ka = CalculateCoulombKa(p.InternalAngle, p.WallFrictionAngle, 0, 0);

            double H = p.InternalHeightM;
            double B_total = (p.InternalWidth + 2 * p.WallThickness) / 100.0;

            // 2. Wall Moment Calculation (Cantilever)
            double earthPressureAtBottom = p.SoilDensity * H * Ka;
            double surchargePressure = p.SurchargeLoad * Ka;

            double M_earth = earthPressureAtBottom * Math.Pow(H, 2) / 6.0;
            double M_surcharge = surchargePressure * Math.Pow(H, 2) / 2.0;

            double wallMoment = p.ImportanceFactor * (p.EarthPressureFactor * M_earth + p.LiveLoadFactor * M_surcharge);

            // 3. Bottom Plate Moment (Simplified)
            double verticalLoadPerMeter = (p.WallThicknessM * H * 2 * 25.0) + (B_total * p.BottomThicknessM * 25.0) + (p.InternalWidthM * p.SurchargeLoad);
            double uniformReaction = verticalLoadPerMeter / B_total;

            double M_span = (uniformReaction * Math.Pow(p.InternalWidthM, 2)) / 8.0;
            double bottomMoment = Math.Max(wallMoment, Math.Abs(M_span - wallMoment));

            // 4. Required Rebar Area (RC formula: As = M / (fyd * 0.9 * d))
            // M in kNm/m = 100,000 Ncm/m
            // SteelStrength in MPa = 100 N/cm2
            // d in cm
            // As = (M * 100000) / (SteelStrength * 100 * 0.9 * d)
            // As = (M * 1000) / (SteelStrength * 0.9 * d)
            double d_wall = p.WallThickness - 4.5;
            double d_bottom = p.BottomThickness - 4.5;

            double As_wall = (wallMoment * 1000.0) / (p.SteelStrength * 0.9 * d_wall);
            double As_bottom = (bottomMoment * 1000.0) / (p.SteelStrength * 0.9 * d_bottom);

            // 5. Anti-floating Check
            double submergedDepth = Math.Max(0, (H + p.BottomThicknessM) - (p.GroundwaterDepth / 100.0));
            double buoyancy = submergedDepth * B_total * 10.0;
            double deadWeight = (p.WallThicknessM * H * 2 * 25.0) + (B_total * p.BottomThicknessM * 25.0);
            double safetyFactor = deadWeight / buoyancy;

            return new DesignResult
            {
                MaxWallMoment = wallMoment,
                MaxBottomMoment = bottomMoment,
                Ka = Ka,
                WallRebarAreaRequired = As_wall,
                BottomRebarAreaRequired = As_bottom,
                AntiFloatingPass = safetyFactor >= 1.05,
                SafetyFactor = safetyFactor
            };
        }
    }
}
