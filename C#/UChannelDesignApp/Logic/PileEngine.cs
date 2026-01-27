using System;
using System.Linq;
using UChannelDesignApp.Models;

namespace UChannelDesignApp.Logic
{
    public class PileEngine
    {
        public static double CalculateUpliftCapacity(PileParameters p)
        {
            double totalCapacity = 0;
            foreach (var layer in p.Layers)
            {
                double skinArea = p.Perimeter * layer.Thickness;
                totalCapacity += layer.UpliftCoefficient * layer.SkinFriction * skinArea;
            }
            return totalCapacity; // kN
        }

        public static PileVerificationResult VerifyUplift(UChannelParameters u, PileParameters p)
        {
            double B_total = (u.InternalWidth + 2 * u.WallThickness) / 100.0;
            double H_total = (u.InternalHeight + u.BottomThickness) / 100.0;

            // Buoyancy force per meter
            double submergedDepth = Math.Max(0, H_total - (u.GroundwaterDepth / 100.0));
            double buoyancyPerMeter = B_total * submergedDepth * 10.0;

            // Dead weight per meter
            double deadWeightPerMeter = (u.BottomThicknessM * B_total * 25.0) +
                                         (2 * (u.InternalHeightM) * u.WallThicknessM * 25.0);

            // Net uplift force per meter (Factored)
            double netUpliftForce = buoyancyPerMeter * 1.2 - deadWeightPerMeter * 0.9;

            // Pile resistance per meter
            double capacityPerPile = CalculateUpliftCapacity(p);
            double resistancePerMeter = (capacityPerPile / p.LongitudinalSpacing);

            return new PileVerificationResult
            {
                BuoyancyPerMeter = buoyancyPerMeter,
                DeadWeightPerMeter = deadWeightPerMeter,
                RequiredResistance = netUpliftForce,
                ProvidedResistance = resistancePerMeter,
                SafetyFactor = (deadWeightPerMeter + resistancePerMeter) / buoyancyPerMeter
            };
        }
    }

    public class PileVerificationResult
    {
        public double BuoyancyPerMeter { get; set; }
        public double DeadWeightPerMeter { get; set; }
        public double RequiredResistance { get; set; }
        public double ProvidedResistance { get; set; }
        public double SafetyFactor { get; set; }
        public bool IsSafe => SafetyFactor >= 1.05;
    }
}
