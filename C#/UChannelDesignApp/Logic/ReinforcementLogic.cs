using System;
using System.Collections.Generic;
using UChannelDesignApp.Models;

namespace UChannelDesignApp.Logic
{
    public static class ReinforcementLogic
    {
        public static double GetBarArea(int diameterMm)
        {
            return Math.PI * Math.Pow(diameterMm / 20.0, 2); // cm2
        }

        public static int CalculateSpacing(double requiredAreaCm2, int diameterMm, double sectionThicknessCm)
        {
            // Minimum reinforcement ratio (0.2% - 0.6% depending on standard)
            // Using 0.002 as a conservative base for distribution or 0.004 for main.
            double minArea = 0.002 * sectionThicknessCm * 100.0;
            double effectiveArea = Math.Max(requiredAreaCm2, minArea);

            double barArea = GetBarArea(diameterMm);
            double spacingCm = (barArea / effectiveArea) * 100.0;

            // Standard spacings: 10, 12, 15, 20 cm
            if (spacingCm >= 20) return 20;
            if (spacingCm >= 15) return 15;
            if (spacingCm >= 12) return 12;
            return 10;
        }

        public static RebarSchedule GenerateRebarSchedule(UChannelParameters p, DesignResult result)
        {
            var schedule = new RebarSchedule();
            double cover = 4.0; // cm

            // N1/N2: Side Wall Main (one face)
            int spacingN1 = CalculateSpacing(result.WallRebarAreaRequired / 2, 16, p.WallThickness);
            schedule.Items.Add(new RebarItem
            {
                ID = "N1",
                Diameter = 16,
                Shape = RebarShape.Straight,
                SegmentA = p.InternalHeight + p.BottomThickness - cover,
                Quantity = (int)(100.0 / spacingN1)
            });

            // N3/N4: Bottom Plate Main
            int spacingN3 = CalculateSpacing(result.BottomRebarAreaRequired / 2, 16, p.BottomThickness);
            schedule.Items.Add(new RebarItem
            {
                ID = "N3",
                Diameter = 16,
                Shape = RebarShape.Straight,
                SegmentA = p.InternalWidth + 2 * p.WallThickness - 2 * cover,
                Quantity = (int)(100.0 / spacingN3)
            });

            // N5: Distribution Rebar
            schedule.Items.Add(new RebarItem
            {
                ID = "N5",
                Diameter = 12,
                Shape = RebarShape.Straight,
                SegmentA = 100.0,
                Quantity = (int)((p.InternalHeight * 2 + p.InternalWidth) / 20.0)
            });

            // N6: Stirrup
            schedule.Items.Add(new RebarItem
            {
                ID = "N6",
                Diameter = 10,
                Shape = RebarShape.ClosedLoop,
                SegmentA = 2 * (p.InternalWidth + 2 * p.WallThickness + p.BottomThickness) - 8 * cover,
                Quantity = 5
            });

            return schedule;
        }
    }
}
