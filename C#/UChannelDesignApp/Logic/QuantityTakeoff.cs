using System.Collections.Generic;
using UChannelDesignApp.Models;

namespace UChannelDesignApp.Logic
{
    public class QuantityTakeoff
    {
        public static RebarSchedule GenerateSchedule(UChannelParameters p, DesignResult dr)
        {
            var schedule = new RebarSchedule();

            // Assume 1 meter length for calculation
            double unitLength = 100.0; // cm

            // N1: Side Wall Bar (L-Shape)
            // Logic: n1 = ROUND((Hmax - 5) / 12.5, 0) - 1
            // Here Hmax is p.InternalHeight (simplified)
            double spacing = 12.5; // d
            int n1 = (int)Math.Round((p.InternalHeight - 5.0) / spacing) - 1;
            if (n1 < 0) n1 = 0;

            schedule.Items.Add(new RebarItem
            {
                ID = "N1",
                Diameter = 12,
                Shape = RebarShape.L_Shape,
                SegmentA = p.InternalHeight,
                SegmentB = p.WallThickness,
                Quantity = n1 * 2 // Both sides
            });

            // N2: Main Bottom Bar (Straight)
            schedule.Items.Add(new RebarItem
            {
                ID = "N2",
                Diameter = 14,
                Shape = RebarShape.Straight,
                SegmentA = p.InternalWidth + 2 * p.WallThickness,
                Quantity = (int)(unitLength / 15.0) + 1
            });

            // N3: Longitudinal Distribution Bars (Running through the length)
            // Area-based or distance-based? 
            // In Excel table, L1 often corresponds to section length.
            schedule.Items.Add(new RebarItem
            {
                ID = "N3",
                Diameter = 10,
                Shape = RebarShape.Straight,
                SegmentA = unitLength, // Per meter
                Quantity = 12 // Simplified assumption for demo
            });

            // N4: Pile Main Bars (Vertical)
            schedule.Items.Add(new RebarItem
            {
                ID = "N4",
                Diameter = 16,
                Shape = RebarShape.Straight,
                SegmentA = 1200.0, // Pile length (12m)
                Quantity = 8 // 8 longitudinal bars per pile
            });

            // N5: Pile Stirrups (Circular)
            schedule.Items.Add(new RebarItem
            {
                ID = "N5",
                Diameter = 8,
                Shape = RebarShape.Stirrup,
                SegmentA = 188.5, // Perimeter for 60cm diameter (pi*D)
                Quantity = 60 // 12m / 20cm spacing
            });

            // Concrete Volume (m3 per meter section)
            // Including Pile volume (area * length / longitudinal_spacing)
            double sectionArea = ((p.InternalWidth + 2 * p.WallThickness) * p.BottomThickness +
                                  (2 * p.InternalHeight * p.WallThickness)) / 10000.0;
            double pileVolumePerMeter = (System.Math.PI * System.Math.Pow(30.0, 2) * 1200.0 / 5.0) / 1000000.0; // cm3 to m3

            schedule.TotalConcreteVolume = sectionArea + pileVolumePerMeter;

            return schedule;
        }
    }
}
