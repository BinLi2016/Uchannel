using System;
using System.Linq;
using System.Collections.Generic;
using UChannelDesignApp.Models;
using UChannelDesignApp.Logic;

namespace UChannelDesignApp
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("========================================");
            Console.WriteLine("    U-Channel Design & Reinforcement    ");
            Console.WriteLine("========================================");

            // 1. Initial Parameters (Example: YU1 type)
            (double win, double hmax, double wallT, double floorT) = DimensionLookup.GetPresetDimensions("YU1");
            var uChannel = new UChannelParameters
            {
                InternalWidth = win,
                InternalHeight = hmax,
                WallThickness = wallT,
                BottomThickness = floorT,
                GroundwaterDepth = 150.0, // cm from top
            };

            Console.WriteLine("--- U-Channel Design Results ---");
            Console.WriteLine($"Preset: YU1");
            Console.WriteLine($"Dimensions: Inner-Width={uChannel.InternalWidth}cm, Inner-Height={uChannel.InternalHeight}cm");
            Console.WriteLine($"Thickness: Wall={uChannel.WallThickness}cm, Floor={uChannel.BottomThickness}cm");

            // 2. Structural Verification
            var designResult = StructuralEngine.Verify(uChannel);

            Console.WriteLine("\n--- Structural Verification ---");
            Console.WriteLine($"Earth Pressure Coeff (Ka): {designResult.Ka:F3}");
            Console.WriteLine($"Max Wall Moment: {designResult.MaxWallMoment:F2} kN.m/m");
            Console.WriteLine($"Max Bottom Moment: {designResult.MaxBottomMoment:F2} kN.m/m");
            Console.WriteLine($"Required Wall As: {designResult.WallRebarAreaRequired:F2} cm2/m");
            Console.WriteLine($"Anti-Floating: {(designResult.AntiFloatingPass ? "PASS" : "FAIL")} (SF={designResult.SafetyFactor:F2})");

            // 3. Reinforcement Schedule
            var schedule = ReinforcementLogic.GenerateRebarSchedule(uChannel, designResult);

            Console.WriteLine("\n--- Reinforcement Schedule (per meter) ---");
            Console.WriteLine("ID  | Dia | Shape   | Count | Weight(kg)");
            Console.WriteLine("-----------------------------------------");
            foreach (var item in schedule.Items)
            {
                Console.WriteLine($"{item.ID,-3} | {item.Diameter,-3} | {item.Shape,-7} | {item.Quantity,-5} | {item.TotalWeight:F2}");
            }

            Console.WriteLine($"\nTotal Concrete Volume: {schedule.TotalConcreteVolume:F3} m3/m");

            // 4. Pile Verification (Optional)
            if (!designResult.AntiFloatingPass)
            {
                var pileParams = new PileParameters
                {
                    Diameter = 60, // cm
                    Length = 12.0, // m
                    LongitudinalSpacing = 5.0, // m
                    Layers = new List<GeologicalLayer>
                    {
                        new GeologicalLayer { Name = "Clay", Thickness = 5.0, SkinFriction = 30.0, UpliftCoefficient = 0.7 },
                        new GeologicalLayer { Name = "Sand", Thickness = 7.0, SkinFriction = 50.0, UpliftCoefficient = 0.8 }
                    }
                };

                var pileResult = PileEngine.VerifyUplift(uChannel, pileParams);
                Console.WriteLine("\n--- Anti-Uplift Pile Verification ---");
                Console.WriteLine($"Provided Resistance: {pileResult.ProvidedResistance:F2} kN/m");
                Console.WriteLine($"Safety Factor with Piles: {pileResult.SafetyFactor:F2} ({(pileResult.IsSafe ? "SAFE" : "UNSAFE")})");
            }

            Console.WriteLine("========================================");
        }
    }
}
