using System;

namespace UChannelDesignApp.Logic
{
    public static class DimensionLookup
    {
        /// <summary>
        /// Calculates wall/bottom thickness based on max height and width.
        /// Logic extracted from Excel: LOOKUP(MAX(H, W), {0, 3, 4, 5, 6.5, 7.5, 9, 10}, {0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9})
        /// Note: Inputs should be in meters for this specific function.
        /// </summary>
        public static double GetStandardThickness(double heightM, double widthM)
        {
            double maxDim = Math.Max(heightM, widthM);

            double[] thresholds = { 0.0, 3.0, 4.0, 5.0, 6.5, 7.5, 9.0, 10.0 };
            double[] thicknesses = { 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.9 }; // Extended last value

            for (int i = thresholds.Length - 1; i >= 0; i--)
            {
                if (maxDim >= thresholds[i])
                {
                    return thicknesses[i];
                }
            }

            return 0.3; // Default minimum
        }

        public static (double internalWidth, double internalHeight, double wallT, double floorT) GetPresetDimensions(string yuType)
        {
            return yuType.ToUpper() switch
            {
                // Values based on "U型槽参数表(一).png"
                // (D-2B, Hmax, B, T)
                "YU1" => (2225.0, 243.5, 30.0, 40.0),
                "YU2" => (2225.0, 295.1, 30.0, 40.0),
                "YU3" => (2225.0, 346.6, 40.0, 50.0),
                "YU4" => (2225.0, 398.2, 40.0, 50.0),
                "YU5" => (2225.0, 449.7, 50.0, 60.0),
                "YU6" => (2225.0, 527.6, 60.0, 70.0),
                "YU7" => (2225.0, 605.5, 60.0, 70.0),
                "YU8" => (2225.0, 665.6, 70.0, 80.0),
                "YU9" => (2225.0, 725.7, 70.0, 80.0),
                "YU10" => (2225.0, 725.7, 70.0, 80.0),
                "YU11" => (2225.0, 760.0, 80.0, 90.0),
                "YU12" => (2225.0, 697.0, 70.0, 80.0),
                "YU13" => (2225.0, 634.1, 60.0, 70.0),
                "YU14" => (2225.0, 571.1, 60.0, 70.0),
                "YU15" => (2225.0, 515.3, 60.0, 70.0),
                "YU16" => (2225.0, 459.5, 50.0, 60.0),
                "YU17" => (2225.0, 403.7, 50.0, 60.0),
                "YU18" => (2225.0, 347.9, 40.0, 50.0),
                "YU19" => (2225.0, 292.1, 30.0, 40.0),
                _ => (2225.0, 243.5, 30.0, 40.0)
            };
        }
    }
}
