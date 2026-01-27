using System.Collections.Generic;

namespace UChannelDesignApp.Models
{
    public class GeologicalLayer
    {
        public string Name { get; set; }
        public double Thickness { get; set; } // m
        public double SkinFriction { get; set; } // qsik (kPa)
        public double UpliftCoefficient { get; set; } // lambda
    }

    public class PileParameters
    {
        public double Diameter { get; set; } // cm
        public double Length { get; set; } // m
        public double LongitudinalSpacing { get; set; } // m
        public double TransverseSpacing { get; set; } // m
        public double ConcreteDensity { get; set; } = 25.0; // kN/m3
        public List<GeologicalLayer> Layers { get; set; } = new List<GeologicalLayer>();

        public double Area => System.Math.PI * System.Math.Pow(Diameter / 200.0, 2);
        public double Perimeter => System.Math.PI * (Diameter / 100.0);
    }
}
