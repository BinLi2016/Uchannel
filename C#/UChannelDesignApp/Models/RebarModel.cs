using System;
using System.Collections.Generic;
using System.Linq;

namespace UChannelDesignApp.Models
{
    public enum RebarShape { Straight, L_Shape, U_Shape, Stirrup, ClosedLoop }

    public class RebarItem
    {
        public string ID { get; set; } = string.Empty;
        public int Diameter { get; set; }
        public RebarShape Shape { get; set; }
        public double SegmentA { get; set; }
        public double SegmentB { get; set; }
        public double SegmentC { get; set; }
        public int Quantity { get; set; }

        public double TotalLength => (SegmentA + SegmentB + SegmentC) * Quantity;
        public double WeightPerMeter => (Diameter * Diameter) / 162.0;
        public double TotalWeight => (TotalLength / 100.0) * WeightPerMeter;

        public List<double> Segments => new List<double> { SegmentA, SegmentB, SegmentC }.Where(s => s > 0).ToList();
    }

    public class RebarSchedule
    {
        public List<RebarItem> Items { get; set; } = new List<RebarItem>();
        public double TotalConcreteVolume { get; set; }
    }
}
