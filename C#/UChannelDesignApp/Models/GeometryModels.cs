using System;
using System.Collections.Generic;

namespace UChannelDesignApp.Models
{
    public struct Point2D
    {
        public double X { get; set; }
        public double Y { get; set; }

        public Point2D(double x, double y)
        {
            X = x;
            Y = y;
        }
    }

    public class RebarData
    {
        public string ID { get; set; } = string.Empty;
        public List<Point2D> Points { get; set; } = new List<Point2D>();
        public double Diameter { get; set; }
    }

    public class DimensionData
    {
        public Point2D Start { get; set; }
        public Point2D End { get; set; }
        public Point2D TextPosition { get; set; }
        public string Text { get; set; } = string.Empty;
        public double Rotation { get; set; } // Radians
    }

    public class CrossSectionGeometry
    {
        public List<Point2D> OuterContour { get; set; } = new List<Point2D>();
        public List<Point2D> InnerContour { get; set; } = new List<Point2D>();
        public List<RebarData> Rebars { get; set; } = new List<RebarData>();
        public List<DimensionData> Dimensions { get; set; } = new List<DimensionData>();
    }
}
