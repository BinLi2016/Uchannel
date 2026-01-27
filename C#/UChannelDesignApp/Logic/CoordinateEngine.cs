using System;
using System.Collections.Generic;
using UChannelDesignApp.Models;

namespace UChannelDesignApp.Logic
{
    public class CoordinateEngine
    {
        public static CrossSectionGeometry CalculateCrossSection(UChannelParameters p)
        {
            var geometry = new CrossSectionGeometry();

            double halfW = p.InternalWidth / 2.0;
            double outerHalfW = halfW + p.WallThickness;

            // Outer Contour (Clockwise starting from Top Left)
            geometry.OuterContour.Add(new Point2D(-outerHalfW, p.InternalHeight));
            geometry.OuterContour.Add(new Point2D(outerHalfW, p.InternalHeight));
            geometry.OuterContour.Add(new Point2D(outerHalfW, -p.BottomThickness));
            geometry.OuterContour.Add(new Point2D(-outerHalfW, -p.BottomThickness));

            // Inner Contour (with Haunches)
            geometry.InnerContour.Add(new Point2D(-halfW, p.InternalHeight));
            geometry.InnerContour.Add(new Point2D(-halfW, p.HaunchHeight));
            geometry.InnerContour.Add(new Point2D(-halfW + p.HaunchWidth, 0));
            geometry.InnerContour.Add(new Point2D(halfW - p.HaunchWidth, 0));
            geometry.InnerContour.Add(new Point2D(halfW, p.HaunchHeight));
            geometry.InnerContour.Add(new Point2D(halfW, p.InternalHeight));

            // Dimensions
            // Total Width (D)
            geometry.Dimensions.Add(new DimensionData
            {
                Start = new Point2D(-outerHalfW, -p.BottomThickness - 50),
                End = new Point2D(outerHalfW, -p.BottomThickness - 50),
                Text = $"D = {p.InternalWidth + 2 * p.WallThickness:F1}",
                TextPosition = new Point2D(0, -p.BottomThickness - 65)
            });

            // Internal Width
            geometry.Dimensions.Add(new DimensionData
            {
                Start = new Point2D(-halfW, p.InternalHeight + 20),
                End = new Point2D(halfW, p.InternalHeight + 20),
                Text = $"{p.InternalWidth:F1}",
                TextPosition = new Point2D(0, p.InternalHeight + 35)
            });

            // Height
            geometry.Dimensions.Add(new DimensionData
            {
                Start = new Point2D(outerHalfW + 20, 0),
                End = new Point2D(outerHalfW + 20, p.InternalHeight),
                Text = $"H = {p.InternalHeight:F1}",
                TextPosition = new Point2D(outerHalfW + 35, p.InternalHeight / 2),
                Rotation = Math.PI / 2
            });

            // Rebar placement (Detailed)
            double cover = 4.0;

            // N1: Outer Vertical Bar
            var n1 = new RebarData { ID = "N1", Diameter = 1.4 };
            n1.Points.Add(new Point2D(-outerHalfW + cover, p.InternalHeight - cover));
            n1.Points.Add(new Point2D(-outerHalfW + cover, -p.BottomThickness + cover));
            n1.Points.Add(new Point2D(outerHalfW - cover, -p.BottomThickness + cover));
            n1.Points.Add(new Point2D(outerHalfW - cover, p.InternalHeight - cover));
            geometry.Rebars.Add(n1);

            // N2: Inner Vertical Bar
            var n2 = new RebarData { ID = "N2", Diameter = 1.2 };
            n2.Points.Add(new Point2D(-halfW - cover, p.InternalHeight - cover));
            n2.Points.Add(new Point2D(-halfW - cover, cover));
            n2.Points.Add(new Point2D(halfW + cover, cover));
            n2.Points.Add(new Point2D(halfW + cover, p.InternalHeight - cover));
            geometry.Rebars.Add(n2);

            // N3: Bottom plate main bar (schematic straight bar near bottom cover)
            var n3 = new RebarData { ID = "N3", Diameter = 1.2 };
            n3.Points.Add(new Point2D(-halfW + cover, -p.BottomThickness + cover));
            n3.Points.Add(new Point2D(halfW - cover, -p.BottomThickness + cover));
            geometry.Rebars.Add(n3);

            // N7: Links (Dots in section) - Let's just draw some dots as circles
            for (double y = 10; y < p.InternalHeight - 10; y += 20)
            {
                var linkLeft = new RebarData { ID = "N7", Diameter = 0.8 };
                linkLeft.Points.Add(new Point2D(-outerHalfW + p.WallThickness / 2.0, y));
                linkLeft.Points.Add(new Point2D(-outerHalfW + p.WallThickness / 2.0 + 1, y)); // Small segment to represent dot
                geometry.Rebars.Add(linkLeft);

                var linkRight = new RebarData { ID = "N7", Diameter = 0.8 };
                linkRight.Points.Add(new Point2D(outerHalfW - p.WallThickness / 2.0, y));
                linkRight.Points.Add(new Point2D(outerHalfW - p.WallThickness / 2.0 + 1, y));
                geometry.Rebars.Add(linkRight);
            }

            return geometry;
        }
    }
}
