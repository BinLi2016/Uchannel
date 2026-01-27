using Xunit;
using UChannelDesignApp.Logic;
using UChannelDesignApp.Models;
using System.Linq;

namespace UChannelDesignApp.Tests
{
    public class CoordinateEngineTests
    {
        [Fact]
        public void CalculateCrossSection_YU1_ReturnsCorrectOuterDimensions()
        {
            // Arrange
            var parameters = new UChannelParameters
            {
                InternalWidth = 2225.0,
                InternalHeight = 243.5,
                WallThickness = 30.0,
                BottomThickness = 40.0
            };

            // Act
            var geometry = CoordinateEngine.CalculateCrossSection(parameters);

            // Assert
            // Outer width should be InternalWidth + 2 * WallThickness = 2225 + 60 = 2285
            // Outer height should be InternalHeight + BottomThickness = 243.5 + 40 = 283.5

            var minX = geometry.OuterContour.Min(p => p.X);
            var maxX = geometry.OuterContour.Max(p => p.X);
            var minY = geometry.OuterContour.Min(p => p.Y);
            var maxY = geometry.OuterContour.Max(p => p.Y);

            Assert.Equal(-1142.5, minX);
            Assert.Equal(1142.5, maxX);
            Assert.Equal(-40.0, minY);
            Assert.Equal(243.5, maxY);

            // Check inner points (we expect 6 points for inner contour now)
            Assert.Equal(6, geometry.InnerContour.Count);

            // Left haunch start
            Assert.Contains(geometry.InnerContour, p => p.X == -1112.5 && p.Y == 10.0);
            // Left haunch end
            Assert.Contains(geometry.InnerContour, p => p.X == -1102.5 && p.Y == 0.0);
        }

        [Fact]
        public void CalculateCrossSection_ReturnsRebars()
        {
            var p = new UChannelParameters
            {
                InternalWidth = 200,
                InternalHeight = 200,
                WallThickness = 30,
                BottomThickness = 30
            };

            var geometry = CoordinateEngine.CalculateCrossSection(p);

            Assert.NotEmpty(geometry.Rebars);
            Assert.Contains(geometry.Rebars, r => r.ID == "N1");
            Assert.Contains(geometry.Rebars, r => r.ID == "N3");
        }
    }
}
