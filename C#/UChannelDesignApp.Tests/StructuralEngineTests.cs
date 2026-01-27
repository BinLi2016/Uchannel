using Xunit;
using UChannelDesignApp.Logic;
using UChannelDesignApp.Models;

namespace UChannelDesignApp.Tests
{
    public class StructuralEngineTests
    {
        [Fact]
        public void CalculateCoulombKa_StandardValues_ReturnsExpected()
        {
            // Range phi=23, delta=11.5, alpha=0, beta=0
            double Ka = StructuralEngine.CalculateCoulombKa(23.0, 11.5, 0, 0);

            // Expected roughly 0.3973
            Assert.Equal(0.3973, Ka, 4);
        }

        [Fact]
        public void Verify_YU1_ReturnsReasonableValues()
        {
            var p = new UChannelParameters
            {
                InternalWidth = 2225.0,
                InternalHeight = 243.5,
                WallThickness = 30.0,
                BottomThickness = 40.0,
                SoilDensity = 18.0,
                SurchargeLoad = 10.0,
                InternalAngle = 23.0,
                WallFrictionAngle = 11.5
            };

            var result = StructuralEngine.Verify(p);

            // Expected roughly 42.61
            Assert.Equal(42.61, result.MaxWallMoment, 2);
        }
    }
}
