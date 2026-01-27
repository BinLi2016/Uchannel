using System;

namespace UChannelDesignApp.Models
{
    public class DesignResult
    {
        public double MaxWallMoment { get; set; } // kNm/m
        public double MaxBottomMoment { get; set; } // kNm/m
        public double Ka { get; set; }
        public double WallRebarAreaRequired { get; set; } // cm2/m
        public double BottomRebarAreaRequired { get; set; } // cm2/m
        public bool AntiFloatingPass { get; set; }
        public double SafetyFactor { get; set; }
    }
}
