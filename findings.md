# Findings: Sidewall.jpg Analysis

## Image Details
- **File**: `DrawingShot\Sidewall.jpg`
- **Subject**: Trapezoidal sidewall with rebar.

## Geometry
- **Length (L)**: 400 cm (4000 mm)
- **Right Height (h2)**: 120 cm (1200 mm)
- **Left Height (h1)**: Not explicitly numbered. Visually appears to be around 160 cm. I will define it as a parameter with a default value of 160 cm.
- **Cover**: 50 mm.

## Rebar Details
- **N1**: "10 N1-1~10 扇形分布" (10 bars, N1-1 to N1-10, fan distribution).
  - Implementation: Linear interpolation between left and right heights.
- **N2**: "N2 @ (L-Cover)/20 均匀平行分布" (21 bars if 20 intervals, uniform parallel distribution).
  - Implementation: Vertical bars distributed horizontally.

## Implementation Approach
- Use `sketch` for the concrete outline.
- Use `sketch` for $N1$ bars (manual generation or transpiler update).
- Use `bars` for $N2$ bars.
