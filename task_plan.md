# Implementation Plan: U-Channel Design Software Architecture

## Goal Description
Provide a professional architecture for a U-channel design software that integrates calculation logic (C#) with drawing generation (MXCAD).

## Selected Architecture: Option B - Desktop-Integrated Assistant (WPF) - **IMPLEMENTED**
**Classic engineering software feel with an embedded high-performance viewer.**

*   **Host**: [DONE] Modern WPF Application using `.NET 9.0`.
*   **Viewer Layer**: [DONE] Use `Microsoft.Web.WebView2` to host the MXCAD engine.
*   **Data Flow**:
    1.  **C# UI**: [DONE] Collects structural parameters (Hmax, B, T, etc.).
    2.  **Coordinate Engine**: [DONE] `CoordinateEngine` calculates precise 2D coordinates for all rebar and sections. Verified with xUnit.
    3.  **JS Bridge**: [DONE] C# serializes JSON and executes `window.drawUChannel(json)` in WebView2.
    4.  **MXCAD**: [DONE] `UChannelPainter.js` receives data and programmatically draws entities.

## TDD Strategy (Test-Driven Development) - **VERIFIED**
1.  **Red**: Create unit tests in `UChannelDesignApp.Tests` (xUnit).
2.  **Green**: Implement `CoordinateEngine` and `StructuralEngine`.
3.  **Refactor**: Verified against Coulomb earth pressure theory and basic geometric requirements.

---

## Completed Components

### [Calculation Engine]
- `CoordinateEngine.cs`: Outputs contour and rebar coordinates.
- `StructuralEngine.cs`: Performs stability and strength verification.
- `Logic/DimensionLookup.cs`: Handles YU1-YU19 presets.

### [UI & Bridge]
- `MainWindow.xaml`: WPF layout with sidebar controls.
- `MainWindow.xaml.cs`: Bridge logic using `SetVirtualHostNameToFolderMapping`.
- `MXCAD/UChannelPainter.js`: Drawing logic using MXCAD API.

## Verification Result
- **Automated Tests**: 4 tests passed (Coordinate math, Haunch logic, Rebar generation, Ka calculation).
- **Manual Build**: `dotnet build` successful for the WPF project.

---
**Status**: Ready for final user review and demonstration.
