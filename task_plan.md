# Task: Convert Sidewall.jpg to CAD Drawing

## Goal
Accurately represent the sidewall design from `DrawingShot\Sidewall.jpg` in a CAD drawing using the P-CAD transpiler to AutoLISP.

## Phases
- [x] Phase 1: Image Analysis and Parameter Extraction
- [ ] Phase 2: P-CAD Code Drafting
  - [ ] Implement concrete outline.
  - [ ] Implement $N2$ bars (parallel).
  - [ ] Implement $N1$ bars (fan).
- [ ] Phase 3: Transpilation to AutoLISP
- [ ] Phase 4: Verification and Refinement

## Decisions
- Origin: Bottom-left of the sidewall.
- Units: mm (P-CAD standard), inputs in cm.
- Rebar representation: 
  - N1: Fan distribution (need to figure out how to represent this in P-CAD or if the transpiler needs a new feature).
  - N2: Parallel distribution.
