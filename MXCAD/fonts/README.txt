Place required CAD fonts (SHX/TTF) in this folder so MXCAD can render text correctly.

Notes
- MXCAD searches `fontspath` for font files referenced by the drawing (e.g., simplex.shx, romans.shx, txt.shx, gdt.shx, iso.shx, etc.).
- Copy the SHX files used by your DWG/DXF (or by the .mxweb you are loading) into this folder.
- If a referenced font is missing, you can provide a fallback by copying an available SHX (e.g., txt.shx) and renaming the copy to the missing font’s filename. Rendering will not be identical but avoids empty glyphs.

Licensing
- Many SHX fonts from AutoCAD or other CAD systems are licensed; do not redistribute them. Use your own licensed fonts here.

Examples
- ./fonts/simplex.shx
- ./fonts/romans.shx
- ./fonts/txt.shx
- ./fonts/gdt.shx

