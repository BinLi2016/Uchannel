#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
P-CAD to AutoLISP Transpiler
使用 Approach 4: 全局变量参数注入
参数通过全局变量设置，可在调用渲染函数前修改
"""

import re
import sys
import argparse
from pathlib import Path
from typing import Dict, List, Tuple, Optional, Any

# =============================================================================
# GLOBAL CONFIGURATION - Modify these values for easy customization
# =============================================================================

# Text Style Settings
TEXT_FONT = "fsdb_e.shx,cadhzf.shx"  # SHX font with big font
TEXT_WIDTH_FACTOR = 0.70              # Width factor for text

# Text Heights (in drawing units, typically mm)
LABEL_TEXT_HEIGHT = 10                # Height for label text
TABLE_CELL_TEXT_HEIGHT = 10           # Height for table cell text
DIM_TEXT_HEIGHT = 10                  # Height for dimension text (DIMTXT)
DEFAULT_TEXT_SIZE = 10                # Default text size (TEXTSIZE)

# Barshape Layout Text Heights
LAYOUT_TITLE_TEXT_HEIGHT = 10         # Height for barshape layout titles
LAYOUT_LABEL_TEXT_HEIGHT = 10         # Height for barshape layout labels
LAYOUT_NOTE_TEXT_HEIGHT = 10          # Height for barshape layout notes
ANNOTATION_TEXT_HEIGHT = 10           # Height for custom annotations

# Dimension Settings
DIM_SCALE = 25.0                      # Dimension scale (DIMSCALE)

# Hatch Settings
HATCH_SCALE = 25.0                    # Default hatch pattern scale

# =============================================================================


class PCADParser:
    """简化的 P-CAD 解析器"""
    
    def __init__(self, source: str):
        self.source = source
        self.lines = source.split('\n')
        self.parser_units = 'mm'
        self.params = {}
        self.derive = {}
        self.layers = {}
        self.sketches = []
        self.regions = []
        self.dimensions = []
        self.hatch_styles = {}
        self.materials = {}
        self.rebar_sets = {}
        self.barshapes = {}
        self.tables = {}
        self.sheets = {}
        
        # P-CAD DSL v1.0 new constructs
        self.origin = (0, 0)
        self.drawing_info = {}
        self.views = {}
        self.notes = []
        self.meshes = []
        self.bars_list = []
        self.labels = []
        self.callouts = []
        self.section_markers = []
        self.components = {}
        self.barshape_layouts = {}  # P-CAD v1.1: freeform barshape placement
        
    def parse(self):
        """解析 P-CAD 文件"""
        i = 0
        while i < len(self.lines):
            line = self.lines[i].strip()
            line = re.sub(r'//.*$', '', line).strip()
            if not line:
                i += 1
                continue
            
            if line.startswith('units'):
                match = re.search(r'units\s+(\w+);', line)
                if match:
                    self.units = match.group(1)
            
            elif line.startswith('params'):
                i = self._parse_params(i)
                continue
            
            elif line.startswith('derive'):
                i = self._parse_derive(i)
                continue
            
            elif line.startswith('layers'):
                i = self._parse_layers(i)
                continue
            
            elif line.startswith('hatch_style'):
                i = self._parse_hatch_style(i)
                continue
            
            elif line.startswith('sketch'):
                i = self._parse_sketch(i)
                continue
            
            elif line.startswith('region'):
                i = self._parse_region(i)
                continue
            
            elif line.startswith('dim'):
                i = self._parse_dimension(i)
                continue
            
            elif line.startswith('materials'):
                i = self._parse_materials(i)
                continue
                
            elif line.startswith('rebar_set'):
                i = self._parse_rebar_set(i)
                continue
            
            # IMPORTANT: Check barshape_layout BEFORE barshape (both start with 'barshape')
            elif line.startswith('barshape_layout'):
                i = self._parse_barshape_layout(i)
                continue
                
            elif line.startswith('barshape'):
                i = self._parse_barshape(i)
                continue
                
            elif line.startswith('table'):
                i = self._parse_table(i)
                continue
                
            elif line.startswith('sheet'):
                i = self._parse_sheet(i)
                continue
            
            # P-CAD DSL v1.0 new constructs
            elif line.startswith('origin'):
                i = self._parse_origin(i)
                continue
            
            elif line.startswith('drawing_info'):
                i = self._parse_drawing_info(i)
                continue
            
            elif line.startswith('view '):
                i = self._parse_view(i)
                continue
            
            elif line.startswith('notes '):
                i = self._parse_notes(i)
                continue
            
            elif line.startswith('mesh '):
                i = self._parse_mesh(i)
                continue
            
            elif line.startswith('bars '):
                i = self._parse_bars(i)
                continue
            
            elif line.startswith('label '):
                i = self._parse_label(i)
                continue
            
            elif line.startswith('callout '):
                i = self._parse_callout(i)
                continue
            
            i += 1
    
    def _parse_params(self, start_idx: int) -> int:
        """解析 params 块"""
        i = start_idx
        i += 1  # Skip 'params {'
        while i < len(self.lines):
            line = re.sub(r'//.*$', '', self.lines[i]).strip()
            if not line:
                i += 1
                continue
            if line == '}':
                break
            match = re.search(r'(\w+)\s*=\s*([^;]+);', line)
            if match:
                param_name = match.group(1)
                try:
                    param_value = float(match.group(2))
                except:
                    param_value = match.group(2) # Keep as string if not float
                self.params[param_name] = {'value': param_value}
                print(f"DEBUG: Found param: {param_name} = {param_value}")
            i += 1
        return i
    
    def _parse_derive(self, start_idx: int) -> int:
        """解析 derive 块"""
        i = start_idx
        i += 1  # Skip 'derive {'
        while i < len(self.lines):
            line = re.sub(r'//.*$', '', self.lines[i]).strip()
            if not line:
                i += 1
                continue
            if line == '}':
                break
            match = re.search(r'(\w+)\s*=\s*(.+);', line)
            if match:
                var_name = match.group(1)
                expr = match.group(2).strip()
                self.derive[var_name] = expr
            i += 1
        return i
    
    def _parse_layers(self, start_idx: int) -> int:
        """解析 layers 块 (支持 color() 和 #hex 语法)"""
        i = start_idx
        i += 1  # Skip 'layers {'
        while i < len(self.lines):
            line = re.sub(r'//.*$', '', self.lines[i]).strip()
            if not line:
                i += 1
                continue
            if line == '}':
                break
            
            # Try Approach 1: name: #hex, weight; (Found in RebarDetails_2.pcad)
            hex_match = re.search(r'(\w+)[:=]\s*#([0-9a-fA-F]{6})[,\s]+([\d.]+)', line)
            if hex_match:
                layer_name = hex_match.group(1)
                hex_color = hex_match.group(2)
                r = int(hex_color[0:2], 16)
                g = int(hex_color[2:4], 16)
                b = int(hex_color[4:6], 16)
                lw = float(hex_match.group(3))
                self.layers[layer_name] = {'color': (r, g, b), 'lineweight': lw}
                print(f"DEBUG: Found layer (hex): {layer_name} = RGB({r},{g},{b}) LW={lw}")
                i += 1
                continue

            # Try Approach 2: name: color(r,g,b) lineweight(lw);
            rgb_match = re.search(r'(\w+)[:=]\s*color\((\d+),\s*(\d+),\s*(\d+)\)\s*lineweight\(([\d.]+)\)', line)
            if rgb_match:
                layer_name = rgb_match.group(1)
                r, g, b = int(rgb_match.group(2)), int(rgb_match.group(3)), int(rgb_match.group(4))
                lw = float(rgb_match.group(5))
                self.layers[layer_name] = {'color': (r, g, b), 'lineweight': lw}
                print(f"DEBUG: Found layer (color): {layer_name} = RGB({r},{g},{b}) LW={lw}")
                i += 1
                continue
            
            i += 1
        return i

    
    def _parse_hatch_style(self, start_idx: int) -> int:
        """解析 hatch_style 块"""
        i = start_idx
        match = re.search(r'hatch_style\s+(\w+)', self.lines[i])
        if not match:
            return i
        
        style_name = match.group(1)
        i += 1  # Skip 'hatch_style name {'
        
        pattern = None
        scale = 1.0
        angle = 0
        
        while i < len(self.lines):
            line = self.lines[i].strip()
            if line == '}':
                break
            
            if 'pattern' in line:
                match = re.search(r'pattern\s*=\s*(\w+);', line)
                if match:
                    pattern = match.group(1)
            elif 'scale' in line:
                match = re.search(r'scale\s*=\s*([\d.]+);', line)
                if match:
                    scale = float(match.group(1))
            elif 'angle' in line:
                match = re.search(r'angle\s*=\s*(\d+);', line)
                if match:
                    angle = int(match.group(1))
            
            i += 1
        
        self.hatch_styles[style_name] = {
            'pattern': pattern,
            'scale': scale,
            'angle': angle
        }
        
        return i
    
    def _parse_sketch(self, start_idx: int) -> int:
        """解析 sketch 块"""
        i = start_idx
        match = re.search(r'sketch\s+(\w+)\s+layer=(\w+)', self.lines[i])
        if not match: return i
        sketch_name = match.group(1)
        layer = match.group(2)
        i += 1
        polylines = []
        lines = []
        while i < len(self.lines):
            line = re.sub(r'//.*$', '', self.lines[i]).strip()
            if not line:
                i += 1
                continue
            if line == '}': break
            
            # Polyline handling
            if 'polyline' in line:
                m = re.search(r'polyline\s+(\w+)\s+(closed|open)', line)
                if m:
                    polyline_name = m.group(1)
                    is_closed = m.group(2) == 'closed'
                    points = []
                    i += 1
                    while i < len(self.lines):
                        p_line = re.sub(r'//.*$', '', self.lines[i]).strip()
                        if not p_line:
                            i += 1
                            continue
                        if p_line == '}': break
                        # Handle multiple points on one line: (x1, y1):r=R -> (x2, y2)
                        p_line = re.sub(r'->', ' ', p_line)
                        # Pattern: (x, y) or (x, y):r=<radius>
                        for p_match in re.finditer(r'\(([^)]+)\)(?::r=([^\s,;>\]]+))?', p_line):
                            coord = p_match.group(1).strip()
                            radius = p_match.group(2).strip() if p_match.group(2) else None
                            points.append({'coord': coord, 'radius': radius})
                        i += 1
                    polylines.append({'name': polyline_name, 'closed': is_closed, 'points': points})
            
            # Line handling
            elif line.startswith('line '):
                # line Name (x1, y1) -> (x2, y2);
                m = re.search(r'line\s+(\w+)\s+\(([^)]+)\)\s*->\s*\(([^)]+)\);', line)
                if m:
                    lines.append({
                        'name': m.group(1),
                        'p1': m.group(2).strip(),
                        'p2': m.group(3).strip()
                    })
            
            # Circle handling: circle Name (cx, cy) r=radius;
            elif line.startswith('circle '):
                m = re.search(r'circle\s+(\w+)\s+\(([^)]+)\)\s+r\s*=\s*([^;]+);', line)
                if m:
                    if 'circles' not in locals():
                        circles = []
                    circles.append({
                        'name': m.group(1),
                        'center': m.group(2).strip(),
                        'radius': m.group(3).strip()
                    })
            
            i += 1
        
        sketch_data = {'name': sketch_name, 'layer': layer, 'polylines': polylines, 'lines': lines}
        if 'circles' in locals():
            sketch_data['circles'] = circles
        self.sketches.append(sketch_data)
        return i
    
    def _parse_region(self, start_idx: int) -> int:
        """解析 region 块"""
        i = start_idx
        match = re.search(r'region\s+(\w+)', self.lines[i])
        if not match: return i
        region_name = match.group(1)
        i += 1
        boundary = None
        hatch = None
        while i < len(self.lines):
            line = re.sub(r'//.*$', '', self.lines[i]).strip()
            if not line:
                i += 1
                continue
            if line == '}': break
            m = re.search(r'boundary\s*=\s*(\w+)\.(\w+);', line)
            if m: boundary = {'sketch': m.group(1), 'polyline': m.group(2)}
            m = re.search(r'hatch\s*=\s*(\w+);', line)
            if m: hatch = m.group(1)
            i += 1
        if boundary:
            self.regions.append({'name': region_name, 'boundary': boundary, 'hatch': hatch})
        return i
    def _parse_dimension(self, start_idx: int) -> int:
        """解析 dimension 块 - supports linear, vertical, radial, diameter"""
        i = start_idx
        # dim linear { or dim vertical { or dim radial { or dim diameter {
        match = re.search(r'dim\s+(\w+)', self.lines[i])
        if not match:
            return i
        
        dim_type = match.group(1)
        i += 1
        
        # Common properties
        from_point = None
        to_point = None
        text = None
        # Radial/diameter specific
        center = None
        radius = None
        diameter = None
        angle = None
        
        while i < len(self.lines):
            line = self.lines[i].strip()
            if line == '}':
                break
            
            # from = (0, 0);
            match = re.search(r'from\s*=\s*\(([^)]+)\);', line)
            if match:
                from_point = match.group(1)
            
            # to = (B, 0);
            match = re.search(r'to\s*=\s*\(([^)]+)\);', line)
            if match:
                to_point = match.group(1)
            
            # center = (x, y);
            match = re.search(r'center\s*=\s*\(([^)]+)\);', line)
            if match:
                center = match.group(1)
            
            # radius = R1;
            match = re.search(r'radius\s*=\s*([^;]+);', line)
            if match:
                radius = match.group(1).strip()
            
            # diameter = D1;
            match = re.search(r'diameter\s*=\s*([^;]+);', line)
            if match:
                diameter = match.group(1).strip()
            
            # angle = 45;
            match = re.search(r'angle\s*=\s*([^;]+);', line)
            if match:
                angle = match.group(1).strip()
            
            # text = "R125";
            match = re.search(r'text\s*=\s*"([^"]+)";', line)
            if match:
                text = match.group(1)
            
            i += 1
        
        # Store dimension based on type
        if dim_type in ['linear', 'vertical', 'horizontal']:
            if from_point and to_point:
                self.dimensions.append({
                    'type': dim_type,
                    'from': from_point,
                    'to': to_point,
                    'text': text
                })
        elif dim_type == 'radial':
            if center and radius:
                self.dimensions.append({
                    'type': 'radial',
                    'center': center,
                    'radius': radius,
                    'angle': angle or '45',
                    'text': text
                })
        elif dim_type == 'diameter':
            if center and diameter:
                self.dimensions.append({
                    'type': 'diameter',
                    'center': center,
                    'diameter': diameter,
                    'angle': angle or '45',
                    'text': text
                })
        
        return i

    def _parse_materials(self, start_idx: int) -> int:
        """解析 materials 块 (支持嵌套)"""
        i = start_idx
        i += 1  # Skip 'materials {'
        
        current_category = None
        
        while i < len(self.lines):
            line = re.sub(r'//.*$', '', self.lines[i]).strip()
            if not line:
                i += 1
                continue
            if line == '}':
                break
            
            # steel {
            cat_match = re.search(r'(\w+)\s*\{', line)
            if cat_match:
                current_category = cat_match.group(1)
                self.materials[current_category] = {}
                i += 1
                while i < len(self.lines):
                    sub_line = re.sub(r'//.*$', '', self.lines[i]).strip()
                    if not sub_line:
                        i += 1
                        continue
                    if sub_line == '}':
                        break
                    # HPB300 { fy = 300; density = 7850; }
                    item_match = re.search(r'(\w+)\s*\{([^}]*)\}', sub_line)
                    if item_match:
                        name = item_match.group(1)
                        props_str = item_match.group(2)
                        props = {}
                        for prop in re.finditer(r'(\w+)\s*=\s*([^;]+);', props_str):
                            props[prop.group(1)] = prop.group(2).strip()
                        self.materials[current_category][name] = props
                    i += 1
            i += 1
        return i

    def _parse_rebar_set(self, start_idx: int) -> int:
        """解析 rebar_set N6 { dia = 6; grade = HPB300; }"""
        i = start_idx
        # Check if it's single line or multi line
        line = self.lines[i].strip()
        if '{' in line and '}' in line:
            match = re.search(r'rebar_set\s+(\w+)\s*\{([^}]*)\}', line)
            if match:
                name = match.group(1)
                props_str = match.group(2)
                props = {}
                for prop in re.finditer(r'(\w+)\s*=\s*([^;]+);', props_str):
                    props[prop.group(1)] = prop.group(2).strip()
                self.rebar_sets[name] = props
                print(f"DEBUG: Found single-line rebar_set: {name}")
            return i + 1  # Return next line (caller uses continue, skipping i += 1)
        
        # Multi-line
        match = re.search(r'rebar_set\s+(\w+)', line)
        if match:
            name = match.group(1)
            props = {}
            i += 1
            while i < len(self.lines):
                l = self.lines[i].strip()
                if l == '}': break
                m = re.search(r'(\w+)\s*=\s*([^;]+);', l)
                if m: props[m.group(1)] = m.group(2).strip()
                i += 1
            self.rebar_sets[name] = props
            print(f"DEBUG: Found multi-line rebar_set: {name}")
        return i

    def _parse_barshape(self, start_idx: int) -> int:
        """解析 barshape Name { ... }"""
        i = start_idx
        match = re.search(r'barshape\s+(\w+)', self.lines[i])
        if not match: return i
        name = match.group(1)
        shape = {'name': name, 'dims': {}, 'hooks': {}, 'annotations': []}
        i += 1
        while i < len(self.lines):
            line = re.sub(r'//.*$', '', self.lines[i]).strip()
            if not line:
                i += 1
                continue
            if line == '}': break
            if line.startswith('type'):
                m = re.search(r'type\s*=\s*(\w+);', line)
                if m: shape['type'] = m.group(1)
            elif line.startswith('segments'):
                # Check for single-line segments
                m = re.search(r'segments\s*=\s*\[(.*)\];', line)
                if m:
                    shape['segments'] = m.group(1)
                else:
                    # Multi-line segments: read until ];
                    if '[' in line:
                        segment_content = line.split('[', 1)[1].strip()
                        i += 1
                        while i < len(self.lines):
                            sl = re.sub(r'//.*$', '', self.lines[i]).strip()
                            if not sl:
                                i += 1
                                continue
                            if '];' in sl:
                                segment_content += ' ' + sl.split('];')[0].strip()
                                break
                            segment_content += ' ' + sl
                            i += 1
                        shape['segments'] = segment_content
            elif line.startswith('bend_radius'):
                # bend_radius = 2.5d; or bend_radius = [r, r, r, r];
                m = re.search(r'bend_radius\s*=\s*([^;]+);', line)
                if m: shape['bend_radius'] = m.group(1).strip()
            elif line.startswith('hooks'):
                # hooks { start = hook(...); end = hook(...); }
                # Check for single-line hooks block
                if '{' in line and '}' in line:
                    # Single-line: hooks { start = hook(...); end = hook(...); }
                    for m in re.finditer(r'(start|end)\s*=\s*hook\(([^)]+)\)\s*;?', line):
                        hook_name = m.group(1)
                        hook_params = {}
                        for param in re.finditer(r'(\w+)\s*=\s*([^,)]+)', m.group(2)):
                            hook_params[param.group(1)] = param.group(2).strip()
                        shape['hooks'][hook_name] = hook_params
                else:
                    # Multi-line hooks block
                    i += 1
                    while i < len(self.lines):
                        hl = re.sub(r'//.*$', '', self.lines[i]).strip()
                        if not hl:
                            i += 1
                            continue
                        if hl == '}':
                            break
                        # start = hook(angle=135, length=10d);
                        m = re.search(r'(start|end)\s*=\s*hook\(([^)]+)\)\s*;', hl)
                        if m:
                            hook_name = m.group(1)
                            hook_params = {}
                            for param in re.finditer(r'(\w+)\s*=\s*([^,)]+)', m.group(2)):
                                hook_params[param.group(1)] = param.group(2).strip()
                            shape['hooks'][hook_name] = hook_params
                        i += 1
            elif line.startswith('dims'):
                # Check for single-line dims block: dims { L; } or dims { a = expr; b = expr; }
                if '{' in line and '}' in line:
                    # Extract content between braces
                    brace_content = re.search(r'\{([^}]*)\}', line)
                    if brace_content:
                        content = brace_content.group(1).strip()
                        # Parse multiple dims separated by ;
                        for dim_part in content.split(';'):
                            dim_part = dim_part.strip()
                            if not dim_part:
                                continue
                            # a = expr or just L (single var)
                            m = re.search(r'(\w+)\s*=\s*(.+)', dim_part)
                            if m:
                                shape['dims'][m.group(1)] = m.group(2).strip()
                            else:
                                # Single variable name like L
                                shape['dims'][dim_part] = dim_part
                else:
                    # Multi-line dims block
                    i += 1
                    while i < len(self.lines):
                        dl = re.sub(r'//.*$', '', self.lines[i]).strip()
                        if not dl:
                            i += 1
                            continue
                        if dl == '}':
                            break
                        m = re.search(r'(\w+)\s*=\s*([^;]+);', dl)
                        if m:
                            shape['dims'][m.group(1)] = m.group(2).strip()
                        i += 1
            elif line.startswith('annotations'):
                i += 1
                while i < len(self.lines):
                    al = re.sub(r'//.*$', '', self.lines[i]).strip()
                    if not al:
                        i += 1
                        continue
                    if al == '}': break
                    # dim at segment(1) text="H3+50";
                    m = re.search(r'(\w+)\s+at\s+(\w+)\((\d+)\)\s+text\s*=\s*"([^"]+)"', al)
                    if m:
                        shape['annotations'].append({
                            'type': m.group(1),
                            'location_type': m.group(2),
                            'index': int(m.group(3)),
                            'text': m.group(4)
                        })
                    i += 1
            elif line.startswith('inherit'):
                m = re.search(r'inherit\s*=\s*(\w+)\s*;', line)
                if m: shape['inherit'] = m.group(1)
            elif line.startswith('set'):
                m = re.search(r'set\s*=\s*(\w+)\s*;', line)
                if m: shape['set'] = m.group(1)
            i += 1
        self.barshapes[name] = shape
        print(f"DEBUG: Found barshape: {name} with hooks={list(shape['hooks'].keys())}")
        return i

    def _parse_table(self, start_idx: int) -> int:
        """解析 table Name { ... }"""
        i = start_idx
        match = re.search(r'table\s+([\w\u4e00-\u9fa5]+)', self.lines[i])
        if not match: return i
        name = match.group(1)
        table = {'name': name, 'columns': {}, 'rows': [], 'compute': {}, 'summary': {}}
        i += 1
        while i < len(self.lines):
            line = re.sub(r'//.*$', '', self.lines[i]).strip()
            if not line:
                i += 1
                continue
            if line == '}': break
            if line.startswith('type'):
                m = re.search(r'type\s*=\s*(\w+);', line)
                if m: table['type'] = m.group(1)
            elif line.startswith('key'):
                m = re.search(r'key\s*=\s*([^;]+);', line)
                if m: table['key'] = m.group(1)
            elif line.startswith('columns'):
                i += 1
                while i < len(self.lines):
                    cl = re.sub(r'//.*$', '', self.lines[i]).strip()
                    if not cl:
                        i += 1
                        continue
                    if cl == '}': break
                    m = re.search(r'([\w\u4e00-\u9fa5]+):\s*([^;]+);', cl)
                    if m: table['columns'][m.group(1)] = m.group(2).strip()
                    i += 1
            elif line.startswith('row'):
                m = re.search(r'row\s*(?:(\w+)\s*)?\{([^}]*)\}', line)
                if m:
                    row_data = {}
                    pairs = re.finditer(r'([\w\u4e00-\u9fa5]+)\s*=\s*([^;]+);', m.group(2))
                    for p in pairs:
                        val = p.group(2).strip()
                        if val.startswith('"') and val.endswith('"'): val = val[1:-1]
                        row_data[p.group(1)] = val
                    table['rows'].append(row_data)
            elif line.startswith('compute'):
                i += 1
                while i < len(self.lines):
                    cl = re.sub(r'//.*$', '', self.lines[i]).strip()
                    if not cl:
                        i += 1
                        continue
                    if cl == '}': break
                    m = re.search(r'([\w\u4e00-\u9fa5]+)\s*=\s*([^;]+);', cl)
                    if m: table['compute'][m.group(1)] = m.group(2).strip()
                    i += 1
            elif line.startswith('summary'):
                # summary { HPB300钢筋 = sum(总重 where 规格.grade == HPB300); }
                i += 1
                while i < len(self.lines):
                    sl = re.sub(r'//.*$', '', self.lines[i]).strip()
                    if not sl:
                        i += 1
                        continue
                    if sl == '}': break
                    m = re.search(r'([\w\u4e00-\u9fa5]+)\s*=\s*([^;]+);', sl)
                    if m: table['summary'][m.group(1)] = m.group(2).strip()
                    i += 1
            elif line.startswith('layout'):
                # layout { header { row { cell "..."; } } }
                table['layout'] = {'header': {'rows': []}}
                i += 1
                while i < len(self.lines):
                    ll = re.sub(r'//.*$', '', self.lines[i]).strip()
                    if not ll:
                        i += 1
                        continue
                    if ll == '}': break
                    if ll.startswith('header'):
                        i += 1
                        while i < len(self.lines):
                            hl = re.sub(r'//.*$', '', self.lines[i]).strip()
                            if not hl:
                                i += 1
                                continue
                            if hl == '}': break
                            if hl.startswith('row'):
                                # row { cell "编号"; cell "规格"; ... }
                                cells = re.findall(r'cell\s+"?([^";]+)"?(?:\s+\w+\s*=\s*\d+)*', hl)
                                if not cells:
                                    cells = re.findall(r'cell\s+([\w\u4e00-\u9fa5]+)', hl)
                                table['layout']['header']['rows'].append(cells)
                            i += 1
                    i += 1
            i += 1
        self.tables[name] = table
        print(f"DEBUG: Found table: {name} with {len(table['rows'])} rows, summary={list(table['summary'].keys())}")
        return i

    def _parse_sheet(self, start_idx: int) -> int:
        """解析 sheet Name { ... }"""
        i = start_idx
        match = re.search(r'sheet\s+([\w\u4e00-\u9fa5]+)', self.lines[i])
        if not match: return i
        
        name = match.group(1)
        sheet = {'name': name, 'placements': [], 'notes': []}
        i += 1
        
        while i < len(self.lines):
            line = self.lines[i].strip()
            if line == '}': break
            
            if line.startswith('size'):
                m = re.search(r'size\s*=\s*(\w+);', line)
                if m: sheet['size'] = m.group(1)
            elif line.startswith('scale'):
                m = re.search(r'scale\s*=\s*([^;]+);', line)
                if m: sheet['scale'] = m.group(1)
            elif line.startswith('titleblock'):
                i += 1
                tb = {}
                while i < len(self.lines):
                    t_line = self.lines[i].strip()
                    if t_line == '}': break
                    m = re.search(r'(\w+)\s*=\s*"([^"]*)";', t_line)
                    if m: tb[m.group(1)] = m.group(2)
                    i += 1
                sheet['titleblock'] = tb
            elif line.startswith('place'):
                # place table Name;
                m = re.search(r'place\s+(\w+)\s+([\w\u4e00-\u9fa5]+);', line)
                if m: sheet['placements'].append({'type': m.group(1), 'name': m.group(2)})
            i += 1
        self.sheets[name] = sheet
        return i
    
    def _parse_origin(self, start_idx: int) -> int:
        """解析 origin (x, y); 语句"""
        line = self.lines[start_idx].strip()
        match = re.search(r'origin\s*\(([^,]+),\s*([^)]+)\)\s*;?', line)
        if match:
            try:
                x = float(match.group(1).strip())
                y = float(match.group(2).strip())
                self.origin = (x, y)
                print(f"DEBUG: Found origin: ({x}, {y})")
            except ValueError:
                # Expression-based origin, store as strings
                self.origin = (match.group(1).strip(), match.group(2).strip())
        return start_idx + 1
    
    def _parse_drawing_info(self, start_idx: int) -> int:
        """解析 drawing_info { ... } 块"""
        i = start_idx + 1  # Skip 'drawing_info {'
        
        while i < len(self.lines):
            line = re.sub(r'//.*$', '', self.lines[i]).strip()
            if not line:
                i += 1
                continue
            if line == '}':
                break
            
            # title = "...";
            match = re.search(r'(\w+)\s*=\s*"([^"]*)"\s*;', line)
            if match:
                self.drawing_info[match.group(1)] = match.group(2)
            else:
                # Non-quoted values: scale = 1:50;
                match = re.search(r'(\w+)\s*=\s*([^;]+);', line)
                if match:
                    self.drawing_info[match.group(1)] = match.group(2).strip()
            i += 1
        
        print(f"DEBUG: Found drawing_info: {self.drawing_info}")
        return i
    
    def _parse_view(self, start_idx: int) -> int:
        """解析 view Name { ... } 块"""
        i = start_idx
        match = re.search(r'view\s+([\w\u4e00-\u9fa5]+)', self.lines[i])
        if not match:
            return i
        
        name = match.group(1)
        view = {'name': name}
        i += 1
        
        while i < len(self.lines):
            line = re.sub(r'//.*$', '', self.lines[i]).strip()
            if not line:
                i += 1
                continue
            if line == '}':
                break
            
            # source = Name;
            m = re.search(r'source\s*=\s*([\w\u4e00-\u9fa5]+)\s*;', line)
            if m:
                view['source'] = m.group(1)
            
            # at = (x, y);
            m = re.search(r'at\s*=\s*\(([^,]+),\s*([^)]+)\)\s*;', line)
            if m:
                view['at'] = (m.group(1).strip(), m.group(2).strip())
            
            # scale = 1:15;
            m = re.search(r'scale\s*=\s*([^;]+);', line)
            if m:
                view['scale'] = m.group(1).strip()
            
            # section = Name;
            m = re.search(r'section\s*=\s*([\w\u4e00-\u9fa5]+)\s*;', line)
            if m:
                view['section'] = m.group(1)
            
            i += 1
        
        self.views[name] = view
        print(f"DEBUG: Found view: {name}")
        return i
    
    def _parse_notes(self, start_idx: int) -> int:
        """解析 notes at (x, y) layer=xxx { ... } 块"""
        i = start_idx
        line = self.lines[i].strip()
        
        # notes at (x, y) [layer=xxx] {
        note = {'items': []}
        
        at_match = re.search(r'at\s*\(([^,]+),\s*([^)]+)\)', line)
        if at_match:
            note['at'] = (at_match.group(1).strip(), at_match.group(2).strip())
        
        layer_match = re.search(r'layer\s*=\s*(\w+)', line)
        if layer_match:
            note['layer'] = layer_match.group(1)
        
        i += 1
        
        while i < len(self.lines):
            l = re.sub(r'//.*$', '', self.lines[i]).strip()
            if not l:
                i += 1
                continue
            if l == '}':
                break
            
            # title = "注：";
            m = re.search(r'title\s*=\s*"([^"]*)"\s*;', l)
            if m:
                note['title'] = m.group(1)
            
            # items = ["...", "...", ...];
            m = re.search(r'items\s*=\s*\[(.*)\]\s*;', l, re.DOTALL)
            if m:
                items_str = m.group(1)
                items = re.findall(r'"([^"]*)"', items_str)
                note['items'] = items
            
            i += 1
        
        self.notes.append(note)
        print(f"DEBUG: Found notes block with {len(note.get('items', []))} items")
        return i
    
    def _parse_mesh(self, start_idx: int) -> int:
        """解析 mesh Name layer=xxx { ... } 块"""
        i = start_idx
        line = self.lines[i].strip()
        
        match = re.search(r'mesh\s+([\w\u4e00-\u9fa5]+)\s+layer\s*=\s*(\w+)', line)
        if not match:
            return i
        
        mesh = {'name': match.group(1), 'layer': match.group(2)}
        i += 1
        
        while i < len(self.lines):
            l = re.sub(r'//.*$', '', self.lines[i]).strip()
            if not l:
                i += 1
                continue
            if l == '}':
                break
            
            # set = N6;
            m = re.search(r'set\s*=\s*(\w+)\s*;', l)
            if m:
                mesh['set'] = m.group(1)
            
            # region = RegionName;
            m = re.search(r'region\s*=\s*([\w\u4e00-\u9fa5]+)\s*;', l)
            if m:
                mesh['region'] = m.group(1)
            
            # spacing_x = 120;
            m = re.search(r'spacing_x\s*=\s*([^;]+);', l)
            if m:
                mesh['spacing_x'] = m.group(1).strip()
            
            # spacing_y = 120;
            m = re.search(r'spacing_y\s*=\s*([^;]+);', l)
            if m:
                mesh['spacing_y'] = m.group(1).strip()
            
            # style = "grid";
            m = re.search(r'style\s*=\s*"?(\w+)"?\s*;', l)
            if m:
                mesh['style'] = m.group(1)
            
            # label = "N6@12";
            m = re.search(r'label\s*=\s*"([^"]*)"\s*;', l)
            if m:
                mesh['label'] = m.group(1)
            
            i += 1
        
        self.meshes.append(mesh)
        print(f"DEBUG: Found mesh: {mesh['name']}")
        return i
    
    def _parse_bars(self, start_idx: int) -> int:
        """解析 bars Name layer=xxx { ... } 块"""
        i = start_idx
        line = self.lines[i].strip()
        
        match = re.search(r'bars\s+([\w\u4e00-\u9fa5]+)\s+layer\s*=\s*(\w+)', line)
        if not match:
            # Try without layer
            match = re.search(r'bars\s+([\w\u4e00-\u9fa5]+)', line)
            if not match:
                return i
            bars = {'name': match.group(1)}
        else:
            bars = {'name': match.group(1), 'layer': match.group(2)}
        
        i += 1
        
        while i < len(self.lines):
            l = re.sub(r'//.*$', '', self.lines[i]).strip()
            if not l:
                i += 1
                continue
            if l == '}':
                break
            
            # set = N12;
            m = re.search(r'set\s*=\s*(\w+)\s*;', l)
            if m:
                bars['set'] = m.group(1)
            
            # path = ...;
            m = re.search(r'path\s*=\s*([^;]+);', l)
            if m:
                bars['path'] = m.group(1).strip()
            
            # count = 2;
            m = re.search(r'count\s*=\s*(\d+)\s*;', l)
            if m:
                bars['count'] = int(m.group(1))
            
            # spacing = 50;
            m = re.search(r'spacing\s*=\s*([^;]+);', l)
            if m:
                bars['spacing'] = m.group(1).strip()
            
            # label = "N12×2";
            m = re.search(r'label\s*=\s*"([^"]*)"\s*;', l)
            if m:
                bars['label'] = m.group(1)
            
            # render_style = path | dots;
            m = re.search(r'render_style\s*=\s*(\w+)\s*;', l)
            if m:
                bars['render_style'] = m.group(1)
            
            # dot_radius = ...;
            m = re.search(r'dot_radius\s*=\s*([^;]+);', l)
            if m:
                bars['dot_radius'] = m.group(1).strip()
            
            # region = ...;
            m = re.search(r'region\s*=\s*([\w\u4e00-\u9fa5]+)\s*;', l)
            if m:
                bars['region'] = m.group(1)
            
            # position = ...;
            m = re.search(r'position\s*=\s*(\w+)\s*;', l)
            if m:
                bars['position'] = m.group(1)
            
            i += 1
        
        self.bars_list.append(bars)
        print(f"DEBUG: Found bars: {bars['name']}")
        return i
    
    def _parse_label(self, start_idx: int) -> int:
        """解析 label "text" at (x, y) layer=xxx; 语句"""
        line = self.lines[start_idx].strip()
        
        # label "A-A" at (500,1200) layer=text;
        m = re.search(r'label\s+"([^"]+)"\s+at\s*\(([^,]+),\s*([^)]+)\)(?:\s+layer\s*=\s*(\w+))?\s*;?', line)
        if m:
            label = {
                'text': m.group(1),
                'at': (m.group(2).strip(), m.group(3).strip()),
                'layer': m.group(4) if m.group(4) else 'text'
            }
            self.labels.append(label)
            print(f"DEBUG: Found label: {label['text']}")
        
        return start_idx + 1
    
    def _parse_callout(self, start_idx: int) -> int:
        """解析 callout Name { ... } 块"""
        i = start_idx
        match = re.search(r'callout\s+([\w\u4e00-\u9fa5]+)', self.lines[i])
        if not match:
            return i
        
        callout = {'name': match.group(1)}
        i += 1
        
        while i < len(self.lines):
            l = re.sub(r'//.*$', '', self.lines[i]).strip()
            if not l:
                i += 1
                continue
            if l == '}':
                break
            
            # at = (x, y); or at = [(x1, y1), (x2, y2)];
            m = re.search(r'at\s*=\s*\(([^,]+),\s*([^)]+)\)\s*;', l)
            if m:
                callout['at'] = (m.group(1).strip(), m.group(2).strip())
            
            # text = "...";
            m = re.search(r'text\s*=\s*"([^"]*)"\s*;', l)
            if m:
                callout['text'] = m.group(1)
            
            # leader = auto;
            m = re.search(r'leader\s*=\s*(\w+)\s*;', l)
            if m:
                callout['leader'] = m.group(1)
            
            # leader_style = straight | curved | spline;
            m = re.search(r'leader_style\s*=\s*(\w+)\s*;', l)
            if m:
                callout['leader_style'] = m.group(1)
            
            i += 1
        
        self.callouts.append(callout)
        print(f"DEBUG: Found callout: {callout['name']}")
        return i

    def _parse_barshape_layout(self, start_idx: int) -> int:
        """解析 barshape_layout Name { ... } 块 - freeform grid placement"""
        i = start_idx
        match = re.search(r'barshape_layout\s+([\w\u4e00-\u9fa5]+)', self.lines[i])
        if not match:
            return i
        
        name = match.group(1)
        layout = {
            'name': name,
            'title': '',
            'grid': (3, 3),
            'cell_size': (100, 150),
            'origin': (0, 0),
            'placements': []
        }
        i += 1
        
        while i < len(self.lines):
            line = re.sub(r'//.*$', '', self.lines[i]).strip()
            if not line:
                i += 1
                continue
            if line == '}':
                break
            
            # title = "Details of rebars";
            m = re.search(r'title\s*=\s*"([^"]*)"', line)
            if m:
                layout['title'] = m.group(1)
            
            # grid = 3x3; or grid = (3, 3);
            m = re.search(r'grid\s*=\s*(\d+)\s*x\s*(\d+)', line)
            if m:
                layout['grid'] = (int(m.group(1)), int(m.group(2)))
            else:
                m = re.search(r'grid\s*=\s*\(\s*(\d+)\s*,\s*(\d+)\s*\)', line)
                if m:
                    layout['grid'] = (int(m.group(1)), int(m.group(2)))
            
            # cell_size = (100, 150);
            m = re.search(r'cell_size\s*=\s*\(\s*([^,]+)\s*,\s*([^)]+)\s*\)', line)
            if m:
                try:
                    layout['cell_size'] = (float(m.group(1)), float(m.group(2)))
                except:
                    layout['cell_size'] = (100, 150)
            
            # origin = (0, 0);
            m = re.search(r'origin\s*=\s*\(\s*([^,]+)\s*,\s*([^)]+)\s*\)', line)
            if m:
                try:
                    layout['origin'] = (float(m.group(1)), float(m.group(2)))
                except:
                    layout['origin'] = (0, 0)
            
            # place N1 at (0, 0) { ... }
            if line.startswith('place'):
                m = re.search(r'place\s+(\w+)\s+at\s*\(\s*(\d+)\s*,\s*(\d+)\s*\)\s*\{', line)
                if m:
                    shape_name = m.group(1)
                    col = int(m.group(2))
                    row = int(m.group(3))
                    
                    props_str = ""
                    current_depth = 0
                    
                    # Analyze the first line (the place line)
                    # It definitely has at least one '{' from the regex match
                    # We need to count braces in this line properly
                    # Remove comments first
                    clean_line = re.sub(r'//.*$', '', line)
                    # Count braces
                    current_depth += clean_line.count('{')
                    current_depth -= clean_line.count('}')
                    
                    # Extract content after the first '{'
                    if '{' in clean_line:
                        props_str = clean_line.split('{', 1)[1] + " "
                    
                    # If depth is already 0, we are done (single line case like: place ... { ... })
                    if current_depth > 0:
                         i += 1
                         while i < len(self.lines):
                            l = re.sub(r'//.*$', '', self.lines[i]).strip()
                            if not l:
                                i += 1
                                continue
                            
                            # Count braces in this line
                            open_count = l.count('{')
                            close_count = l.count('}')
                            
                            # Check if this line closes the block
                            # We need to be careful: does the block close mid-line?
                            # For simplicity in this parser, we assume standard formatting or just accumulate
                            # But strictly we should find WHERE it closes.
                            
                            # However, to fix the immediate bug, we just need to verify depth reaches 0
                            
                            if current_depth + open_count - close_count == 0:
                                # This line finishes the block.
                                # But we might have content before the final '}'
                                # And potentially multiple '}'
                                # It's safer to just accumulate the whole line (or up to the closing brace)
                                # Given the complexity, let's assume the last '}' that brings depth to 0 is the end.
                                # But simpler: just accumulate until depth is 0.
                                
                                # Accumulate
                                props_str += l + " "
                                
                                current_depth += open_count
                                current_depth -= close_count
                                break
                            
                            current_depth += open_count
                            current_depth -= close_count
                            props_str += l + " "
                            i += 1
                    
                    placement = {
                        'shape': shape_name,
                        'col': col,
                        'row': row,
                        'label': '',
                        'note': ''
                    }
                    
                    # Parse properties inside the braces
                    label_m = re.search(r'label\s*=\s*"([^"]*)"', props_str)
                    if label_m:
                        placement['label'] = label_m.group(1)
                    
                    note_m = re.search(r'note\s*=\s*"([^"]*)"', props_str)
                    if note_m:
                        placement['note'] = note_m.group(1)
                    
                    layout['placements'].append(placement)
                
                    # Parse annotations list
                    annotations = []
                    # Check for "annotations = [ ... ]" inside placement block
                    ann_match = re.search(r'annotations\s*=\s*\[(.*?)\]', props_str, re.DOTALL)
                    if ann_match:
                        ann_content = ann_match.group(1)
                        # Parse individual annotation objects { text="..."; at=(x,y); ... }
                        # This regex finds content between { and }
                        for ann_obj_match in re.finditer(r'\{([^}]+)\}', ann_content):
                            ann_props_str = ann_obj_match.group(1)
                            ann = {'text': '', 'at': (0, 0), 'layer': 'text'}
                            
                            # Parse text
                            t_m = re.search(r'text\s*=\s*"([^"]*)"', ann_props_str)
                            if t_m: ann['text'] = t_m.group(1)
                            
                            # Parse at
                            at_m = re.search(r'at\s*=\s*\(\s*([^,]+)\s*,\s*([^)]+)\s*\)', ann_props_str)
                            if at_m:
                                try:
                                    ann['at'] = (float(at_m.group(1)), float(at_m.group(2)))
                                except:
                                    pass
                                    
                            # Parse layer
                            l_m = re.search(r'layer\s*=\s*(\w+)', ann_props_str)
                            if l_m: ann['layer'] = l_m.group(1)
                            
                            # Parse angle
                            a_m = re.search(r'angle\s*=\s*([-\d.]+)', ann_props_str)
                            if a_m: ann['angle'] = float(a_m.group(1))
                            
                            annotations.append(ann)
                    
                    placement['annotations'] = annotations
            
            i += 1
        
        self.barshape_layouts[name] = layout
        print(f"DEBUG: Found barshape_layout: {name} with {len(layout['placements'])} placements")
        return i


class AutoLISPGenerator:
    """AutoLISP 代码生成器 (Approach 4: 全局变量参数注入)"""
    
    def __init__(self, parser: PCADParser):
        self.parser = parser
        self.code = []
    
    def generate(self) -> str:
        """生成完整的 AutoLISP 代码"""
        self.code = []
        
        self._add_header()
        self._add_set_params_function()
        self._generate_utility_functions()  # Generate helpers at global scope
        self._add_render_function()
        self._add_usage_comments()
        
        return '\n'.join(self.code)

    def _estimate_dim_scale(self) -> float:
        """根据绘图尺寸和 A4 纸张比例 (297mm) 估算 DIM_SCALE，并舍入到 5 的倍数"""
        max_dim = 1000.0  # 默认值
        
        # 1. 检查 barshape_layouts 中的 cell_size
        if hasattr(self.parser, 'barshape_layouts'):
            for layout in self.parser.barshape_layouts.values():
                cell_size = layout.get('cell_size', (1000, 1000))
                grid = layout.get('grid', (1, 1))
                
                if isinstance(cell_size, (list, tuple)) and len(cell_size) >= 2:
                    try:
                        # Calculate total dimensions considering grid
                        cols = 1
                        rows = 1
                        if isinstance(grid, (list, tuple)) and len(grid) >= 2:
                            cols = float(grid[0])
                            rows = float(grid[1])
                            
                        total_w = float(cell_size[0]) * cols
                        total_h = float(cell_size[1]) * rows
                        
                        max_dim = max(max_dim, total_w, total_h)
                    except: pass
            
        # 2. 检查 sketches 中的数值
        if hasattr(self.parser, 'sketches'):
            for sketch in self.parser.sketches:
                for polyline in sketch['polylines']:
                    for pt in polyline['points']:
                        # Handle both old format (string) and new format (dict)
                        if isinstance(pt, dict):
                            pt_expr = pt.get('coord', '')
                        else:
                            pt_expr = pt
                        nums = [float(n) for n in re.findall(r'[-+]?\d*\.?\d+', pt_expr)]
                        if nums:
                            max_dim = max(max_dim, max(map(abs, nums)))
                        
        # 3. 检查参数值
        if hasattr(self.parser, 'params'):
            for param in self.parser.params.values():
                val = param.get('value', 0)
                if isinstance(val, (int, float)):
                    max_dim = max(max_dim, abs(float(val)))
                
        # 目标比例：(最大尺寸 / 297) 舍入到 5 的倍数
        # dim-scale = ( (max bound size) / 297 ) round to 5's times
        raw_scale = max_dim / 297.0
        scale = round(raw_scale / 5.0) * 5.0
        
        return float(max(1.0, scale))
    
    def _add_header(self):
        """添加文件头"""
        self.code.append("; =========================================")
        self.code.append("; AutoLISP generated from P-CAD")
        self.code.append("; Parameter Injection: Approach 4 (Global Variables)")
        self.code.append("; =========================================")
        self.code.append("")
    
    def _add_set_params_function(self):
        """添加参数设置函数"""
        param_names = list(self.parser.params.keys())
        
        # Generate function parameter list dynamically
        param_val_names = [f"{name}_val" for name in param_names]
        param_list_str = " ".join(param_val_names)
        
        # Generate usage comment
        usage_str = " ".join(param_names)
        
        self.code.append("; =========================================")
        self.code.append("; Function: Set Parameters")
        self.code.append(f"; Usage: (set-params {usage_str})")
        self.code.append("; =========================================")
        self.code.append(f"(defun set-params ({param_list_str})")
        
        # Generate parameter assignment code
        for i, param_name in enumerate(param_names):
            val_name = param_val_names[i]
            # Handle 't' specially (it's a reserved symbol in AutoLISP)
            if param_name == 't':
                self.code.append(f"  (setq t_param {val_name})  ; Use t_param to avoid conflict with AutoLISP 't'")
                self.code.append("  (setq t t_param)")
            else:
                self.code.append(f"  (setq {param_name} {val_name})")
        
        # Generate parameter print message dynamically
        print_parts = []
        for param_name in param_names:
            if param_name == 't':
                print_parts.append(f'\"{param_name}=\" (rtos t_param)')
            else:
                print_parts.append(f'\"{param_name}=\" (rtos {param_name})')
        
        print_str = " \", \" ".join(print_parts)
        self.code.append(f"  (princ (strcat \"\\nParameters set: \" {print_str} \"\\n\"))")
        self.code.append("  (princ)")
        self.code.append(")")
        self.code.append("")
    
    def _add_render_function(self):
        """添加渲染函数"""
        self.code.append("; =========================================")
        self.code.append("; Function: Render P-CAD Geometry")
        self.code.append("; Usage: (c:PCAD_Render)")
        self.code.append("; Note: Call (set-params ...) first to set parameters")
        self.code.append("; =========================================")
        self.code.append("(defun c:PCAD_Render ()")
        self.code.append("  (setvar \"CMDECHO\" 0)")
        
        # Estimate dynamic DIMSCALE
        self.estimated_scale = self._estimate_dim_scale()
        self.code.append(f"  (setvar \"DIMSCALE\" {self.estimated_scale})")
        self.code.append(f"  (setvar \"DIMTXT\" {DIM_TEXT_HEIGHT})")
        self.code.append(f"  (setvar \"TEXTSIZE\" {DEFAULT_TEXT_SIZE})")
        self.code.append("")
        
        # Check if parameters are set, use defaults if not
        self.code.append("  ; Check and set default parameters if not defined")
        for param_name, param_info in self.parser.params.items():
            default = param_info['value']
            # Handle 't' specially (it's a reserved symbol in AutoLISP)
            if param_name == 't':
                # Ensure default is a proper float (not 200.0.0)
                if isinstance(default, int):
                    default_str = f"{default}.0"
                elif isinstance(default, float):
                    default_str = str(default) if '.' in str(default) else f"{default}.0"
                else:
                    default_str = str(default)
                self.code.append(f"  (if (not (boundp 't_param)) (setq t_param {default_str}))")
                self.code.append("  (setq t t_param)  ; Use t_param to avoid conflict with AutoLISP 't'")
            else:
                # Ensure default is a proper float (not 2000.0.0)
                if isinstance(default, int):
                    default_str = f"{default}.0"
                elif isinstance(default, float):
                    # If already a float, use as-is (avoid 2000.0.0)
                    default_str = str(default) if '.' in str(default) else f"{default}.0"
                else:
                    default_str = str(default)
                self.code.append(f"  (if (not (boundp '{param_name})) (setq {param_name} {default_str}))")
        self.code.append("")
        
        # Calculate derived values
        self.code.append("  ; Calculate derived values")
        for derive_name, derive_expr in self.parser.derive.items():
            lisp_expr = self._convert_expr_to_lisp(derive_expr)
            # Handle 't' variable specially in derived expressions
            if 't' in lisp_expr and lisp_expr != 't':
                lisp_expr = lisp_expr.replace(' t ', ' t_param ').replace('(t)', '(t_param)')
            if lisp_expr == 't':
                lisp_expr = 't_param'
            self.code.append(f"  (setq {derive_name} {lisp_expr})")
        self.code.append("")
        
        # Setup layers
        self._generate_layers()
        
        # Setup text style for all text entities (dimensions, labels, tables, etc.)
        self.code.append(f"  ; Setup text style: {TEXT_FONT}, width factor {TEXT_WIDTH_FACTOR}")
        self.code.append(f"  (command \"._-STYLE\" \"Standard\" \"{TEXT_FONT}\" 0 {TEXT_WIDTH_FACTOR} 0 \"_N\" \"_N\" \"\")")
        self.code.append("")
        
        # Generate sketches (polylines)
        self._generate_sketches()
        
        # Generate regions (hatches)
        self._generate_regions()
        
        # Generate dimensions
        self._generate_dimensions()
        
        # Generate bars
        self._generate_bars()
        
        # Generate barshapes
        self._generate_barshapes()
        
        # Generate tables
        self._generate_tables()
        
        # Generate sheets (layouts)
        self._generate_sheets()
        
        # Generate barshape layouts (freeform placement)
        self._generate_barshape_layouts()
        
        # Generate labels
        self._generate_labels()
        
        self.code.append("  (setvar \"CMDECHO\" 1)")
        self.code.append("  (princ \"\\nP-CAD rendering complete.\\n\")")
        self.code.append("  (princ)")
        self.code.append(")")
        self.code.append("")
    
    def _convert_expr_to_lisp(self, expr: str) -> str:
        """将 P-CAD 表达式转换为 AutoLISP 表达式，支持单位转换"""
        expr = expr.strip()
        
        # Handle unit-qualified literals (e.g. 8cm, 12mm, 0.55m)
        unit_match = re.search(r'^([\d.]+)(mm|cm|m|kg|kg/m|m3)$', expr)
        if unit_match:
            val = float(unit_match.group(1))
            unit = unit_match.group(2)
            # Normalize to mm (or other base unit as per parser_units)
            factors = {'mm': 1.0, 'cm': 10.0, 'm': 1000.0}
            if unit in factors:
                return str(val * factors[unit])
            return str(val)

        # Handle built-in accessors (simplified)
        if expr.startswith('table('):
            # table(TableName, key) -> (get-table-row "TableName" key)
            return f"; Table accessor not fully implemented: {expr}"

        # Handle math functions: sin(), cos(), tan(), sqrt(), abs(), max(), min(), floor(), fix()
        # "fix" in AutoLISP is truncation (like floor but towards zero for negatives, or just int conversion)
        math_funcs = ['sin', 'cos', 'tan', 'atan', 'asin', 'acos', 'atan2', 'sqrt', 'abs', 'max', 'min', 'floor', 'fix']
        
        # Helper to find matching closing parenthesis
        def find_closing_paren(s, start):
            depth = 1
            for k in range(start, len(s)):
                if s[k] == '(': depth += 1
                elif s[k] == ')': depth -= 1
                if depth == 0: return k
            return -1

        for func in math_funcs:
            # Check for generic function pattern func(...)
            # Use strict startswith check but handle whitespace safely? 
            # Actually expr matches exactly or is clean substring.
            if expr.startswith(f'{func}('):
                # Ensure it closes at the end
                if not expr.endswith(')'):
                    continue
                    
                inner = expr[len(func)+1:-1]
                
                # Split args by comma respecting parentheses
                args = []
                current_arg = ""
                d = 0
                for char in inner:
                    if char == '(': d += 1
                    elif char == ')': d -= 1
                    elif char == ',' and d == 0:
                        args.append(current_arg.strip())
                        current_arg = ""
                        continue
                    current_arg += char
                if current_arg: args.append(current_arg.strip())
                
                # Convert all args
                lisp_args = [self._convert_expr_to_lisp(arg) for arg in args]
                
                # Special handling for trig functions (degrees to radians)
                if func in ['sin', 'cos', 'tan']:
                    if len(lisp_args) == 1:
                        return f"({func} (* {lisp_args[0]} (/ pi 180.0)))"
                
                # Special handling for inverse trig (radians to degrees)
                if func in ['atan', 'asin', 'acos']:
                    if len(lisp_args) == 1:
                        return f"(* ({func} {lisp_args[0]}) (/ 180.0 pi))"
                
                # Special handling for atan2 (radians to degrees)
                if func == 'atan2':
                    if len(lisp_args) == 2:
                        # AutoLISP atan takes (y [x])
                        return f"(* (atan {lisp_args[0]} {lisp_args[1]}) (/ 180.0 pi))"
                
                # Special handling for pow(b, e) -> (expt b e)
                if func == 'pow': # Not in list above but captured if we add it? No "pow" is not in math_funcs list yet.
                    pass 
                
                # Standard AutoLISP mapping
                # max, min take variable args: (max a b c)
                # sqrt, abs, floor, fix take 1 arg
                
                return f"({func} {' '.join(lisp_args)})"
        
        # Handle pow(base, exp) specifically if not in generic list (it maps to expt)
        if expr.startswith('pow(') and expr.endswith(')'):
             inner = expr[4:-1]
             # Parse args safely
             args = []
             current_arg = ""
             d = 0
             for char in inner:
                 if char == '(': d += 1
                 elif char == ')': d -= 1
                 elif char == ',' and d == 0:
                     args.append(current_arg.strip())
                     current_arg = ""
                     continue
                 current_arg += char
             if current_arg: args.append(current_arg.strip())
             
             if len(args) == 2:
                 base = self._convert_expr_to_lisp(args[0])
                 exp = self._convert_expr_to_lisp(args[1])
                 return f"(expt {base} {exp})"

        # Handle simple variable reference
        if re.match(r'^[a-zA-Z_][a-zA-Z0-9_]*$', expr):
            # Handle 't' specially
            if expr == 't':
                return 't_param'
            return expr
        
        # Handle numeric literals (positive)
        if re.match(r'^[\d.]+$', expr):
            return expr
        
        # Handle unary minus: -var or -123 or -(expr)
        if expr.startswith('-'):
            inner = expr[1:].strip()
            # If it's a negative number literal, return as-is
            if re.match(r'^[\d.]+$', inner):
                return expr  # e.g., "-123" is valid in AutoLISP
            # Otherwise, convert to (- 0 inner) for variables/expressions
            inner_lisp = self._convert_expr_to_lisp(inner)
            return f"(- 0 {inner_lisp})"
        
        # Handle expressions with parentheses: (B - W) / 2
        # First, check if entire expression is wrapped in parentheses
        if expr.startswith('(') and expr.endswith(')'):
            # Check if it's a complete parenthesized expression
            depth = 0
            complete = True
            for i, char in enumerate(expr):
                if char == '(':
                    depth += 1
                elif char == ')':
                    depth -= 1
                    if depth == 0 and i < len(expr) - 1:
                        complete = False
                        break
            if complete:
                # Remove outer parentheses and recurse
                return self._convert_expr_to_lisp(expr[1:-1])
        
        # Handle addition/subtraction (lowest precedence, process from right to left for left-associativity)
        # Process from right to left to maintain left-associativity: a - b - c = (a - b) - c
        depth = 0
        for i in range(len(expr) - 1, -1, -1):  # Scan from right to left
            char = expr[i]
            if char == ')':
                depth += 1
            elif char == '(':
                depth -= 1
            elif depth == 0:
                if char == '+' and i > 0 and not expr[i-1] in '*/+-':
                    left = self._convert_expr_to_lisp(expr[:i].strip())
                    right = self._convert_expr_to_lisp(expr[i+1:].strip())
                    return f"(+ {left} {right})"
                elif char == '-' and i > 0 and not expr[i-1] in '*/+-':
                    left = self._convert_expr_to_lisp(expr[:i].strip())
                    right = self._convert_expr_to_lisp(expr[i+1:].strip())
                    return f"(- {left} {right})"
        
        # Handle division/multiplication (higher precedence)
        # Process from right to left for left-associativity
        depth = 0
        for i in range(len(expr) - 1, -1, -1):  # Scan from right to left
            char = expr[i]
            if char == ')':
                depth += 1
            elif char == '(':
                depth -= 1
            elif depth == 0:
                if char == '/' and i > 0:
                    left = self._convert_expr_to_lisp(expr[:i].strip())
                    right = self._convert_expr_to_lisp(expr[i+1:].strip())
                    # Ensure right operand is float if it's a literal number
                    if re.match(r'^[\d.]+$', right) and '.' not in right:
                        right = f"{right}.0"
                    return f"(/ {left} {right})"
                elif char == '*' and i > 0:
                    left = self._convert_expr_to_lisp(expr[:i].strip())
                    right = self._convert_expr_to_lisp(expr[i+1:].strip())
                    return f"(* {left} {right})"
        
        # Fallback: return as-is (might be a variable name)
        return expr
    
    def _generate_layers(self):
        """生成图层设置代码"""
        self.code.append("  ; Setup layers")
        
        # Ensure standard layers exist if not declared in PCAD
        standard_layers = {
            'outline': {'color': (0, 255, 255), 'lineweight': 0.25},
            'rebar': {'color': (255, 0, 0), 'lineweight': 0.20},
            'text': {'color': (0, 255, 0), 'lineweight': 0.18},
            'dim': {'color': (0, 255, 255), 'lineweight': 0.18},
            'hatch': {'color': (180, 180, 180), 'lineweight': 0.10}
        }
        for name, info in standard_layers.items():
            if name not in self.parser.layers:
                self.parser.layers[name] = info
        
        # RGB to ACI (AutoCAD Color Index) conversion
        def rgb_to_aci(r, g, b):
            # Better mapping based on color components
            if r > 200 and g < 50 and b < 50: return 1  # Red
            if r > 200 and g > 200 and b < 50: return 2  # Yellow
            if r < 50 and g > 200 and b < 50: return 3  # Green
            if r < 50 and g > 200 and b > 200: return 4  # Cyan
            if r < 50 and g < 50 and b > 200: return 5  # Blue
            if r > 200 and g < 50 and b > 200: return 6  # Magenta
            if r > 200 and g > 200 and b > 200: return 7  # White
            if abs(r-g) < 30 and abs(g-b) < 30 and r > 100 and r < 200: return 8  # Gray
            return 7  # Default white
        
        for layer_name, layer_info in self.parser.layers.items():
            color = layer_info['color']
            lw = layer_info['lineweight']
            aci = rgb_to_aci(color[0], color[1], color[2])
            # Use a single combined LAYER command for robustness
            self.code.append(f"  (command \"._-LAYER\" \"_M\" \"{layer_name}\" \"_C\" \"{aci}\" \"\" \"_LW\" \"{lw}\" \"\" \"\")")
        
        self.code.append("")

    
    def _generate_sketches(self):
        """生成 sketch (polyline) 代码 - 使用 entmake 创建 LWPOLYLINE with bulge support"""
        for sketch in self.parser.sketches:
            self.code.append(f"  ; Sketch: {sketch['name']}")
            self.code.append(f"  (command \"._-LAYER\" \"_S\" \"{sketch['layer']}\" \"\")")
            
            for polyline in sketch['polylines']:
                # Use unique prefix for each polyline
                var_prefix = f"{sketch['name']}_{polyline['name']}"
                
                # Collect point data (coord + radius)
                point_data = []
                for pt in polyline['points']:
                    # Handle both old format (string) and new format (dict)
                    if isinstance(pt, dict):
                        coord_str = pt['coord']
                        radius = pt.get('radius')
                    else:
                        coord_str = pt
                        radius = None
                    
                    if coord_str.strip().startswith('//'):
                        continue
                    x_expr, y_expr = self._parse_point_expr(coord_str)
                    # Handle 't' variable specially
                    if 't' in x_expr and x_expr != 't':
                        x_expr = x_expr.replace(' t ', ' t_param ').replace('(t)', '(t_param)')
                    if 't' in y_expr and y_expr != 't':
                        y_expr = y_expr.replace(' t ', ' t_param ').replace('(t)', '(t_param)')
                    if x_expr == 't':
                        x_expr = 't_param'
                    if y_expr == 't':
                        y_expr = 't_param'
                    
                    # Convert radius expression if present
                    radius_lisp = self._convert_expr_to_lisp(radius) if radius else None
                    point_data.append({'x': x_expr, 'y': y_expr, 'radius': radius_lisp})
                
                if point_data:
                    # Build LWPOLYLINE using entmake with bulge support
                    closed_flag = 1 if polyline['closed'] else 0
                    n_pts = len(point_data)
                    
                    self.code.append(f"  ; Create LWPOLYLINE for {var_prefix} with arc support")
                    
                    # Build points and bulges lists
                    self.code.append(f"  (setq pts_{var_prefix} (list")
                    for pd in point_data:
                        self.code.append(f"    (list {pd['x']} {pd['y']})")
                    self.code.append(f"  ))")
                    
                    # Build bulges list - bulge is applied to vertex BEFORE the arc
                    # For a corner at vertex i with radius R, the bulge at vertex (i-1) defines the arc
                    self.code.append(f"  (setq bulges_{var_prefix} (list")
                    for i, pd in enumerate(point_data):
                        # Check if NEXT vertex has a radius (meaning arc from current to next)
                        next_idx = (i + 1) % n_pts
                        if next_idx < n_pts or polyline['closed']:
                            next_pd = point_data[next_idx] if next_idx < n_pts else point_data[0]
                            if next_pd['radius']:
                                # Bulge will be computed at runtime
                                self.code.append(f"    'CALC  ; Arc to vertex {next_idx}")
                            else:
                                self.code.append(f"    0.0")
                        else:
                            self.code.append(f"    0.0")
                    self.code.append(f"  ))")
                    
                    # Generate bulge calculation function call
                    self.code.append(f"  (setq bulges_{var_prefix} (pcad-calc-bulges pts_{var_prefix} (list")
                    for pd in point_data:
                        if pd['radius']:
                            self.code.append(f"    {pd['radius']}")
                        else:
                            self.code.append(f"    0.0")
                    self.code.append(f"  ) {'T' if polyline['closed'] else 'nil'}))")
                    
                    # Use entmake to create LWPOLYLINE
                    self.code.append(f"  (setq ent_data_{var_prefix} (list")
                    self.code.append(f"    '(0 . \"LWPOLYLINE\")")
                    self.code.append(f"    '(100 . \"AcDbEntity\")")
                    self.code.append(f"    (cons 8 \"{sketch['layer']}\")")
                    self.code.append(f"    '(100 . \"AcDbPolyline\")")
                    self.code.append(f"    (cons 90 {n_pts})")
                    self.code.append(f"    (cons 70 {closed_flag})")
                    self.code.append(f"  ))")
                    
                    # Add vertices with bulge values
                    self.code.append(f"  (setq i 0)")
                    self.code.append(f"  (foreach pt pts_{var_prefix}")
                    self.code.append(f"    (setq b (nth i bulges_{var_prefix}))")
                    self.code.append(f"    (setq ent_data_{var_prefix}")
                    self.code.append(f"      (append ent_data_{var_prefix}")
                    self.code.append(f"        (list (cons 10 (list (car pt) (cadr pt) 0.0))")
                    self.code.append(f"              (cons 42 b))")
                    self.code.append(f"      )")
                    self.code.append(f"    )")
                    self.code.append(f"    (setq i (1+ i))")
                    self.code.append(f"  )")
                    
                    # Create the entity
                    self.code.append(f"  (entmake ent_data_{var_prefix})")
                    
                    # Save entity reference
                    ent_var_name = f"sketch_{sketch['name']}_{polyline['name']}"
                    self.code.append(f"  (setq {ent_var_name} (entlast))")
            
            # Generate code for individual lines in the sketch
            if 'lines' in sketch:
                for line in sketch['lines']:
                    p1_x, p1_y = self._parse_point_expr(line['p1'])
                    p2_x, p2_y = self._parse_point_expr(line['p2'])
                    self.code.append(f"  (command \"._LINE\" (list {p1_x} {p1_y} 0.0) (list {p2_x} {p2_y} 0.0) \"\")")
            
            # Generate code for circles in the sketch
            if 'circles' in sketch:
                for circle in sketch['circles']:
                    cx, cy = self._parse_point_expr(circle['center'])
                    radius_expr = self._convert_expr_to_lisp(circle['radius'])
                    self.code.append(f"  ; Circle: {circle['name']}")
                    self.code.append(f"  (command \"._CIRCLE\" (list {cx} {cy} 0.0) {radius_expr})")
                    # Save entity reference
                    ent_var_name = f"sketch_{sketch['name']}_{circle['name']}"
                    self.code.append(f"  (setq {ent_var_name} (entlast))")
            
            self.code.append("")

    
    def _parse_point_expr(self, point_expr: str) -> Tuple[str, str]:
        """解析点表达式，返回 (x_expr, y_expr)"""
        # Remove any comments
        point_expr = re.sub(r'//.*$', '', point_expr).strip()
        
        # Handle expressions like "0, 0" or "B, 0" or "(B - W) / 2, t"
        # Split by comma, but be careful with parentheses
        parts = []
        current = ""
        depth = 0
        
        for char in point_expr:
            if char == '(':
                depth += 1
                current += char
            elif char == ')':
                depth -= 1
                current += char
            elif char == ',' and depth == 0:
                parts.append(current.strip())
                current = ""
            else:
                current += char
        
        if current:
            parts.append(current.strip())
        
        if len(parts) == 2:
            x_expr = self._convert_expr_to_lisp(parts[0])
            y_expr = self._convert_expr_to_lisp(parts[1])
            return x_expr, y_expr
        elif len(parts) == 1:
            # Single value, assume it's x, y is 0
            x_expr = self._convert_expr_to_lisp(parts[0])
            return x_expr, "0.0"
        
        return "0.0", "0.0"
    
    def _generate_regions(self):
        """生成 region (hatch) 代码"""
        for region in self.parser.regions:
            self.code.append(f"  ; Region: {region['name']}")
            
            # Find the sketch for this region
            sketch_name = region['boundary']['sketch']
            sketch = next((s for s in self.parser.sketches if s['name'] == sketch_name), None)
            
            if sketch:
                self.code.append(f"  (command \"._-LAYER\" \"_S\" \"hatch\" \"\")")
                
                # Get hatch style
                hatch_style_name = region.get('hatch', 'concrete')
                hatch_style = self.parser.hatch_styles.get(hatch_style_name, {})
                pattern = hatch_style.get('pattern', 'ANSI37')
                scale = getattr(self, 'estimated_scale', HATCH_SCALE)
                angle = hatch_style.get('angle', 0)
                
                # Create hatch from last entity (the polyline)
                # Map pattern names to more universally available patterns
                # ANSI31 (diagonal lines) is available in all AutoCAD versions
                pattern_map = {
                    'ANSI37': 'ANSI31',  # ANSI37 (crosshatch) -> ANSI31 (diagonal lines)
                    'ANSI31': 'ANSI31',
                    'SOLID': 'SOLID',
                    'CONCRETE': 'ANSI31'  # Concrete typically uses diagonal lines
                }
                pattern_upper = pattern.upper()
                # Use mapped pattern or fallback to ANSI31 (most universal)
                mapped_pattern = pattern_map.get(pattern_upper, 'ANSI31')
                
                # ANSI patterns have built-in angles (ANSI31 is 45°), so reset angle to 0
                if mapped_pattern.startswith('ANSI'):
                    angle = 0
                
                self.code.append(f"  ; Create hatch from sketch: {sketch_name}")
                
                # Try to use specific polyline if specified in region, else use the last one in the sketch
                # Region definition: boundary = Sketch.Polyline;
                # Parser stores: boundary = {'sketch': 'Sketch', 'polyline': 'Polyline'}
                polyline_name = region['boundary']['polyline']
                ent_var_name = f"sketch_{sketch_name}_{polyline_name}"
                
                # Check if this entity variable was created?
                # For safety, we can wrap in 'if' check in LISP or assumes it exists
                # Or fallback to entlast if using old logic? NO, old logic is broken.
                
                # HATCH command: Use -BHATCH for better compatibility
                # Set hatch pattern properties via system variables first, then apply
                self.code.append(f"  (if (boundp '{ent_var_name})")
                self.code.append(f"    (progn")
                self.code.append(f"      (setvar \"HPNAME\" \"{mapped_pattern}\")")
                self.code.append(f"      (setvar \"HPSCALE\" {scale})")
                self.code.append(f"      (setvar \"HPANG\" (* {angle} (/ pi 180.0)))")
                self.code.append(f"      (command \"._-BHATCH\" \"_S\" {ent_var_name} \"\" \"\")")
                self.code.append(f"    )")
                self.code.append(f"    (princ \"\\nWarning: Boundary entity {ent_var_name} not found for region {region['name']}\\n\")")
                self.code.append("  )")
            
            self.code.append("")
    
    def _generate_dimensions(self):
        """生成尺寸标注代码 - supports linear, vertical, radial, diameter"""
        for dim in self.parser.dimensions:
            dim_type = dim['type']
            
            self.code.append(f"  (command \"._-LAYER\" \"_S\" \"dim\" \"\")")
            
            if dim_type in ['linear', 'vertical', 'horizontal']:
                from_expr = dim['from']
                to_expr = dim['to']
                text = dim.get('text')
                
                from_x, from_y = self._parse_point_expr(from_expr)
                to_x, to_y = self._parse_point_expr(to_expr)
                
                self.code.append(f"  ; Dimension: {text or 'auto'}")
                self.code.append(f"  (setq p1 (list {from_x} {from_y} 0.0))")
                self.code.append(f"  (setq p2 (list {to_x} {to_y} 0.0))")
                
                if dim_type == 'linear':
                    self.code.append(f"  (setq mid_x (/ (+ {from_x} {to_x}) 2.0))")
                    self.code.append(f"  (setq mid_y (/ (+ {from_y} {to_y}) 2.0))")
                    self.code.append(f"  (setq p3 (list mid_x (+ mid_y 100) 0.0))")
                    self.code.append(f"  (command \"._DIMLINEAR\" p1 p2 p3)")
                elif dim_type == 'vertical':
                    self.code.append(f"  (setq mid_x (/ (+ {from_x} {to_x}) 2.0))")
                    self.code.append(f"  (setq mid_y (/ (+ {from_y} {to_y}) 2.0))")
                    self.code.append(f"  (setq p3 (list (+ mid_x 100) mid_y 0.0))")
                    self.code.append(f"  (command \"._DIMLINEAR\" p1 p2 p3)")
                else:
                    self.code.append(f"  (setq p3 (list (+ {from_x} 50) (+ {from_y} 50) 0.0))")
                    self.code.append(f"  (command \"._DIMALIGNED\" p1 p2 p3)")
                
                self.code.append(f"  (setq ent (entlast))")
                if text:
                    self.code.append(f"  (command \"._DIMEDIT\" \"_N\" \"{text}\" ent \"\")")
            
            elif dim_type == 'radial':
                center_expr = dim['center']
                radius_expr = dim['radius']
                angle_expr = dim.get('angle', '45')
                text = dim.get('text')
                
                center_x, center_y = self._parse_point_expr(center_expr)
                radius_lisp = self._convert_expr_to_lisp(radius_expr)
                angle_lisp = self._convert_expr_to_lisp(angle_expr)
                
                # Use MTEXT-based leader annotation for radial dimensions
                # This avoids creating a temporary circle that may not render correctly
                self.code.append(f"  ; Radial Dimension: {text or 'R'}")
                self.code.append(f"  (setq center_pt (list {center_x} {center_y} 0.0))")
                self.code.append(f"  (setq rad_val {radius_lisp})")
                self.code.append(f"  (setq ang_rad (* {angle_lisp} (/ pi 180.0)))")
                self.code.append(f"  ; Calculate arc point on the circle at specified angle")
                self.code.append(f"  (setq arc_pt (list (+ {center_x} (* rad_val (cos ang_rad)))")
                self.code.append(f"                     (+ {center_y} (* rad_val (sin ang_rad))) 0.0))")
                self.code.append(f"  ; Calculate leader endpoint (outside the arc)")
                self.code.append(f"  (setq leader_pt (list (+ {center_x} (* (* rad_val 1.3) (cos ang_rad)))")
                self.code.append(f"                        (+ {center_y} (* (* rad_val 1.3) (sin ang_rad))) 0.0))")
                self.code.append(f"  ; Draw leader line from arc point to text location")
                self.code.append(f"  (command \"._LINE\" arc_pt leader_pt \"\")")
                # Add text at leader end
                dim_text = text if text else f"R{{{radius_lisp}}}"
                self.code.append(f"  (command \"._MTEXT\" leader_pt \"_J\" \"_ML\" \"_H\" (* (getvar \"DIMSCALE\") {DIM_TEXT_HEIGHT}) \"_W\" 0 \"{dim_text}\" \"\")")

            
            elif dim_type == 'diameter':
                center_expr = dim['center']
                diameter_expr = dim['diameter']
                angle_expr = dim.get('angle', '45')
                text = dim.get('text')
                
                center_x, center_y = self._parse_point_expr(center_expr)
                diameter_lisp = self._convert_expr_to_lisp(diameter_expr)
                angle_lisp = self._convert_expr_to_lisp(angle_expr)
                
                self.code.append(f"  ; Diameter Dimension: {text or 'D'}")
                self.code.append(f"  (setq center_pt (list {center_x} {center_y} 0.0))")
                self.code.append(f"  (setq dia_val {diameter_lisp})")
                self.code.append(f"  (setq rad_val (/ dia_val 2.0))")
                self.code.append(f"  (setq ang_rad (* {angle_lisp} (/ pi 180.0)))")
                self.code.append(f"  (setq arc_pt (list (+ {center_x} (* rad_val (cos ang_rad)))")
                self.code.append(f"                     (+ {center_y} (* rad_val (sin ang_rad))) 0.0))")
                self.code.append(f"  (setq leader_pt (list (+ {center_x} (* (* rad_val 1.5) (cos ang_rad)))")
                self.code.append(f"                        (+ {center_y} (* (* rad_val 1.5) (sin ang_rad))) 0.0))")
                self.code.append(f"  (command \"._CIRCLE\" center_pt rad_val)")
                self.code.append(f"  (setq circ_ent (entlast))")
                self.code.append(f"  (command \"._DIMDIAMETER\" arc_pt leader_pt)")
                if text:
                    self.code.append(f"  (setq ent (entlast))")
                    self.code.append(f"  (command \"._DIMEDIT\" \"_N\" \"{text}\" ent \"\")")
                self.code.append(f"  (entdel circ_ent)")
            
            self.code.append("")

    def _generate_bars(self):
        """生成 bars 代码"""
        for bars in self.parser.bars_list:
            self.code.append(f"  ; Bars: {bars['name']}")
            layer = bars.get('layer', 'rebar')
            self.code.append(f"  (command \"._-LAYER\" \"_S\" \"{layer}\" \"\")")
            
            # Handle path if it's a line(...) -> line(...)
            path = bars.get('path', '')
            if 'line(' in path:
                points = []
                for p_match in re.finditer(r'line\(([^)]+)\)', path):
                    pts = self._parse_point_expr(p_match.group(1))
                    points.append(pts)
                
                if len(points) >= 2:
                    p1_x, p1_y = points[0]
                    p2_x, p2_y = points[1]
                    
                    spacing = bars.get('spacing')
                    count = bars.get('count')
                    
                    if spacing or count:
                        # Improved implementation: loop for multiple bars
                        self.code.append(f"  (setq p1_base (list {p1_x} {p1_y} 0.0))")
                        self.code.append(f"  (setq p2_base (list {p2_x} {p2_y} 0.0))")
                        
                        if spacing:
                            spacing_lisp = self._convert_expr_to_lisp(spacing)
                            self.code.append(f"  (setq s {spacing_lisp})")
                            # Decide direction of distribution
                            # If path is vertical, distribute horizontally
                            # If path is horizontal, distribute vertically
                            self.code.append(f"  (setq dx 0.0 dy 0.0)")
                            self.code.append(f"  (if (< (abs (- {p1_x} {p2_x})) 0.1) (setq dx s) (setq dy s))")
                            
                            # Calculate number of repetitions
                            if count:
                                num_lisp = str(count)
                            else:
                                # Heuristic: if spacing is (L-Cover)/20, maybe count is 21?
                                # Let's assume count is 20 if spacing formula has /20
                                if '/20' in str(spacing):
                                    num_lisp = "21"
                                else:
                                    num_lisp = "10" # Default
                                    
                            self.code.append(f"  (repeat {num_lisp}")
                            self.code.append("    (command \"._LINE\" p1_base p2_base \"\")")
                            self.code.append("    (setq p1_base (list (+ (car p1_base) dx) (+ (cadr p1_base) dy) 0.0))")
                            self.code.append("    (setq p2_base (list (+ (car p2_base) dx) (+ (cadr p2_base) dy) 0.0))")
                            self.code.append("  )")
                        elif count:
                            self.code.append(f"  (repeat {count}")
                            self.code.append("    (command \"._LINE\" p1_base p2_base \"\")")
                            # How to offset without spacing? Default 100
                            self.code.append("    (setq p1_base (list (car p1_base) (+ (cadr p1_base) 100.0) 0.0))")
                            self.code.append("    (setq p2_base (list (car p2_base) (+ (cadr p2_base) 100.0) 0.0))")
                            self.code.append("  )")
                    else:
                        self.code.append(f"  (setq p1 (list {p1_x} {p1_y} 0.0))")
                        self.code.append(f"  (setq p2 (list {p2_x} {p2_y} 0.0))")
                        self.code.append("  (command \"._LINE\" p1 p2 \"\")")
    
    def _generate_barshapes(self):
        """生成钢筋大样代码 - generates AutoLISP functions to draw bar shapes with scaling support"""
        if not self.parser.barshapes:
            return
        
        self.code.append("  ; Bar shapes (rebar detail drawings)")
        self.code.append("  ; Each barshape generates a function: (draw-barshape-<Name> base_pt scale)")
        self.code.append("  ; base_pt is a list (x y z), scale is a number (1.0 = full size)")
        self.code.append("  ; For table cells, use scale 0.05-0.1 to fit shapes in cells")
        self.code.append("")
        
        # Generate a drawing function for each barshape
        for name, shape in self.parser.barshapes.items():
            shape_type = shape.get('type', 'custom')
            segments = shape.get('segments', '')
            dims = shape.get('dims', {})
            hooks = shape.get('hooks', {})
            
            self.code.append(f"  ; --- Barshape: {name} (type: {shape_type}) ---")
            
            # Parse segments if available
            if segments:
                # Parse segment string: "(x1, y1):r=R -> (x2, y2) -> ..."
                parsed_segments = self._parse_segments_to_points(segments)
                
                # Validate variable references in segment expressions
                all_defined_vars = set(self.parser.params.keys()) | set(self.parser.derive.keys()) | set(dims.keys())

                for seg in parsed_segments:
                    if seg['point']:
                        for expr in seg['point']:
                            var_names = re.findall(r'\b([A-Za-z_][A-Za-z0-9_]*)\b', str(expr))
                            for var in var_names:
                                if var not in all_defined_vars and not var.replace('.', '').isdigit():
                                    if var not in ['pi', 'sin', 'cos', 'tan', 'sqrt', 'abs']:
                                        print(f"WARNING: Barshape '{name}' uses undefined variable '{var}' in segment expression '{expr}'")
                                        print(f"         Ensure '{var}' is defined in 'params', 'derive', or local 'dims'.")
                
                if parsed_segments:
                    self.code.append(f"  ; Dims: {dims}")
                    
                    # Check if any segments have fillet radius
                    has_fillets = any(seg.get('radius') for seg in parsed_segments)
                    
                    # Prepare local variables string for defun
                    # Rename 'r' to 'seg_rad' to avoid shadowing global 'R' parameter
                    local_vars_list = ['pt', 'x', 'y', 'sc', 'prev_x', 'prev_y', 'seg_rad']
                    local_vars_list.extend(dims.keys())
                    local_vars_str = " ".join(local_vars_list)
                    
                    self.code.append(f"  (defun draw-barshape-{name} (base_pt scale / {local_vars_str})")
                    self.code.append("    (setq sc (if scale scale 1.0))")

                    # Initialize local dims
                    if dims:
                        self.code.append(f"    ; Initialize local dims")
                        for d_name, d_expr in dims.items():
                            lisp_expr = self._convert_expr_to_lisp(d_expr)
                            self.code.append(f"    (setq {d_name} {lisp_expr})")

                    self.code.append("    (setq pts_list '()")
                    self.code.append("          rad_list '())")
                    
                    for i, seg in enumerate(parsed_segments):
                        pt = seg['point']
                        radius = seg.get('radius') or '0.0'
                        x_expr = self._convert_expr_to_lisp(pt[0])
                        y_expr = self._convert_expr_to_lisp(pt[1])
                        r_expr = self._convert_expr_to_lisp(radius)
                        
                        self.code.append(f"    (setq x (+ (car base_pt) (* sc {x_expr})))")
                        self.code.append(f"    (setq y (+ (cadr base_pt) (* sc {y_expr})))")
                        self.code.append(f"    (setq pts_list (append pts_list (list (list x y))))")
                        self.code.append(f"    (setq rad_list (append rad_list (list (* sc {r_expr}))))")
                    
                    # Check for closed loop: if first and last points are roughly the same
                    first_pt = parsed_segments[0]['point']
                    last_pt = parsed_segments[-1]['point']
                    # Using string comparison of expressions for simplicity, assuming normalized
                    is_closed = (first_pt[0] == last_pt[0] and first_pt[1] == last_pt[1])
                    closed_val = "T" if is_closed else "nil"
                    
                    self.code.append(f"    (pcad-draw-pline-with-fillets pts_list rad_list {closed_val})")
                    
                    # Implement hook drawing logic (P-CAD v1.1 normative)
                    if hooks:
                        self.code.append(f"    ; Drawing hooks for {name}")
                        # Get last segment end point for end hook
                        if len(parsed_segments) >= 2:
                            # End hook: based on direction from second-to-last to last point
                            if 'end' in hooks:
                                end_hook = hooks['end']
                                angle = end_hook.get('angle', '90')
                                length = end_hook.get('length', '50')
                                angle_lisp = self._convert_expr_to_lisp(angle)
                                length_lisp = self._convert_expr_to_lisp(length)
                                self.code.append(f"    ; End hook: angle={angle}, length={length}")
                                self.code.append(f"    (setq hook_angle (+ (angle (list 0 0) (list x y)) (* {angle_lisp} (/ pi 180.0))))")
                                self.code.append(f"    (setq hook_end_x (+ x (* sc {length_lisp} (cos hook_angle))))")
                                self.code.append(f"    (setq hook_end_y (+ y (* sc {length_lisp} (sin hook_angle))))")
                                self.code.append(f"    (command \"._LINE\" (list x y) (list hook_end_x hook_end_y) \"\")")
                            # Start hook
                            if 'start' in hooks:
                                start_hook = hooks['start']
                                angle = start_hook.get('angle', '90')
                                length = start_hook.get('length', '50')
                                angle_lisp = self._convert_expr_to_lisp(angle)
                                length_lisp = self._convert_expr_to_lisp(length)
                                first_seg = parsed_segments[0]
                                fx_expr = self._convert_expr_to_lisp(first_seg['point'][0])
                                fy_expr = self._convert_expr_to_lisp(first_seg['point'][1])
                                self.code.append(f"    ; Start hook: angle={angle}, length={length}")
                                self.code.append(f"    (setq fx (+ (car base_pt) (* sc {fx_expr})))")
                                self.code.append(f"    (setq fy (+ (cadr base_pt) (* sc {fy_expr})))")
                                self.code.append(f"    (setq hook_angle (+ (angle (list 0 0) (list fx fy)) (* {angle_lisp} (/ pi 180.0))))")
                                self.code.append(f"    (setq hook_start_x (+ fx (* sc {length_lisp} (cos hook_angle))))")
                                self.code.append(f"    (setq hook_start_y (+ fy (* sc {length_lisp} (sin hook_angle))))")
                                self.code.append(f"    (command \"._LINE\" (list fx fy) (list hook_start_x hook_start_y) \"\")")
                    
                    self.code.append(f"  )")
            else:
                self.code.append(f"  ; (No segment data for {name})")
            
            self.code.append("")
        
        self.code.append("")


    def _generate_labels(self):
        """生成标签文字代码"""
        if not self.parser.labels:
            return
        
        self.code.append("  ; --- Labels ---")
        for label in self.parser.labels:
            text = label['text']
            x_expr, y_expr = label['at']
            layer = label.get('layer', 'text')
            
            x_lisp = self._convert_expr_to_lisp(x_expr)
            y_lisp = self._convert_expr_to_lisp(y_expr)
            
            self.code.append(f"  ; Label: {text}")
            self.code.append(f"  (command \"._-LAYER\" \"_S\" \"{layer}\" \"\")")
            self.code.append(f"  (setq lbl_pt (list {x_lisp} {y_lisp} 0.0))")
            # Use MTEXT with middle center justification, applying DIMSCALE to height
            self.code.append(f"  (command \"._MTEXT\" lbl_pt \"_J\" \"_MC\" \"_H\" (* (getvar \"DIMSCALE\") {LABEL_TEXT_HEIGHT}) \"_W\" 0 \"{text}\" \"\")")
        
        self.code.append("")

    def _generate_utility_functions(self):
        """生成渲染所需的通用 AutoLISP 函数"""
        self.code.append("  ; --- Utility Functions for Rendering ---")
        self.code.append("  (defun draw-table-cell (pt w h txt / p1 p2 p3 p4 cp)")
        self.code.append("    (setq p1 pt")
        self.code.append("          p2 (list (+ (car pt) w) (cadr pt) 0.0)")
        self.code.append("          p3 (list (+ (car pt) w) (- (cadr pt) h) 0.0)")
        self.code.append("          p4 (list (car pt) (- (cadr pt) h) 0.0)")
        self.code.append("          cp (list (+ (car pt) (* 0.5 w)) (- (cadr pt) (* 0.5 h)) 0.0)")
        self.code.append("    )")
        self.code.append("    (command \"._-LAYER\" \"_S\" \"outline\" \"\")")
        self.code.append("    (command \"._PLINE\" p1 p2 p3 p4 \"_C\")")
        self.code.append("    (if (and txt (/= txt \"\"))")
        self.code.append("      (progn")
        self.code.append("        (command \"._-LAYER\" \"_S\" \"text\" \"\")")
        # Table cell text height from global config, applying DIMSCALE
        self.code.append(f"        (command \"._MTEXT\" cp \"_J\" \"_MC\" \"_H\" (* (getvar \"DIMSCALE\") {TABLE_CELL_TEXT_HEIGHT}) \"_W\" (* 0.9 w) txt \"\")")
        self.code.append("      )")
        self.code.append("    )")
        self.code.append("  )")
        self.code.append("")
        self.code.append("  (defun pcad-smart-dim-arc (ent / obj midPt)")
        self.code.append("    (vl-load-com)")
        self.code.append("    (if (and ent (wcmatch (cdr (assoc 0 (entget ent))) \"ARC,CIRCLE\"))")
        self.code.append("      (progn")
        self.code.append("        (command \"._-LAYER\" \"_S\" \"dim\" \"\")")
        self.code.append("        (setq obj (vlax-ename->vla-object ent))")
        self.code.append("        (setq midPt (vlax-curve-getPointAtParam obj ")
        self.code.append("                      (/ (+ (vlax-curve-getStartParam obj) ")
        self.code.append("                            (vlax-curve-getEndParam obj)) ")
        self.code.append("                         2.0)")
        self.code.append("                    )")
        self.code.append("        )")
        self.code.append("        (command \"._DIMRADIUS\" (list ent midPt) midPt)")
        self.code.append("      )")
        self.code.append("    )")
        self.code.append("  )")
        self.code.append("")
        
        self.code.append("  ; pcad-draw-pline-with-fillets: Draws rebar shape with fillets using FILLET command")
        self.code.append("  ; pts: list of (x y) points")
        self.code.append("  ; radii: list of radii (same length as pts, use 0 for no fillet)")
        self.code.append("  ; closed: T or nil")
        self.code.append("  ; Approach: Draw LINE segments, store entities, then FILLET using entity-point pairs")
        self.code.append("  (defun pcad-draw-pline-with-fillets (pts radii closed / i n r p0 p1 p2 lines ent1 ent2 sel1 sel2)")
        self.code.append("    (command \"._-LAYER\" \"_S\" \"rebar\" \"\")")
        self.code.append("    (setq n (length pts))")
        self.code.append("    (setq lines '())")
        self.code.append("    ")
        self.code.append("    ; Step 1: Draw all LINE segments and store entity names")
        self.code.append("    (setq i 0)")
        self.code.append("    (while (< i (1- n))")
        self.code.append("      (setq p0 (nth i pts))")
        self.code.append("      (setq p1 (nth (1+ i) pts))")
        self.code.append("      (command \"._LINE\" p0 p1 \"\")")
        self.code.append("      (setq lines (append lines (list (entlast))))")
        self.code.append("      (setq i (1+ i))")
        self.code.append("    )")
        self.code.append("    ")
        self.code.append("    ; Step 2: Apply FILLET at each corner with non-zero radius")
        self.code.append("    (setq i 1)")
        self.code.append("    (while (< i (1- n))")
        self.code.append("      (setq r (nth i radii))")
        self.code.append("      (if (and r (> r 0.0))")
        self.code.append("        (progn")
        self.code.append("          ; Get the two LINE entities adjacent to this corner")
        self.code.append("          (setq ent1 (nth (1- i) lines))  ; Line before corner")
        self.code.append("          (setq ent2 (nth i lines))       ; Line after corner")
        self.code.append("          ; Get the adjacent points for calculating midpoints")
        self.code.append("          (setq p0 (nth (1- i) pts))  ; Point before corner")
        self.code.append("          (setq p1 (nth i pts))       ; The corner point")
        self.code.append("          (setq p2 (nth (1+ i) pts))  ; Point after corner")
        self.code.append("          ; Calculate midpoints of each segment for pick points")
        self.code.append("          (setq mid1 (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p0 p1))")
        self.code.append("          (setq mid2 (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p1 p2))")
        self.code.append("          ; Set fillet radius first")
        self.code.append("          (command \"._FILLET\" \"_R\" r)")
        self.code.append("          ; Apply fillet using entity-point pairs with midpoints")
        self.code.append("          (command \"._FILLET\" (list ent1 mid1) (list ent2 mid2))")
        self.code.append("          (pcad-smart-dim-arc (entlast))")
        self.code.append("        )")
        self.code.append("      )")
        self.code.append("      (setq i (1+ i))")
        self.code.append("    )")
        self.code.append("    ")
        self.code.append("    ; Step 3: Handle closing corner if closed")
        self.code.append("    (if closed")
        self.code.append("      (progn")
        self.code.append("        (setq r (nth (1- n) radii)) ; Radius at the last point (which is same as first)")
        self.code.append("        (if (and r (> r 0.0))")
        self.code.append("          (progn")
        self.code.append("            ; Fillet between last segment and first segment")
        self.code.append("            (setq ent1 (last lines))      ; Last drawn line")
        self.code.append("            (setq ent2 (car lines))       ; First drawn line")
        self.code.append("            ")
        self.code.append("            ; Points regarding the closing corner (last pt = first pt)")
        self.code.append("            (setq p0 (nth (- n 2) pts))   ; Penultimate point")
        self.code.append("            (setq p1 (nth (1- n) pts))    ; Last point (corner)")
        self.code.append("            (setq p2 (nth 1 pts))         ; Second point (after corner)")
        self.code.append("            ")
        self.code.append("            ; Calculate midpoints")
        self.code.append("            (setq mid1 (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p0 p1))")
        self.code.append("            (setq mid2 (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p1 p2))")
        self.code.append("            ")
        self.code.append("            (command \"._FILLET\" \"_R\" r)")
        self.code.append("            (command \"._FILLET\" (list ent1 mid1) (list ent2 mid2))")
        self.code.append("            (pcad-smart-dim-arc (entlast))")
        self.code.append("          )")
        self.code.append("        )")
        self.code.append("      )")
        self.code.append("    )")
        self.code.append("  )")
        self.code.append("")
        
        # Add pcad-calc-bulges function for LWPOLYLINE arc support
        self.code.append("  ; pcad-calc-bulges: Calculate bulge values for LWPOLYLINE arc segments")
        self.code.append("  ; pts: list of (x y) points")
        self.code.append("  ; radii: list of radii (same length as pts, 0 for no arc)")
        self.code.append("  ; closed: T or nil")
        self.code.append("  ;")
        self.code.append("  ; CRITICAL: radius at vertex i means arc FROM vertex (i-1) TO vertex i")
        self.code.append("  ; The bulge value must be assigned to vertex (i-1), the ARC START")
        self.code.append("  ;")
        self.code.append("  ; Bulge = sagitta / half_chord, where:")
        self.code.append("  ;   chord = distance between arc start and arc end")
        self.code.append("  ;   sagitta = R - sqrt(R² - half_chord²)")
        self.code.append("  ;")
        self.code.append("  (defun pcad-calc-bulges (pts radii closed / i n bulges r next_r p0 p1 chord half_chord sagitta bulge)")
        self.code.append("    (setq n (length pts))")
        self.code.append("    ; Initialize all bulges to 0")
        self.code.append("    (setq bulges '())")
        self.code.append("    (repeat n (setq bulges (append bulges (list 0.0))))")
        self.code.append("    ")
        self.code.append("    ; For each vertex i, check if the NEXT vertex (i+1) has a radius")
        self.code.append("    ; If yes, that defines an arc from i to i+1, so bulge goes at vertex i")
        self.code.append("    (setq i 0)")
        self.code.append("    (while (< i n)")
        self.code.append("      (setq next_idx (1+ i))")
        self.code.append("      (if (>= next_idx n)")
        self.code.append("        (if closed (setq next_idx 0) (setq next_idx nil))")
        self.code.append("      )")
        self.code.append("      (if next_idx")
        self.code.append("        (progn")
        self.code.append("          (setq next_r (nth next_idx radii))")
        self.code.append("          ; Check if radius is non-zero (can be negative for CW arc)")
        self.code.append("          (if (and next_r (/= next_r 0.0))")
        self.code.append("            (progn")
        self.code.append("              (setq abs_r (abs next_r))")
        self.code.append("              (setq is_cw (< next_r 0))")
        self.code.append("              ")
        self.code.append("              ; Arc from vertex i to vertex next_idx")
        self.code.append("              (setq p0 (nth i pts))")
        self.code.append("              (setq p1 (nth next_idx pts))")
        self.code.append("              (setq chord (distance p0 p1))")
        self.code.append("              (if (> chord 0.0)")
        self.code.append("                (progn")
        self.code.append("                  (setq half_chord (/ chord 2.0))")
        self.code.append("                  (if (>= abs_r half_chord)")
        self.code.append("                    (progn")
        self.code.append("                      (setq sagitta (- abs_r (sqrt (- (* abs_r abs_r) (* half_chord half_chord)))))")
        self.code.append("                      (setq bulge (/ sagitta half_chord))")
        self.code.append("                      ; If neg radius (CW), negate the bulge")
        self.code.append("                      (if is_cw (setq bulge (- bulge)))")
        self.code.append("                      ")
        self.code.append("                      ; Assign bulge to vertex i (arc start)")
        self.code.append("                      (setq bulges (subst-nth i bulge bulges))")
        self.code.append("                    )")
        self.code.append("                  )")
        self.code.append("                )")
        self.code.append("              )")
        self.code.append("            )")
        self.code.append("          )")
        self.code.append("        )")
        self.code.append("      )")
        self.code.append("      (setq i (1+ i))")
        self.code.append("    )")
        self.code.append("    bulges")
        self.code.append("  )")
        self.code.append("")
        self.code.append("  ; Helper: substitute nth element in list")
        self.code.append("  (defun subst-nth (idx val lst / i result)")
        self.code.append("    (setq result '())")
        self.code.append("    (setq i 0)")
        self.code.append("    (foreach item lst")
        self.code.append("      (if (= i idx)")
        self.code.append("        (setq result (append result (list val)))")
        self.code.append("        (setq result (append result (list item)))")
        self.code.append("      )")
        self.code.append("      (setq i (1+ i))")
        self.code.append("    )")
        self.code.append("    result")
        self.code.append("  )")
        self.code.append("")

    def _parse_segments_to_points(self, segments_str: str) -> list:
        """Parse segment string with fillet radius support.
        
        Syntax: '(x1, y1):r=R1 -> (x2, y2) -> (x3, y3):r=R2'
        
        Returns list of segment dicts:
          [{'point': (x, y), 'radius': None or 'R1'}, ...]
        """
        segments = []
        # Split by ->
        parts = segments_str.split('->')
        for part in parts:
            part = part.strip()
            if not part:
                continue
            
            segment = {'point': None, 'radius': None}
            
            # Check for :r=<expr> suffix after the point
            # Pattern: (x, y):r=expr or (x, y):r=expr
            radius_match = re.search(r'\):r=([^\s,;>\]]+)', part)
            if radius_match:
                segment['radius'] = radius_match.group(1).strip()
            
            # Extract (x, y) point
            point_match = re.search(r'\(([^)]+)\)', part)
            if point_match:
                coord_str = point_match.group(1)
                coords = [c.strip() for c in coord_str.split(',')]
                if len(coords) == 2:
                    segment['point'] = (coords[0], coords[1])
                    segments.append(segment)
        
        return segments
    
    def _generate_tables(self):
        """生成表格代码 - generates AutoLISP code to draw tables"""
        if not self.parser.tables:
            return
        
        self.code.append("  ; Tables - Generate data structures and drawing functions")
        
        for name, table in self.parser.tables.items():
            table_type = table.get('type', 'schedule')
            columns = table.get('columns', {})
            rows = table.get('rows', [])
            
            self.code.append(f"  ; --- Table: {name} ---")
            
            # Generate column names list
            col_names = list(columns.keys())
            quoted_cols = [f'"{c}"' for c in col_names]
            cols_str = ' '.join(quoted_cols)
            self.code.append(f"  (setq table_{name}_cols '({cols_str}))")
            
            # Generate rows data as list of lists
            self.code.append(f"  (setq table_{name}_data '(")
            for i, row in enumerate(rows):
                row_values = []
                for col in col_names:
                    val = row.get(col, '')
                    # Escape quotes in value
                    val = str(val).replace('"', '\\"')
                    row_values.append(f'"{val}"')
                self.code.append(f"    ({' '.join(row_values)})")
            self.code.append("  ))")
            
            # Generate a helper function to draw this table
            self.code.append(f"  (defun draw-table-{name} (ins_pt cell_w cell_h / x y col_idx row_data val cell_pt shape_func shape_pt)")
            self.code.append("    (setq x (car ins_pt) y (cadr ins_pt))")
            
            # Header
            self.code.append("    (setq col_idx 0)")
            self.code.append(f"    (foreach col_name table_{name}_cols")
            self.code.append("      (setq cell_pt (list (+ x (* col_idx cell_w)) y 0.0))")
            self.code.append("      (draw-table-cell cell_pt cell_w cell_h col_name)")
            self.code.append("      (setq col_idx (1+ col_idx))")
            self.code.append("    )")
            self.code.append("    (setq y (- y cell_h))")
            
            # Data Rows
            barshape_cols = [idx for idx, (c_name, c_type) in enumerate(columns.items()) if 'barshape_ref' in c_type]
            
            self.code.append(f"    (foreach row_data table_{name}_data")
            self.code.append("      (setq col_idx 0)")
            self.code.append("      (foreach val row_data")
            self.code.append("        (setq cell_pt (list (+ x (* col_idx cell_w)) y 0.0))")
            
            if barshape_cols:
                col_list_str = ' '.join(map(str, barshape_cols))
                self.code.append(f"        (if (member col_idx '({col_list_str}))")
                self.code.append("          (progn")
                self.code.append("            (draw-table-cell cell_pt cell_w cell_h \"\")")
                self.code.append("            (setq shape_func (read (strcat \"draw-barshape-\" val)))")
                self.code.append("            (if (and val (/= val \"\"))")
                # Pass scale 0.05 for table cells
                self.code.append("              ((eval shape_func) cell_pt 0.05)")
                self.code.append("            )")
                self.code.append("            )")

                self.code.append("          )")
                self.code.append("          (draw-table-cell cell_pt cell_w cell_h val)")
                self.code.append("        )")
            else:
                self.code.append("        (draw-table-cell cell_pt cell_w cell_h val)")
                
            self.code.append("        (setq col_idx (1+ col_idx))")
            self.code.append("      )")
            self.code.append("      (setq y (- y cell_h))")
            self.code.append("    )")
            self.code.append("  )")
            self.code.append("")
        
        self.code.append("")
    
    def _generate_sheets(self):
        """生成图纸代码 - generates AutoLISP code for sheet/layout setup"""
        if not self.parser.sheets:
            return
        
        # Paper size constants (in mm)
        paper_sizes = {
            'A4': (210, 297),
            'A3': (420, 297),
            'A2': (594, 420),
            'A1': (841, 594),
            'A0': (1189, 841),
        }
        
        self.code.append("  ; Sheets (layout setup)")
        
        for name, sheet in self.parser.sheets.items():
            size = sheet.get('size', 'A3')
            scale = sheet.get('scale', '1:1')
            
            self.code.append(f"  ; --- Sheet: {name} ---")
            
            # Get paper dimensions
            w, h = paper_sizes.get(size, (420, 297))
            self.code.append(f"  (setq sheet_{name}_width {float(w)})")
            self.code.append(f"  (setq sheet_{name}_height {float(h)})")
            
            # Parse scale (e.g., "1:50" -> 0.02)
            scale_factor = 1.0
            if ':' in scale:
                parts = scale.split(':')
                try:
                    scale_factor = float(parts[0]) / float(parts[1])
                except:
                    scale_factor = 1.0
            self.code.append(f"  (setq sheet_{name}_scale {scale_factor})")
            
            # Titleblock info
            tb = sheet.get('titleblock', {})
            if tb:
                title = tb.get('title', '')
                date = tb.get('date', '')
                self.code.append(f"  (setq sheet_{name}_title \"{title}\")")
                self.code.append(f"  (setq sheet_{name}_date \"{date}\")")
            
            # Placements execution
            placements = sheet.get('placements', [])
            if placements:
                self.code.append(f"  ; Render placements for {name}")
                # Default cell dimensions for tables (larger for barshape columns)
                cell_w = 80.0   # Increased from 40 to accommodate scaled barshapes
                cell_h = 60.0   # Increased from 10 to accommodate scaled barshapes
                current_y = 0.0

                
                for p in placements:
                    ptype = p.get('type')
                    pname = p.get('name')
                    if ptype == 'table':
                        self.code.append(f"  (draw-table-{pname} (list 0.0 {current_y} 0.0) {cell_w} {cell_h})")
                        # Move down for next placement if any
                        table_obj = self.parser.tables.get(pname, {})
                        rows_count = len(table_obj.get('rows', [])) + 1 # +1 for header
                        current_y -= (rows_count * cell_h + 20.0) # Gap of 20
                    elif ptype == 'view':
                        # TODO: Implement view rendering (sketches inside views)
                        self.code.append(f"  ; (View placement {pname} not fully implemented)")
            
            self.code.append("")
        
        self.code.append("")
    
    def _generate_barshape_layouts(self):
        """生成钢筋大样图布局代码 - freeform grid placement with labels and annotations"""
        if not self.parser.barshape_layouts:
            return
        
        self.code.append("  ; Barshape Layouts (freeform grid placement)")
        
        for name, layout in self.parser.barshape_layouts.items():
            title = layout.get('title', '')
            grid = layout.get('grid', (3, 3))
            cell_size = layout.get('cell_size', (100, 150))
            origin = layout.get('origin', (0, 0))
            placements = layout.get('placements', [])
            
            self.code.append(f"  ; --- Barshape Layout: {name} ---")
            
            # Calculate total dimensions
            total_w = grid[0] * cell_size[0]
            total_h = grid[1] * cell_size[1]
            
            # Draw title at top if specified
            if title:
                # Scale offset and height
                title_x = origin[0] + total_w / 2
                self.code.append(f"  (command \"._-LAYER\" \"_S\" \"text\" \"\")")
                self.code.append(f"  (command \"._MTEXT\" (list {title_x} (+ {origin[1]} (* (getvar \"DIMSCALE\") 2.0)) 0.0) \"_J\" \"_MC\" \"_H\" (* (getvar \"DIMSCALE\") {LAYOUT_TITLE_TEXT_HEIGHT}) \"_W\" {total_w} \"{title}\" \"\")")
            
            # Generate drawing function for this layout
            self.code.append(f"  (defun draw-layout-{name} (/ x y cell_w cell_h base_pt label_pt note_pt)")
            self.code.append(f"    (setq cell_w {cell_size[0]})")
            self.code.append(f"    (setq cell_h {cell_size[1]})")
            self.code.append("")
            
            for p in placements:
                shape = p.get('shape', '')
                col = p.get('col', 0)
                row = p.get('row', 0)
                label = p.get('label', '')
                note = p.get('note', '')
                
                # Calculate position for this cell
                # Origin at top-left, Y decreases downward
                # Cell center for shape placement
                self.code.append(f"    ; Placement: {shape} at ({col}, {row})")
                self.code.append(f"    (setq x (+ {origin[0]} (* {col} cell_w) (* 0.5 cell_w)))")
                self.code.append(f"    (setq y (- {origin[1]} (* {row} cell_h) (* 0.5 cell_h)))")
                self.code.append(f"    (setq base_pt (list x (- y 5) 0.0))")
                
                # Draw label above shape
                if label:
                    self.code.append(f"    ; Label: {label}")
                    # Scale offset and height
                    self.code.append(f"    (setq label_pt (list x (+ y (* (getvar \"DIMSCALE\") 5.0)) 0.0))")
                    self.code.append(f"    (command \"._-LAYER\" \"_S\" \"text\" \"\")")
                    self.code.append(f"    (command \"._MTEXT\" label_pt \"_J\" \"_MC\" \"_H\" (* (getvar \"DIMSCALE\") {LAYOUT_LABEL_TEXT_HEIGHT}) \"_W\" (- cell_w 10) \"{label}\" \"\")")
                
                # Draw the barshape (using a larger scale for standalone display)
                if shape and shape in self.parser.barshapes:
                    self.code.append(f"    ; Draw shape: {shape}")
                    # Pass scale 1.0 for layout drawings
                    self.code.append(f"    (draw-barshape-{shape} base_pt 1.0)")
                
                # Draw note below shape
                if note:
                    self.code.append(f"    ; Note: {note}")
                    # Scale offset and height
                    self.code.append(f"    (setq note_pt (list x (- y (* (getvar \"DIMSCALE\") 6.0)) 0.0))")
                    self.code.append(f"    (command \"._-LAYER\" \"_S\" \"text\" \"\")")
                    self.code.append(f"    (command \"._MTEXT\" note_pt \"_J\" \"_MC\" \"_H\" (* (getvar \"DIMSCALE\") {LAYOUT_NOTE_TEXT_HEIGHT}) \"_W\" (- cell_w 10) \"{note}\" \"\")")
                
                # Draw custom annotations
                annotations = p.get('annotations', [])
                if annotations:
                    self.code.append(f"    ; Custom Annotations")
                    for ann in annotations:
                        text = ann.get('text', '')
                        at_x, at_y = ann.get('at', (0, 0))
                        layer = ann.get('layer', 'text')
                        angle = ann.get('angle', 0)
                        
                        if text:
                            # Calculate absolute position relative to cell shape origin
                            self.code.append(f"    (command \"._-LAYER\" \"_S\" \"{layer}\" \"\")")
                            self.code.append(f"    (setq ann_pt (list (+ (car base_pt) {at_x}) (+ (cadr base_pt) {at_y}) 0.0))")
                            # Use Middle Center justification
                            if angle != 0:
                                self.code.append(f"    (command \"._MTEXT\" ann_pt \"_R\" {angle} \"_J\" \"_MC\" \"_H\" (* (getvar \"DIMSCALE\") {ANNOTATION_TEXT_HEIGHT}) \"_W\" 0 \"{text}\" \"\")")
                            else:
                                self.code.append(f"    (command \"._MTEXT\" ann_pt \"_J\" \"_MC\" \"_H\" (* (getvar \"DIMSCALE\") {ANNOTATION_TEXT_HEIGHT}) \"_W\" 0 \"{text}\" \"\")")
                            
                self.code.append("")
            
            self.code.append(f"  )")
            
            # Call the layout function to render
            self.code.append(f"  (draw-layout-{name})")
            self.code.append("")
        
        self.code.append("")
    
    def _add_usage_comments(self):
        """添加使用说明"""
        param_names = list(self.parser.params.keys())
        
        # Generate example values for Method 1
        example_values = []
        for param_name in param_names:
            default = self.parser.params[param_name]['value']
            if isinstance(default, float):
                example_values.append(str(int(default)) if default.is_integer() else str(default))
            else:
                example_values.append(str(default))
        example_str = " ".join(example_values)
        
        self.code.append("; =========================================")
        self.code.append("; Usage Examples:")
        self.code.append("; =========================================")
        self.code.append(";")
        self.code.append("; Method 1: Set parameters then render")
        self.code.append(f";   (set-params {example_str})")
        self.code.append(";   (c:PCAD_Render)")
        self.code.append(";")
        self.code.append("; Method 2: Set parameters individually")
        
        # Generate individual parameter setting examples
        for param_name in param_names:
            default = self.parser.params[param_name]['value']
            if isinstance(default, int):
                default_str = f"{default}.0"
            elif isinstance(default, float):
                default_str = str(default) if '.' in str(default) else f"{default}.0"
            else:
                default_str = str(default)
            
            if param_name == 't':
                self.code.append(f";   (setq t_param {default_str})")
                self.code.append(";   (setq t t_param)  ; Use t_param to avoid conflict with AutoLISP 't'")
            else:
                self.code.append(f";   (setq {param_name} {default_str})")
        
        self.code.append(";   (c:PCAD_Render)")
        self.code.append(";")
        self.code.append("; Method 3: Use default parameters")
        self.code.append(";   (c:PCAD_Render)  ; Uses default values if not set")
        self.code.append("")


def main():
    """主函数"""
    parser_arg = argparse.ArgumentParser(description='P-CAD to AutoLISP Transpiler')
    parser_arg.add_argument('input', help='Input P-CAD file (.pcad)')
    parser_arg.add_argument('output', nargs='?', help='Output AutoLISP file (.lsp). Defaults to input name with .lsp extension.')
    
    args = parser_arg.parse_args()
    
    input_file = Path(args.input)
    if not input_file.exists():
        print(f"Error: File not found: {input_file}")
        sys.exit(1)
    
    output_file = Path(args.output) if args.output else input_file.with_suffix('.lsp')
    
    # Read P-CAD file
    with open(input_file, 'r', encoding='utf-8') as f:
        source = f.read()
    
    # Parse
    print(f"Parsing P-CAD file: {input_file}")
    parser = PCADParser(source)
    parser.parse()
    
    print(f"Found {len(parser.params)} parameters")
    print(f"Found {len(parser.sketches)} sketches")
    print(f"Found {len(parser.regions)} regions")
    print(f"Found {len(parser.dimensions)} dimensions")
    print(f"Found {len(parser.rebar_sets)} rebar sets")
    print(f"Found {len(parser.barshapes)} bar shapes")
    print(f"Found {len(parser.tables)} tables")
    print(f"Found {len(parser.sheets)} sheets")
    # P-CAD DSL v1.0 new constructs
    print(f"Found {len(parser.views)} views")
    print(f"Found {len(parser.notes)} notes blocks")
    print(f"Found {len(parser.meshes)} meshes")
    print(f"Found {len(parser.bars_list)} bars blocks")
    if parser.origin != (0, 0):
        print(f"Found origin: {parser.origin}")
    
    # Generate AutoLISP
    print(f"Generating AutoLISP code...")
    generator = AutoLISPGenerator(parser)
    lisp_code = generator.generate()
    
    # Write output
    with open(output_file, 'w', encoding='utf-8') as f:
        f.write(lisp_code)
    
    print(f"AutoLISP code written to: {output_file}")
    print(f"\nTo use in AutoCAD:")
    print(f"  1. Load: (load \"{str(output_file.absolute()).replace(chr(92), '/')}\")")
    
    # Generate example parameter values
    param_names = list(parser.params.keys())
    example_values = []
    for param_name in param_names:
        default = parser.params[param_name]['value']
        if isinstance(default, float):
            example_values.append(str(int(default)) if default.is_integer() else str(default))
        else:
            example_values.append(str(default))
    example_str = " ".join(example_values)
    print(f"  2. Set params: (set-params {example_str})")
    print(f"  3. Render: (c:PCAD_Render)")


if __name__ == '__main__':
    main()

