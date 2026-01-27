#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
P-CAD to AutoLISP Transpiler
使用 Approach 4: 全局变量参数注入
参数通过全局变量设置，可在调用渲染函数前修改
"""

import re
import sys
from pathlib import Path
from typing import Dict, List, Tuple, Optional, Any


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
        """解析 layers 块"""
        i = start_idx
        i += 1  # Skip 'layers {'
        while i < len(self.lines):
            line = re.sub(r'//.*$', '', self.lines[i]).strip()
            if not line:
                i += 1
                continue
            if line == '}':
                break
            
            # outline: color(0, 255, 255) lineweight(0.25);
            # hatch:   color(180, 180, 180) lineweight(0.10) pattern(ANSI37);
            # Try to match with or without pattern
            match = re.search(r'(\w+):\s*color\((\d+),\s*(\d+),\s*(\d+)\)\s*lineweight\(([\d.]+)\)', line)
            if match:
                layer_name = match.group(1)
                r, g, b = int(match.group(2)), int(match.group(3)), int(match.group(4))
                lw = float(match.group(5))
                self.layers[layer_name] = {'color': (r, g, b), 'lineweight': lw}
            
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
        polyline = None
        while i < len(self.lines):
            line = re.sub(r'//.*$', '', self.lines[i]).strip()
            if not line:
                i += 1
                continue
            if line == '}': break
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
                        p_line = re.sub(r'^\s*->\s*', '', p_line)
                        p_match = re.search(r'\(([^)]+)\)', p_line)
                        if p_match: points.append(p_match.group(1).strip())
                        i += 1
                    polyline = {'name': polyline_name, 'closed': is_closed, 'points': points}
            i += 1
        if polyline:
            self.sketches.append({'name': sketch_name, 'layer': layer, 'polyline': polyline})
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
        """解析 dimension 块"""
        i = start_idx
        # dim linear { or dim vertical {
        match = re.search(r'dim\s+(\w+)', self.lines[i])
        if not match:
            return i
        
        dim_type = match.group(1)
        i += 1
        
        from_point = None
        to_point = None
        text = None
        
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
            
            # text = "B";
            match = re.search(r'text\s*=\s*"([^"]+)";', line)
            if match:
                text = match.group(1)
            
            i += 1
        
        if from_point and to_point and text:
            self.dimensions.append({
                'type': dim_type,
                'from': from_point,
                'to': to_point,
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
        return start_idx
    
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
        
        return start_idx
    
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
        self._add_render_function()
        self._add_usage_comments()
        
        return '\n'.join(self.code)
    
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
        
        # Generate sketches (polylines)
        self._generate_sketches()
        
        # Generate regions (hatches)
        self._generate_regions()
        
        # Generate dimensions
        self._generate_dimensions()
        
        # Generate barshapes
        self._generate_barshapes()
        
        # Generate tables
        self._generate_tables()
        
        # Generate sheets (layouts)
        self._generate_sheets()
        
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

        # Handle simple variable reference
        if re.match(r'^[a-zA-Z_][a-zA-Z0-9_]*$', expr):
            # Handle 't' specially
            if expr == 't':
                return 't_param'
            return expr
        
        # Handle numeric literals
        if re.match(r'^[\d.]+$', expr):
            return expr
        
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
        
        # RGB to ACI (AutoCAD Color Index) conversion
        def rgb_to_aci(r, g, b):
            # Simplified: use color index based on RGB
            if r == 0 and g == 255 and b == 255:  # Cyan
                return 4
            elif r == 180 and g == 180 and b == 180:  # Gray
                return 8
            elif r == 0 and g == 255 and b == 0:  # Green
                return 3
            elif r == 255 and g == 0 and b == 0:  # Red
                return 1
            else:
                return 7  # White
        
        for layer_name, layer_info in self.parser.layers.items():
            color = layer_info['color']
            lw = layer_info['lineweight']
            aci = rgb_to_aci(color[0], color[1], color[2])
            self.code.append(f"  (command \"._LAYER\" \"_M\" \"{layer_name}\" \"_C\" \"{aci}\" \"\" \"_LW\" \"{lw}\" \"\" \"\")")
        
        self.code.append("")
    
    def _generate_sketches(self):
        """生成 sketch (polyline) 代码"""
        for sketch in self.parser.sketches:
            self.code.append(f"  ; Sketch: {sketch['name']}")
            self.code.append(f"  (command \"._LAYER\" \"_S\" \"{sketch['layer']}\" \"\")")
            
            polyline = sketch['polyline']
            # Generate PLINE command
            # Build all points first, then pass to PLINE command
            point_vars = []
            point_idx = 0
            
            for point_expr in polyline['points']:
                # Skip comments
                if point_expr.strip().startswith('//'):
                    continue
                # Convert point expression like "0, 0" or "B, 0" to AutoLISP
                x_expr, y_expr = self._parse_point_expr(point_expr)
                # Handle 't' variable specially
                if 't' in x_expr and x_expr != 't':
                    x_expr = x_expr.replace(' t ', ' t_param ').replace('(t)', '(t_param)')
                if 't' in y_expr and y_expr != 't':
                    y_expr = y_expr.replace(' t ', ' t_param ').replace('(t)', '(t_param)')
                if x_expr == 't':
                    x_expr = 't_param'
                if y_expr == 't':
                    y_expr = 't_param'
                
                # Create point variable to ensure proper evaluation
                pt_var = f"pt_{point_idx}"
                self.code.append(f"    (setq {pt_var} (list {x_expr} {y_expr} 0.0))")
                point_vars.append(pt_var)
                point_idx += 1
            
            # Now create PLINE with all points
            if point_vars:
                self.code.append("  (command \"._PLINE\")")
                for pt_var in point_vars:
                    self.code.append(f"    (command {pt_var})")
                if polyline['closed']:
                    self.code.append("  (command \"_C\")")
                else:
                    self.code.append("  (command \"\")")
            
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
                self.code.append(f"  (command \"._LAYER\" \"_S\" \"hatch\" \"\")")
                
                # Get hatch style
                hatch_style_name = region.get('hatch', 'concrete')
                hatch_style = self.parser.hatch_styles.get(hatch_style_name, {})
                pattern = hatch_style.get('pattern', 'ANSI37')
                scale = hatch_style.get('scale', 1.0)
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
                
                self.code.append("  ; Create hatch on last entity (the polyline)")
                # Store last entity before hatch (entlast may change)
                self.code.append("  (setq hatch_obj (entlast))")
                # HATCH command syntax: HATCH _P pattern_name scale angle _S L "" ""
                # Note: Using ANSI31 as it's universally available in all AutoCAD versions
                self.code.append(f"  (command \"._HATCH\" \"_P\" \"{mapped_pattern}\" \"{scale}\" \"{angle}\" \"_S\" \"L\" \"\" \"\")")
            
            self.code.append("")
    
    def _generate_dimensions(self):
        """生成尺寸标注代码"""
        for dim in self.parser.dimensions:
            dim_type = dim['type']
            from_expr = dim['from']
            to_expr = dim['to']
            text = dim['text']
            
            from_x, from_y = self._parse_point_expr(from_expr)
            to_x, to_y = self._parse_point_expr(to_expr)
            
            self.code.append(f"  ; Dimension: {text}")
            self.code.append(f"  (command \"._LAYER\" \"_S\" \"dim\" \"\")")
            
            if dim_type == 'linear':
                # Calculate dimension line position (offset below)
                self.code.append(f"    (setq dim_pt (list (/ (+ {from_x} {to_x}) 2.0) (- (min {from_y} {to_y}) 100.0) 0.0))")
                self.code.append(f"    (command \"._DIMLINEAR\" (list {from_x} {from_y} 0.0) (list {to_x} {to_y} 0.0) dim_pt)")
            elif dim_type == 'vertical':
                # Calculate dimension line position (offset to the right)
                self.code.append(f"    (setq dim_pt (list (+ (max {from_x} {to_x}) 100.0) (/ (+ {from_y} {to_y}) 2.0) 0.0))")
                self.code.append(f"    (command \"._DIMLINEAR\" (list {from_x} {from_y} 0.0) (list {to_x} {to_y} 0.0) dim_pt)")
            
            self.code.append("")
    
    def _generate_barshapes(self):
        """生成钢筋大样代码 - generates AutoLISP functions to draw bar shapes"""
        if not self.parser.barshapes:
            return
        
        self.code.append("  ; Bar shapes (rebar detail drawings)")
        self.code.append("  ; Each barshape generates a function: (draw-barshape-<Name> base_pt)")
        self.code.append("  ; base_pt is a list (x y z), parameters are taken from global variables")
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
                # Parse segment string: "(x1, y1) -> (x2, y2) -> ..."
                points = self._parse_segments_to_points(segments)
                
                if points:
                    self.code.append(f"  ; Dims: {dims}")
                    self.code.append(f"  (defun draw-barshape-{name} (base_pt / pt x y)")
                    # Switch to rebar layer
                    self.code.append(f"    (command \"._LAYER\" \"_S\" \"rebar\" \"\")")
                    self.code.append(f"    (command \"._PLINE\")")
                    
                    for pt in points:
                        x_expr = self._convert_expr_to_lisp(pt[0])
                        y_expr = self._convert_expr_to_lisp(pt[1])
                        # Calculate absolute point: base_x + x, base_y + y
                        # Use setq to calculate intermediate values for clarity/debugging
                        self.code.append(f"    (setq x (+ (car base_pt) {x_expr}))")
                        self.code.append(f"    (setq y (+ (cadr base_pt) {y_expr}))")
                        self.code.append(f"    (command (list x y))")
                    
                    self.code.append(f"    (command \"\")")
                    
                    # TODO: Implement hook drawing logic
                    if hooks:
                        self.code.append(f"    ; Note: Hooks not yet visualized: {hooks}")
                    
                    self.code.append(f"  )")
            else:
                self.code.append(f"  ; (No segment data for {name})")
            
            self.code.append("")
        
        self.code.append("")
    
    def _parse_segments_to_points(self, segments_str: str) -> list:
        """Parse segment string like '(x1, y1) -> (x2, y2) -> ...' into list of point tuples"""
        points = []
        # Split by ->
        parts = segments_str.split('->')
        for part in parts:
            part = part.strip()
            # Extract (x, y) from each part
            match = re.search(r'\(([^)]+)\)', part)
            if match:
                coord_str = match.group(1)
                # Split by comma, handling potential spaces
                coords = [c.strip() for c in coord_str.split(',')]
                if len(coords) == 2:
                    points.append((coords[0], coords[1]))
        return points
    
    def _generate_tables(self):
        """生成表格代码 - generates AutoLISP code to draw tables"""
        if not self.parser.tables:
            return
        
        self.code.append("  ; Tables - Generate data structures for table rendering")
        
        for name, table in self.parser.tables.items():
            table_type = table.get('type', 'schedule')
            columns = table.get('columns', {})
            rows = table.get('rows', [])
            
            self.code.append(f"  ; --- Table: {name} ---")
            self.code.append(f"  ; Type: {table_type}, Columns: {len(columns)}, Rows: {len(rows)}")
            
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
                self.code.append(f"    ({' '.join(row_values)})  ; Row {i+1}")
            self.code.append("  ))")
            
            # Generate a helper function to draw this table
            self.code.append(f"  ; To draw: (draw-table-{name} base_x base_y cell_width cell_height)")
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
            self.code.append(f"  (setq sheet_{name}_width {w}.0)")
            self.code.append(f"  (setq sheet_{name}_height {h}.0)")
            
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
            
            # Placements - reference what should be placed on this sheet
            placements = sheet.get('placements', [])
            if placements:
                self.code.append(f"  ; Placements for {name}:")
                for p in placements:
                    ptype = p.get('type')
                    pname = p.get('name')
                    self.code.append(f"  ;   - {ptype}: {pname}")
            
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
    if len(sys.argv) < 2:
        print("Usage: python pcad_to_autolisp.py <input.pcad> [output.lsp]")
        sys.exit(1)
    
    input_file = Path(sys.argv[1])
    if not input_file.exists():
        print(f"Error: File not found: {input_file}")
        sys.exit(1)
    
    output_file = Path(sys.argv[2]) if len(sys.argv) > 2 else input_file.with_suffix('.lsp')
    
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
    print(f"  1. Load: (load \"{output_file.absolute()}\")")
    
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
