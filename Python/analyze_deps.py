import json
import os

def analyze_dependencies(meta_file):
    with open(meta_file, 'r', encoding='utf-8') as f:
        data = json.load(f)

    dependency_map = {}

    for file_path, file_data in data.items():
        filename = os.path.basename(file_path)
        dependency_map[filename] = {}
        sheets = file_data.get("sheets", {})
        for sheet_name, sheet_data in sheets.items():
            formulas = sheet_data.get("formulas", {})
            refs = set()
            for cell, formula in formulas.items():
                if isinstance(formula, str):
                    # Simple heuristic to find sheet references like 'SheetName'!A1
                    import re
                    found = re.findall(r"'?([^'!\s]+)'?!", formula)
                    for f in found:
                        if f != sheet_name:
                            refs.add(f)
            dependency_map[filename][sheet_name] = list(refs)
    
    return dependency_map

if __name__ == "__main__":
    meta_path = r"h:\U形槽配筋软件\deep_excel_meta.json"
    if os.path.exists(meta_path):
        deps = analyze_dependencies(meta_path)
        with open(r"h:\U形槽配筋软件\sheet_dependencies.json", "w", encoding="utf-8") as f:
            json.dump(deps, f, indent=4, ensure_ascii=False)
        print("Dependency map generated in sheet_dependencies.json")
    else:
        print("deep_excel_meta.json not found")
