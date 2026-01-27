import json

def debug_json(json_file):
    with open(json_file, 'r', encoding='utf-8') as f:
        data = json.load(f)
    
    first_file = list(data.keys())[0]
    print(f"First file: {first_file}")
    print(f"Sheets: {list(data[first_file].keys())}")
    
    for sheet_name in data[first_file].keys():
        print(f"\nSample from sheet: {sheet_name}")
        for row in data[first_file][sheet_name][:5]:
            print(row)

if __name__ == "__main__":
    debug_json(r"h:\U形槽配筋软件\u_channel_data.json")
