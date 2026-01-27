import json

def list_all_sheets(json_file):
    with open(json_file, 'r', encoding='utf-8') as f:
        data = json.load(f)

    for file, sheets in data.items():
        print(f"File: {file}")
        for sheet_name in sheets.keys():
            print(f"  - {sheet_name}")
        break # Just first file for sample

if __name__ == "__main__":
    list_all_sheets(r"h:\U形槽配筋软件\u_channel_data.json")
