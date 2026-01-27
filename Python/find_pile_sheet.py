import json

def find_sheet(json_file, sheet_name):
    with open(json_file, 'r', encoding='utf-8') as f:
        data = json.load(f)

    for file, sheets in data.items():
        if sheet_name in sheets:
            print(f"Found sheet '{sheet_name}' in {file}")
            sheet_data = sheets[sheet_name]
            for i, row in enumerate(sheet_data[:40]): # First 40 rows
                print(f"Row {i}: {row}")
            return
    print(f"Sheet '{sheet_name}' not found.")

if __name__ == "__main__":
    find_sheet(r"h:\U形槽配筋软件\u_channel_data.json", "4.防水")
