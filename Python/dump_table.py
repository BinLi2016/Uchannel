import json

def dump_table(json_file, table_header):
    with open(json_file, 'r', encoding='utf-8') as f:
        data = json.load(f)

    for file, sheets in data.items():
        if "2.U型槽钢筋" in sheets:
            sheet_data = sheets["2.U型槽钢筋"]
            found = False
            for i, row in enumerate(sheet_data):
                if table_header in str(row):
                    found = True
                    # Print headers and next 5 rows
                    for j in range(i, i + 10):
                        if j < len(sheet_data):
                            print(f"Row {j}: {sheet_data[j]}")
            if found:
                break

if __name__ == "__main__":
    dump_table(r"h:\U形槽配筋软件\u_channel_data.json", "钢筋图参数表")
