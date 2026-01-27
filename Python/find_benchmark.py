import json

def find_benchmark_data(json_file):
    with open(json_file, 'r', encoding='utf-8') as f:
        data = json.load(f)

    for file_path, sheets in data.items():
        if "1.U型槽主筋验算" in sheets:
            sheet = sheets["1.U型槽主筋验算"]
            for i, row in enumerate(sheet):
                # Check for "Ka" or "土压力" or common structural headers
                row_str = " ".join([str(c) for c in row if c is not None])
                if "Ka" in row_str or "倾覆" in row_str or "稳定" in row_str:
                    print(f"File: {file_path}")
                    print(f"Header Row {i}: {row}")
                    # Print next few rows
                    for k in range(1, 10):
                        if i+k < len(sheet):
                            print(f"Data Row {i+k}: {sheet[i+k]}")
                    return

if __name__ == "__main__":
    find_benchmark_data(r"h:\U形槽配筋软件\u_channel_data.json")
