import openpyxl
import os

def get_benchmark_values(file_path, sheet_name):
    print(f"Loading {file_path} [{sheet_name}]...")
    wb = openpyxl.load_workbook(file_path, data_only=True)
    sheet = wb[sheet_name]
    
    # We are looking for something like "壁后主动土压力系数" or "Ka"
    # And "弯矩" or "M"
    data = []
    for row in sheet.iter_rows(max_row=100, max_col=30, values_only=True):
        data.append(row)
        
    for i, row in enumerate(data):
        row_str = " ".join([str(c) for c in row if c is not None])
        if "Ka" in row_str or "土压力" in row_str:
            print(f"Row {i}: {row}")
            # Identify columns
            # Typically H is in early columns, Ka in middle
            
    return data

if __name__ == "__main__":
    get_benchmark_values(r"h:\U形槽配筋软件\DesignSheet\U型槽施工图设计 振华路.xlsx", "1.U型槽主筋验算")
