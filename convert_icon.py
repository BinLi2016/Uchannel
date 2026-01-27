from PIL import Image
import os

img_path = r"h:\U形槽配筋软件\C#\UChannelDesignApp.WPF\app_icon.png"
ico_path = r"h:\U形槽配筋软件\C#\UChannelDesignApp.WPF\app_icon.ico"

try:
    img = Image.open(img_path)
    # Save with multiple sizes for better quality
    img.save(ico_path, format='ICO', sizes=[(16, 16), (32, 32), (48, 48), (64, 64), (128, 128), (256, 256)])
    print("Successfully converted PNG to ICO")
except Exception as e:
    print(f"Failed to convert: {e}")
