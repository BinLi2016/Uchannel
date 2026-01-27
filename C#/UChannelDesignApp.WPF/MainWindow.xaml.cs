using System;
using System.IO;
using System.Text.Json;
using System.Windows;
using System.Windows.Controls;
using Microsoft.Web.WebView2.Core;
using UChannelDesignApp.Logic;
using UChannelDesignApp.Models;

namespace UChannelDesignApp.WPF
{
    public partial class MainWindow : Window
    {
        private readonly bool _autoDrainage;
        private readonly string _webViewConsoleLogPath;

        public MainWindow()
        {
            InitializeComponent();

            _autoDrainage = Array.Exists(Environment.GetCommandLineArgs(), a => string.Equals(a, "--auto-drainage", StringComparison.OrdinalIgnoreCase));
            _webViewConsoleLogPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "webview-console.log");

            InitializeWebView();
        }

        private async void InitializeWebView()
        {
            try
            {
                await WebView.EnsureCoreWebView2Async(null);

                // Capture JS console output for verification/debug.
                try
                {
                    WebView.CoreWebView2.WebMessageReceived += (_, args) =>
                    {
                        try
                        {
                            string msg = args.TryGetWebMessageAsString();
                            File.AppendAllText(
                                _webViewConsoleLogPath,
                                $"[{DateTime.Now:O}] [js] {msg}{Environment.NewLine}");
                        }
                        catch
                        {
                            // Best-effort logging only.
                        }
                    };

                    // Inject console forwarding bridge.
                    const string consoleBridgeScript = @"
(() => {
  const safe = (v) => {
    try {
      if (typeof v === 'string') return v;
      return JSON.stringify(v);
    } catch (_) {
      return String(v);
    }
  };
  const forward = (level, args) => {
    try {
      if (window.chrome && window.chrome.webview && typeof window.chrome.webview.postMessage === 'function') {
        const text = args.map(safe).join(' ');
        window.chrome.webview.postMessage(`[${level}] ${text}`);
      }
    } catch (_) {}
  };
  const wrap = (level) => {
    const orig = console[level];
    console[level] = (...args) => {
      try { forward(level, args); } catch (_) {}
      try { orig && orig.apply(console, args); } catch (_) {}
    };
  };
  ['log','info','warn','error'].forEach(wrap);
})();
";

                    _ = WebView.CoreWebView2.AddScriptToExecuteOnDocumentCreatedAsync(consoleBridgeScript);
                }
                catch
                {
                    // Best-effort logging only.
                }

                // Map virtual host to local MXCAD folder
                string? currentDir = AppDomain.CurrentDomain.BaseDirectory;
                string? mxcadPath = null;

                while (currentDir != null)
                {
                    string potentialPath = Path.Combine(currentDir, "MXCAD");
                    if (Directory.Exists(potentialPath))
                    {
                        mxcadPath = potentialPath;
                        break;
                    }
                    currentDir = Path.GetDirectoryName(currentDir);
                }

                if (mxcadPath != null)
                {
                    WebView.CoreWebView2.SetVirtualHostNameToFolderMapping(
                        "mxcad.local", mxcadPath, CoreWebView2HostResourceAccessKind.Allow);
                    WebView.Source = new Uri("https://mxcad.local/index.html");

                    if (_autoDrainage)
                    {
                        WebView.NavigationCompleted += async (_, __) =>
                        {
                            try
                            {
                                await DrawDrainageMotorLaneInternalAsync();
                            }
                            catch (Exception ex)
                            {
                                try
                                {
                                    File.AppendAllText(_webViewConsoleLogPath, $"[{DateTime.Now:O}] [host] auto-drainage failed: {ex}{Environment.NewLine}");
                                }
                                catch
                                {
                                    // ignore
                                }
                            }
                        };
                    }
                }
                else
                {
                    StatusLabel.Text = "错误：未在父目录中找到 MXCAD 文件夹。";
                }
            }
            catch (Exception ex)
            {
                StatusLabel.Text = "WebView2 错误：" + ex.Message;
            }
        }

        private void PresetCombo_SelectionChanged(object sender, SelectionChangedEventArgs e)
        {
            if (PresetCombo == null || WidthTextBox == null) return;

            if (PresetCombo.SelectedItem is not ComboBoxItem item) return;
            string type = item.Content?.ToString() ?? "YU1";

            (double w, double h, double wt, double ft) = DimensionLookup.GetPresetDimensions(type);

            WidthTextBox.Text = w.ToString();
            HeightTextBox.Text = h.ToString();
            WallTTextBox.Text = wt.ToString();
            FloorTTextBox.Text = ft.ToString();
        }

        private async void DrawButton_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                string presetType = (PresetCombo.SelectedItem as ComboBoxItem)?.Content?.ToString() ?? "YU1";

                var p = new UChannelParameters
                {
                    InternalWidth = double.Parse(WidthTextBox.Text),
                    InternalHeight = double.Parse(HeightTextBox.Text),
                    WallThickness = double.Parse(WallTTextBox.Text),
                    BottomThickness = double.Parse(FloorTTextBox.Text)
                };

                // 1. Calculate Core Logic
                var structuralResult = StructuralEngine.Verify(p);
                var geometry = CoordinateEngine.CalculateCrossSection(p);

                // 2. Prepare Drawing Data
                var drawingData = new
                {
                    Type = "U-Channel Cross Section",
                    PresetType = presetType,
                    Parameters = p,
                    Structural = structuralResult,
                    Geometry = geometry
                };

                string json = JsonSerializer.Serialize(drawingData);

                // 3. Send to MXCAD
                await WebView.CoreWebView2.ExecuteScriptAsync($"window.drawUChannel({json});");
                StatusLabel.Text = $"状态：已更新 {WidthTextBox.Text}×{HeightTextBox.Text} 的图纸";
            }
            catch (Exception ex)
            {
                MessageBox.Show("错误：" + ex.Message);
            }
        }

        private async void DrawLayoutSheetButton_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                string presetType = (PresetCombo.SelectedItem as ComboBoxItem)?.Content?.ToString() ?? "YU1";

                var p = new UChannelParameters
                {
                    InternalWidth = double.Parse(WidthTextBox.Text),
                    InternalHeight = double.Parse(HeightTextBox.Text),
                    WallThickness = double.Parse(WallTTextBox.Text),
                    BottomThickness = double.Parse(FloorTTextBox.Text)
                };

                var structuralResult = StructuralEngine.Verify(p);
                var geometry = CoordinateEngine.CalculateCrossSection(p);

                var drawingData = new
                {
                    Type = "U-Channel Layout Sheet",
                    PresetType = presetType,
                    Parameters = p,
                    Structural = structuralResult,
                    Geometry = geometry
                };

                string json = JsonSerializer.Serialize(drawingData);
                await WebView.CoreWebView2.ExecuteScriptAsync($"window.drawUChannelLayoutSheet({json});");
                StatusLabel.Text = $"状态：已绘制 {presetType} 布置图";
            }
            catch (Exception ex)
            {
                MessageBox.Show("错误：" + ex.Message);
            }
        }

        private async void DrawDrainageMotorLaneButton_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                await DrawDrainageMotorLaneInternalAsync();
                StatusLabel.Text = "状态：排水沟图纸已更新。";
            }
            catch (Exception ex)
            {
                MessageBox.Show("错误：" + ex.Message);
            }
        }

        private async System.Threading.Tasks.Task DrawDrainageMotorLaneInternalAsync()
        {
            var (section, cover, channelTable, coverTable, projectInfo, notes) = DrainageDrawingPreset.MotorLane();

            // Payload contract for JS: { section, cover, channelTable, coverTable, projectInfo, notes }
            var payload = new
            {
                section,
                cover,
                channelTable,
                coverTable,
                projectInfo,
                notes,
            };

            string json = JsonSerializer.Serialize(payload, new JsonSerializerOptions
            {
                PropertyNamingPolicy = JsonNamingPolicy.CamelCase
            });

            try
            {
                File.AppendAllText(_webViewConsoleLogPath, $"[{DateTime.Now:O}] [host] invoking drawDrainageChannelReinforcement(payload)\n");
            }
            catch
            {
                // ignore
            }

            await WebView.CoreWebView2.ExecuteScriptAsync($"window.drawDrainageChannelReinforcement({json});");
        }
    }
}
