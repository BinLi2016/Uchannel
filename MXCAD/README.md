# MXCAD-Test 演示项目

## 项目简介
- 这是一个基于 MXCAD Web SDK 的最小可运行示例，展示如何在浏览器中初始化 CAD 画布并绘制直线、闭合多段线、文字、剖面线以及标注等实体。
- 页面位于 `index.html`，通过引用 CDN 上的 `mxdraw` 与 `mxcad` UMD 资源完成加载。
- 仓库附带了常见 SHX/TTF 字体和一个 `test2.mxweb` 示例文件，方便验证字体渲染与文件打开流程。

## 目录结构
- `index.html`：主示例页面，演示初始化流程与脚本化绘制。
- `docs-index.html`：官方文档快速入口，便于离线查阅 API。
- `mxcad.js` / `mxcad.d.ts`：本地缓存的 MXCAD 运行库与类型定义。
- `fonts/`：需要的 SHX/TTF 字体资源；若缺字形，可在此放入或重命名字体文件。
- `test2.mxweb`：可选示例工程，可通过脚本或界面加载查看。
- `patch.diff`：本地修订记录，可忽略或用于回顾差异。

## 快速体验
1. **启动静态服务器**  
   建议使用任何本地静态服务器（如 `npx serve .`、`python -m http.server` 等）托管项目根目录，避免浏览器 `file://` 环境造成的跨域与字体加载问题。
2. **访问示例页面**  
   在浏览器访问 `http://localhost:端口/index.html`，等待页面加载 MXCAD，画布会自动绘制示例几何并缩放至合适视图。
3. **加载自定义模型（可选）**  
   - 在 `index.html` 中设置 `createMxCad`的 `fileUrl` 指向您的 `.mxweb` 文件，或在初始化完成后调用 `openFile` API。
   - 若模型引用了额外字体，请将对应 SHX/TTF 置于 `fonts/` 目录下。

## 字体资源说明
- MXCAD 会根据初始化参数中的 `fontspath: './fonts/'` 读取字体，请将项目所需的 SHX/TTF 复制到此目录。  
- 若缺少特定字体，可临时复制现有 SHX 并重命名以避免空白字形，但渲染效果可能与原字体不一致。
- 字体多为商业授权，请遵守原始许可证，避免公开传播未授权字体。

## 常见问题
- **画布空白或报错**：确保通过本地服务器访问，并检查浏览器控制台是否成功拉取 `mxdraw` 与 `mxcad` CDN 资源。
- **文字乱码或缺字**：确认字体已放入 `fonts/`，或在 `ensureDefaultTextStyle` 中修改默认字体配置。
- **加载模型失败**：检查 WASM 资源路径，默认示例使用单线程构建 `dist/wasm/2d-st`，无需设置 COOP/COEP 头。

## 参考链接
- MXCAD 官方文档主页：https://mxcad.github.io/mxcad_docs/
- MXDRAW 官方 npm 包：https://www.npmjs.com/package/mxdraw

若需将示例整合到自己的业务项目，可基于 `index.html` 的初始化脚本逐步拆分为模块化代码，或结合构建工具加载 MXCAD ES Module 版本。
