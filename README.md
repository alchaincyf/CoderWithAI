# 编程教程网站

这是一个提供多种编程语言教程的网络应用项目。

## 功能特点

1. **动态教程内容**: 应用从项目中存储的Markdown文件中加载真实的教程内容。
2. **多语言支持**: 支持多种编程语言,教程按语言分类组织。
3. **响应式设计**: 布局能够适应不同的屏幕尺寸。
4. **动态语言选择**: 语言选择栏会根据可用的屏幕宽度进行调整。
5. **带外部链接的页脚**: 包含指向BookAI.TOP的链接和其他导航链接的页脚。

## 项目结构

- `/tutorials`: 包含每种编程语言的子目录,每个子目录中包含该语言的教程Markdown文件。
- `/components`: 应用中使用的React组件。
- `/lib`: 用于获取和处理教程数据的实用函数。

## 使用方法

查看教程的步骤:
1. 导航到首页。
2. 从顶部栏选择一种编程语言。
3. 从侧边栏选择一个教程。
4. 在主要区域阅读教程内容。

## 开发

本项目使用Next.js和React开发。本地运行步骤:

1. 克隆仓库
2. 使用 `npm install` 安装依赖
3. 使用 `npm run dev` 运行开发服务器
4. 在浏览器中打开 [http://localhost:3000](http://localhost:3000)

## 添加新教程

添加新教程的步骤:
1. 在 `/tutorials` 下适当的语言目录中创建新的Markdown文件。
2. 在Markdown文件的前置元数据中添加 `title` 和 `category` 字段。
3. 使用Markdown格式编写教程内容。

## 未来改进计划

- 实现教程搜索功能
- 添加用户认证以跟踪学习进度
- 为代码块增加语法高亮,增强Markdown渲染效果
- 添加交互式代码编辑器,让用户能够直接在浏览器中运行代码
- 实现教程评分和评论系统
- 添加多语言支持,使教程内容可以翻译成不同语言
- 集成视频教程和交��式练习
- 实现个性化学习路径推荐

## 贡献

我们欢迎社区贡献!如果你想为项目做出贡献,请遵循以下步骤:

1. Fork 这个仓库
2. 创建你的特性分支 (`git checkout -b feature/AmazingFeature`)
3. 提交你的更改 (`git commit -m 'Add some AmazingFeature'`)
4. 推送到分支 (`git push origin feature/AmazingFeature`)
5. 开启一个 Pull Request

## 许可证

本项目采用 MIT 许可证 - 查看 [LICENSE.md](LICENSE.md) 文件了解详情

## 联系我们

如果你有任何问题或建议,请通过以下方式联系我们:

- 电子邮件: support@example.com
- Twitter: [@ExampleProject](https://twitter.com/ExampleProject)
- 项目 Issues: [https://github.com/yourusername/project/issues](https://github.com/yourusername/project/issues)

# AI Chat Widget

This project includes an AI Chat Widget component that can be easily integrated into your Next.js application. The widget features a Google-inspired design with a floating chat button and expandable chat window, and uses the DeepSeek API for intelligent responses.

## Features

- Floating chat button (128px x 128px) in the bottom-right corner
- Expandable chat window (600px x 400px)
- Real-time chat interface with DeepSeek AI integration
- Multi-turn conversation support
- Message input and display
- Responsive and modern design

## Setup

1. Create a `.env.local` file in the root of your project and add your DeepSeek API key:
   ```
   DEEPSEEK_API_KEY=your_deepseek_api_key_here
   ```

2. Install the required dependencies:
   ```
   npm install axios
   ```

## Usage

To use the AIChatWidget in your Next.js application:

1. Import the component:
   ```javascript
   import AIChatWidget from '../components/AIChatWidget';
   ```

2. Use it in your JSX:
   ```jsx
   <AIChatWidget />
   ```

## Customization

You can customize the appearance of the chat widget by modifying the CSS classes in the `AIChatWidget.module.css` file.

## Future Improvements

- Add user authentication
- Implement message persistence
- Add animations for smooth transitions
- Implement responsive design for mobile devices
- Enhance error handling and user feedback

For more information on how to develop and extend this component, please refer to the component file at `components/AIChatWidget.tsx` and its corresponding CSS module file.
