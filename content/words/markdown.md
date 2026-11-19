---
title: Markdown
slug: markdown
aka: ["Markdown", ".md", "README.md", "MD"]
summary: "用#、*这类简单符号写出带标题和列表的文档的轻量格式。"
category: 基础
level: 2
related: [agents-md, repo, github, prompt, skill]
updated: 2026-11-18
---

## 一句话
Markdown 是一种写文档的方式：在纯文字里加几个符号，就能表示标题、列表、加粗、链接。文件后缀是 .md。

## 打个比方
给文字打「手势暗号」。在行首写个 #，表示「这是大标题」；写个 -，表示「这是一条列表」。不用点工具栏，边打字边排版。

不贴切的地方：它不是 Word，没有「所见即所得」。同一份 .md，在不同软件里显示的样子会稍有不同。

## 你会在哪儿碰到它
- 项目根目录的 README.md，是项目说明书，GitHub 打开仓库首先展示的就是它。
- 给 AI 的规则文件，如 [AGENTS.md](/words/agents-md)、Claude Code 的 CLAUDE.md，还有 [Skill](/words/skill) 里的 SKILL.md，都是 Markdown。
- AI 在对话里输出的列表、表格、代码块，也是 Markdown 渲染的。
- 基本写法：

```markdown
# 一级标题
## 二级标题
- 列表项
**加粗**
[链接文字](https://example.com)
```

## AI提到它时，你该怎么接
```prompt
请为这个项目写一份 README.md，包括：项目是做什么的、怎么在本地运行、怎么部署。读者是完全不懂技术的人，用大白话写。
```

```prompt
请把下面这段内容整理成 Markdown，用二级标题分章节，列表用短横线，保留原意，不要添加我没写过的信息。（粘贴内容）
```

## 容易踩的坑
- 符号后面忘了空格，例如「#标题」不会被识别成标题，要写「# 标题」。
- 换行失灵。Markdown 里单个换行常被当作同一段，要空一行才分段。
- 把 .md 文件用记事本打开，满屏符号，别慌，那是正常的。想看效果，用支持预览的编辑器，见[编辑器](/words/editor)。
- 微信公众号等平台不直接支持 Markdown，粘贴过去会出现一堆符号，需要先转换。
