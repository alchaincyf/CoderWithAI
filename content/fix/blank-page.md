---
title: "网页一片空白怎么办（怎么看控制台、怎么把错误交给AI）"
slug: blank-page
summary: 空白页背后几乎都有一条红色报错。打开控制台，复制发给AI。
category: 页面问题
match: ["Uncaught TypeError", "Cannot read properties of undefined", "Failed to load resource", "Uncaught ReferenceError", "is not defined", "Uncaught SyntaxError", "blank page"]
platform: [all]
updated: 2026-11-18
---

```error
Uncaught TypeError: Cannot read properties of undefined (reading 'map')
    at App (App.jsx:12:25)
```

页面空白时，浏览器的控制台里常常是这样的红色文字。其他常见：`Uncaught ReferenceError: xxx is not defined`、`Failed to load resource: the server responded with a status of 404`。

## 人话：这是怎么回事

页面上什么都没有，不是因为没内容，而是程序在中途出错，停住了。出错的原因，浏览器已经悄悄记在「控制台」里，只是页面上不显示。

所以重点不是猜，是把那条红字找出来交给 AI。你不需要看懂它。

## 最快的解法：把这段发给AI

先按下一节打开控制台，把红字复制出来，再发：

```prompt 复制给 Claude Code / Codex
我的网页打开是空白的。下面是浏览器控制台（Console）里的红色报错原文：

（在这里粘贴报错）

请先用人话告诉我：是哪个文件第几行出的错、原因是什么。
然后修复它，修完运行 npm run build 确认没有编译错误。
最后请你描述页面现在应该看到什么，我来对照确认。
```

## 想自己动手的话

打开控制台：

- Mac Chrome：按 Command+Option+J。
- Windows Chrome：按 F12，或 Ctrl+Shift+J。
- 点击顶部的「Console」（控制台）标签。

红色的行是报错。点它左边的小三角可以展开更多信息，整段复制，再发给 AI。

如果控制台没有红字，看下一处，点「Network」（网络）标签，刷新页面，找红色的行或状态码是 404、500 的请求。点开它，复制信息。

没有红字也没有失败请求，看终端：运行项目的那个终端窗口里可能有报错。把它也一起贴给 AI。

补充一个常见情形：部署到线上才空白、本地正常。多半是资源路径或环境变量问题，控制台里同样会有线索。

> [!tip] 截图也可以发给 AI，但文字比截图更准。能复制就复制。

## 下次怎么避免

让 AI 写代码时，加一句「对可能为空的数据做空值保护，并在出错时显示提示而不是白屏」。

## 还不行？

- 报错提到 JSON：看 [Unexpected token '<'](/fix/json-unexpected-token)。
- 只有线上空白：看 [线上缺环境变量](/fix/env-missing-in-production)、[Vercel 404](/fix/vercel-404)。
- 认识浏览器开发者工具：[开发者工具](/words/devtools)。
