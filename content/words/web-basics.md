---
title: HTML、CSS和JavaScript
slug: web-basics
aka: ["HTML", "CSS", "JavaScript", "JS", "网页三件套", "TypeScript"]
summary: 网页的三种基本材料：HTML搭骨架，CSS管外观，JavaScript管行为。
category: 网页
level: 2
related: [frontend, react, framework, devtools, static-site]
updated: 2026-11-18
---

## 一句话
所有网页，最终都是这三样东西：HTML 说明页面上有什么，CSS 说明它长什么样，JavaScript 说明它怎么动。

## 打个比方
盖房子：HTML 是钢筋水泥的结构，有几个房间、门在哪；CSS 是装修，墙刷什么色、地板什么材质；JavaScript 是电路和开关，按下去灯才会亮。

不贴切的地方：房子建好就不会变，网页的结构、样式和行为每时每刻都能被 JavaScript 改。

## 你会在哪儿碰到它
- 文件后缀：.html、.css、.js。你让 AI 做一个单页网站，最小的版本就是一个 index.html，三样东西可以写在一个文件里。
- 项目里还会看到 .ts、.tsx。TypeScript 是 JavaScript 的加强版，多了「类型检查」，写错更早被发现，最终仍会变成 JavaScript 运行。
- 用 [React](/words/react) 等框架时，看起来不像 HTML，但最后到浏览器里还是这三样。
- 想改文字或颜色，对 AI 说「改标题文字」「把按钮改成蓝色」就行，不用知道它在哪个文件里。

## AI提到它时，你该怎么接
```prompt
请用一个单独的 index.html 文件做一个页面：（描述）。HTML、CSS、JavaScript 都写在这一个文件里，我双击就能在浏览器打开。完成后告诉我怎么改标题和配色。
```

```prompt
我看不懂这段代码里哪部分是结构、哪部分是样式、哪部分是行为。请用三种颜色或标注区分，并各用一句话解释。
```

## 容易踩的坑
- 单文件 HTML 里用 JavaScript 去请求网络数据，直接双击打开（地址以 file:// 开头）时，常会被浏览器拦下。让 AI 把数据直接写进文件里，或起一个[开发服务器](/words/dev-server)。
- 改了代码浏览器没变化，多半是缓存，强制刷新（Mac：Command+Shift+R）。
- 以为 Java 和 JavaScript 有关系。它们是两门不同的语言，名字像是历史原因。
- 样式冲突：AI 加了新样式，把旧的覆盖了。出现时把截图发给 AI。
