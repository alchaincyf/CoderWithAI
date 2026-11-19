---
title: 浏览器插件
slug: browser-extension
aka: ["Browser Extension", "Chrome Extension", "Chrome插件", "浏览器扩展", "Manifest V3", "Web Store"]
summary: 装进浏览器里的小工具，可以改网页、加按钮、读页面内容。
category: App
level: 6
related: [frontend, json, devtools, api, deploy]
updated: 2026-11-18
---

## 一句话
浏览器插件是装在Chrome、Edge等浏览器里的小程序，能在特定网页上加按钮、改样式、提取内容。它本质是一个带说明文件`manifest.json`的文件夹。

## 打个比方
像给眼镜加的一层滤镜。网页还是那个网页，但你戴上之后，看到的多了一个按钮、少了一堆广告。

不贴切的地方：滤镜只影响观感，插件可能读到你页面上的所有信息，所以权限声明是它最敏感的部分。

## 你会在哪儿碰到它
- 项目里有`manifest.json`、`content.js`、`popup.html`、`background.js`。
- 本地测试不用上架：在Chrome地址栏输入`chrome://extensions`，打开「开发者模式」，点「加载已解压的扩展程序」，选择你的文件夹。
- 现在Chrome的插件使用Manifest V3，AI写旧的V2写法会失效。
- 发布到Chrome应用商店需要开发者账号和一次性注册费，金额以注册页为准，还要提交审核。

## AI提到它时，你该怎么接
```prompt 复制给 Claude Code
请做一个Chrome插件：在任意网页上点击插件图标，把当前页面标题和网址复制到剪贴板。
要求：1.使用Manifest V3；2.权限只申请必要的最小集合，并逐条解释为什么需要；3.告诉我怎么在chrome://extensions里加载测试；4.改了代码后，我该怎么让它刷新生效。
先查Chrome官方的Manifest V3文档确认写法。
```

## 容易踩的坑
- 申请过多权限：审核更严，用户也不敢装。
- 改了代码没生效：要在扩展管理页点刷新，网页本身也要重新刷新。
- 用V2的写法：官方已在推进弃用，让AI明确用V3。
- 内容脚本和页面脚本是隔离环境，变量互相看不到，调用页面的数据要换方式。
- 商店审核会检查隐私说明与数据使用，要如实填写。
