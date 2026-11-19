---
title: 浏览器本地存储（localStorage）
slug: localstorage
aka: ["localStorage", "sessionStorage", "IndexedDB", "Cookie", "本地存储"]
summary: 网页把小数据存在你自己浏览器里的功能，不用服务器。
category: 数据
level: 4
related: [database, frontend, devtools, json, auth]
updated: 2026-11-18
---

## 一句话
localStorage是浏览器给每个网站分配的一个小储物格，网页可以往里存文字，关掉页面、重启浏览器后依然在。数据只在这一台电脑的这一个浏览器里。

## 打个比方
像你随身带的小记事本：想记什么就记，下次打开还在。

不贴切的地方：记事本丢了或你清理浏览器数据，内容就没了，别的设备也看不到。它不是[数据库](/words/database)。

## 你会在哪儿碰到它
- 做番茄钟、待办、记账这类「只给自己用」的小工具，AI常说「先用localStorage存，不用后端」。
- 在[浏览器开发者工具](/words/devtools)的Application标签里能看到存了什么。
- 代码里出现`localStorage.setItem`和`localStorage.getItem`。

## AI提到它时，你该怎么接
```prompt 复制给 Claude Code
把这个待办清单做成刷新页面后数据不丢：用localStorage保存。
要求：1.数据用JSON格式存；2.读取时如果数据损坏或为空，页面不能白屏，要回到空列表；3.加一个「导出备份」按钮，把数据下载成.json文件，再加一个导入按钮；4.告诉我这个方案的局限。
```

## 容易踩的坑
- 换设备、换浏览器、无痕窗口、清理缓存，数据都没了。想多设备同步就得换成数据库。
- 容量有限，通常约5MB级别，不适合存图片、视频。
- 别存密码、密钥、隐私信息：同一网站下的脚本都读得到，安全性很低。
- 只能存文字：对象要先转成JSON字符串，读出来再转回去，AI漏了这一步就会读到`[object Object]`。
