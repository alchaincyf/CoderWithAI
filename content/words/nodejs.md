---
title: Node.js
slug: nodejs
aka: ["Node", "NodeJS", "node", "运行时", "LTS"]
summary: 让JavaScript能在浏览器之外运行的程序，几乎所有前端工具都靠它。
category: 基础
level: 1
related: [npm, web-basics, dependencies, dev-server, framework]
updated: 2026-11-18
---

## 一句话
JavaScript原本只能在浏览器里跑。Node.js让它也能在你的电脑或服务器上直接运行，于是你可以用它启动项目、安装工具、写后端。

## 打个比方
原本JavaScript是一位只能在剧院（浏览器）里演出的演员；Node.js给了它一个自己的摄影棚，随时随地都能开拍。

不贴切的地方：Node.js并不是一门新语言，它是「运行JavaScript的环境」，语言还是那个语言。

## 你会在哪儿碰到它
- 做网页项目、装很多AI工具之前，AI常让你「先装Node.js」。Claude Code官方推荐的原生安装并不需要Node.js，见[第1关](/start/setup)；用npm方式安装才需要。
- 到官网nodejs.org下载安装即可。官网会标出LTS（长期支持版）。截至2026年11月，官网推荐的LTS是26.x。新手选LTS就对了。
- 装好后，终端里输入`node -v`能显示版本号，说明装好了。同时会带上[npm](/words/npm)。
- 报错里出现「node: command not found」或「'node'不是内部或外部命令」，就是没装或没装好。

## AI提到它时，你该怎么接
```prompt
请带我检查本机有没有装Node.js，版本是多少，是否满足这个项目的要求。如果没装，告诉我最稳妥的安装方式，我用的是Mac（或Windows）。
```

```prompt
这个项目要求的Node版本是（或：我装了Node但命令找不到）。请帮我排查原因并一步步修好，每一步告诉我该看到什么。
```

## 容易踩的坑
- 装完之后，已经打开的终端窗口认不出它。关掉重开一个终端。
- 版本太老，新项目跑不起来。报错里会写出需要的版本，把全文发给AI。
- 国内下载或安装依赖慢，可以问AI有没有合适的镜像，但要选信誉好的官方或大厂镜像。
- 同时装了多个Node版本，管理混乱。需要时让AI介绍版本管理工具，新手先不碰。
