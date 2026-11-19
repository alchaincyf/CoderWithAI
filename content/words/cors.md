---
title: 跨域（CORS）
slug: cors
aka: ["CORS", "Cross-Origin", "跨域", "Access-Control-Allow-Origin", "同源策略"]
summary: 浏览器的安全规则：网页默认不许直接读别的网站的数据。
category: 连接
level: 5
related: [api, backend, devtools, localhost, https]
updated: 2026-11-18
---

## 一句话
跨域是浏览器的一道安全门：一个网站里的脚本，想读取另一个域名的数据，对方必须明确说「允许」。没说允许，浏览器就拦下来。

## 打个比方
像小区保安。你（网页）想去隔壁小区（别的域名）取东西，保安要看对方有没有提前备案说「欢迎这位住户」。

不贴切的地方：拦你的是你自己的浏览器，不是对方服务器。同一个请求，用命令行或服务端发，往往完全正常。

## 你会在哪儿碰到它
- 本地开发时网页请求接口，[开发者工具](/words/devtools)的Console出现红字：

```error
Access to fetch at 'https://api.example.com/data' from origin 'http://localhost:3000' has been blocked by CORS policy: No 'Access-Control-Allow-Origin' header is present on the requested resource.
```

- 你调用的是第三方接口，或前后端分别部署在不同域名、不同端口。

## AI提到它时，你该怎么接
```prompt 复制给 Claude Code
浏览器报了CORS错误，报错原文如下：<粘贴完整报错>。
请先判断：这个接口是我自己的后端，还是第三方的？
如果是第三方：不要让我改浏览器设置，改成由我自己的服务端代发请求（做一个/api代理）。
如果是我自己的：只允许我的前端域名，不要直接写成允许所有来源。修好后告诉我怎么验证。
```

## 容易踩的坑
- 在浏览器里装「跨域插件」解决：只对你自己有效，别的用户照样报错。
- 服务端写成 `Access-Control-Allow-Origin: *` 一劳永逸：公开数据可以，涉及登录信息的接口不能这么放开。
- 把第三方密钥放在前端、靠跨域直连：见[API Key](/words/api-key)，应该走自己的服务端。
- 看到的是CORS报错，真正原因却是接口本身500、地址写错或根本没启动，先看[Network](/words/devtools)里的真实状态码。
