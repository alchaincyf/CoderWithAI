---
title: "Unexpected token '<' ... is not valid JSON怎么办"
slug: json-unexpected-token
summary: 程序想要数据却收到一个网页。多半是接口地址错了、服务没开或返回了错误页。
category: 数据与接口
match: ["Unexpected token '<'", "is not valid JSON", "Unexpected token < in JSON at position 0", "<!DOCTYPE", "JSON.parse: unexpected character", "SyntaxError: Unexpected token"]
platform: [all]
updated: 2026-11-18
---

```error
Uncaught (in promise) SyntaxError: Unexpected token '<', "<!DOCTYPE "... is not valid JSON
```

老一点的浏览器写作`Unexpected token < in JSON at position 0`。

## 人话：这是怎么回事

你的页面向某个[接口](/words/api)要数据，期待收到[JSON](/words/json)格式（长得像`{"name":"xx"}`）。结果收到的是一个HTML网页，开头是`<!DOCTYPE html>`，那个`<`符号就是报错里的「意外字符」。

对方返回网页，通常是出错了：地址写错了（404页面）、后端没启动、被拦到了登录页、服务器出错了。不是JSON格式的问题，是根本没拿到数据。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code / Codex
我的页面报Unexpected token '<' ... is not valid JSON。
请先找出是哪一行代码在fetch或请求哪个地址。
然后用`curl -i`直接请求这个地址，看它实际返回的是什么状态码和内容（别猜）。
告诉我返回的是404页、登录页、还是服务器错误页，并解释原因。
修复后，在代码里加上判断：状态码不是200时显示明确的错误提示，而不是直接解析JSON。
最后验证请求能拿到正确的JSON。
```

## 想自己动手的话

第一步，打开浏览器开发者工具（Mac按Command+Option+I，Windows按F12），点「Network」（网络）标签，刷新页面。

第二步，找到那个失败的请求（通常叫api之类），点开，看两样：

- 状态码（Status）：404是地址错了，500是服务器出错了，401是没登录。
- Response（响应）：里面是不是一整页HTML。

第三步，对症处理：

- 404：检查请求地址有没有拼错、`/api/xxx`的路径对应的文件是否存在。
- 服务没启动：确认后端或`npm run dev`正在运行。
- 开发环境正常、线上不行：多半是线上缺了配置，看[线上缺环境变量](/fix/env-missing-in-production)。

可以用curl直接看接口返回了什么：

```bash
curl -i http://localhost:3000/api/hello
```

`-i`让它同时显示状态码，把地址换成你的接口。

## 下次怎么避免

请求代码里先检查`response.ok`，再调用`.json()`。

## 还不行？

- 状态码是401：看[401未授权](/fix/api-401)。
- 状态码是429：看[429请求太多](/fix/api-429)。
- 浏览器提示被拦截：看[CORS报错](/fix/cors-blocked)。
- 请求直接失败：看[fetch failed](/fix/fetch-failed)。
