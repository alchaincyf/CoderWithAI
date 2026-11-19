---
title: 报错「has been blocked by CORS policy」怎么办
slug: cors-blocked
summary: 浏览器拦截了跨域请求，是对方服务器没允许你的网站访问。让后端放行，或由你自己的后端中转。
category: 数据与接口
match: ["blocked by CORS policy", "No 'Access-Control-Allow-Origin' header", "CORS", "Access-Control-Allow-Origin", "preflight", "Failed to fetch"]
platform: [all]
updated: 2026-11-18
---

```error
Access to fetch at 'https://api.example.com/data' from origin 'http://localhost:3000' has been blocked by CORS policy: No 'Access-Control-Allow-Origin' header is present on the requested resource.
```

## 人话：这是怎么回事

你的网页（来源是`localhost:3000`）想去请求另一个网站（`api.example.com`）的数据。浏览器有个安全规则：别的网站想被你的网页访问，必须在返回内容里明确说「我允许」，这就是[CORS](/words/cors)。对方没说，浏览器就拦下来了。

重点：**这是浏览器拦的，不是你的代码写错，也不是请求没到服务器。**同一个请求，用终端或Postman发出去往往是通的。所以问题不出在你这边的请求，出在对方服务器有没有放行。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code
前端调用接口时，浏览器控制台报CORS错误：

（把完整的红色报错粘贴到这里，包含Access to fetch at ... from origin ...）

请先判断：这个接口是我自己的后端，还是第三方服务（如OpenAI、天气API等）？
- 如果是我自己的后端：在服务端加上允许我前端来源的CORS设置，来源只写我的具体域名（开发时用 http://localhost:端口），不要用通配符 *，并说明OPTIONS预检请求怎么处理。
- 如果是第三方：不要让浏览器直接请求它。改成由我项目的服务端接口（如Next.js的API route）代为请求，前端只调用我自己的接口。同时检查API密钥是否因此被放进了前端代码，是的话改到服务端。
改完用浏览器控制台和网络面板验证请求成功。
```

## 想自己动手的话

先分清接口是谁的。

接口是你自己写的后端：在后端响应头里加上`Access-Control-Allow-Origin`，值写你的网站地址。大多数后端框架都有现成的CORS中间件，让AI添加即可。

接口是第三方的（很常见）：你没法改人家的服务器。正确做法是让你自己的服务端去请求它，网页再请求你的服务端。因为服务器之间的请求不受浏览器CORS限制。这样做还有个大好处：[API Key](/words/api-key)留在服务端，不会暴露给所有访问者。

打开[开发者工具](/words/devtools)的Network（网络）面板可以看到被拦的请求。

> [!warn]不要装「禁用CORS」的浏览器插件当作解决，那只对你自己的浏览器有效，别人访问照样报错。也不要把密钥写进前端去直连第三方。Anthropic官方还特别说明：部分组织账号不允许从浏览器直接调用Claude API，必须经过自己的后端。

## 下次怎么避免

凡是涉及密钥的第三方API，一开始就设计成「网页 → 自己的服务端 → 第三方」。告诉AI这个原则。

## 还不行？

如果报错变成`Failed to fetch`且没有CORS字样，可能是网络或地址问题，看[fetch-failed](/fix/fetch-failed)。401之类的认证问题看[api-401](/fix/api-401)。
