---
title: 构建（build）
slug: build
aka: ["Build", "npm run build", "打包", "编译", "bundle", "构建失败"]
summary: 把你写的代码整理、检查、压缩成可以上线的最终版本。
category: 网页
level: 2
related: [dev-server, deploy, log, framework, vercel]
updated: 2026-11-18
---

## 一句话
构建是把开发时的代码，加工成真正能发布的文件：检查错误、转换格式、压缩体积、打包到一起。

## 打个比方
出书。作者写的稿子（源代码）有批注、有草稿；出版前要校对、排版、印刷，出来的成书才发到读者手里。

不贴切的地方：印刷厂不会因为你有一个错别字就拒印，而构建经常会，一个类型错误就可能整次失败。

## 你会在哪儿碰到它
- 运行：

```bash
npm run build
```

- 成功后会生成一个输出文件夹，名字依框架而定，常见的有dist、build、.next、out。
- 把项目发到[Vercel](/words/vercel)这类平台，它们会在云端替你执行构建。构建失败，网站就不会更新，旧版本继续在线。
- 在Vercel的部署页面会看到Build Logs，构建失败时红色错误就写在这里。
- 本地[开发服务器](/words/dev-server)里正常，构建却失败，很常见。

## AI提到它时，你该怎么接
```prompt
请在本地运行一次构建，保证能通过再让我部署。如果有报错，请按错误原文逐条修复，不要用关闭检查的方式绕过，修完再跑一次确认。
```

```error
Failed to compile.
Type error: ...
Error: Command "npm run build" exited with 1
```

遇到上面这样的失败，把完整日志（不是最后一行）发给AI：

```prompt
部署时构建失败了，完整日志如下（粘贴）。请找出第一个真正的错误，告诉我是什么原因，怎么修。
```

## 容易踩的坑
- 只看最后一行。真正的原因往往在红字的最前面，见[日志](/words/log)。
- 本地是macOS，线上是Linux，文件名大小写在线上区分，Logo.png和logo.png本地没事，线上会找不到。
- 线上构建缺少[环境变量](/words/env-var)，导致失败。
- 为了通过构建而加忽略错误的开关，类型错误只是被藏起来，之后会以更难查的方式爆发。
