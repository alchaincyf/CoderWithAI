---
title: 本地好好的，上线后报错或拿到undefined（环境变量没配）
slug: env-missing-in-production
summary: .env文件不会上传，线上要在平台里单独配环境变量，配完还得重新部署才生效。
category: 部署上线
match: ["undefined", "process.env", "is not defined", "API key is missing", "Missing environment variable", "NEXT_PUBLIC_"]
platform: [all]
updated: 2026-11-18
---

```error
Error: Missing API key. Pass it via the `apiKey` parameter or set the OPENAI_API_KEY environment variable.
```

或者页面上显示`undefined`，控制台里`process.env.XXX`是空的。报错原文因项目而异，共同点是：本地正常，线上不行。

## 人话：这是怎么回事

你本地项目里有个`.env`文件，存着密钥和各种配置。这个文件按规矩**不会提交到GitHub**（也不该提交），所以[Vercel](/words/vercel)这类平台拿到的代码里根本没有它。线上读不到[环境变量](/words/env-var)，就得到`undefined`或直接报错。

这不是代码的错。解法是在平台上把同样的变量再配一遍。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code
我的项目本地正常，部署到Vercel后报错/拿到undefined，我怀疑是线上没配环境变量。

请：
1. 搜索代码里所有用到process.env（或import.meta.env）的地方，列出变量名清单，并标出哪些是给浏览器用的（Next.js的NEXT_PUBLIC_开头，Vite的VITE_开头）。
2. 对比本地`.env`文件，告诉我线上需要配置哪些变量名。只列变量名，不要打印变量的值。
3. 检查有没有把真正的密钥放在会暴露给浏览器的变量里（比如NEXT_PUBLIC_开头），有就指出来，并改成只在服务端使用。
4. 告诉我在Vercel里怎么配、配完怎么重新部署。
```

## 想自己动手的话

1. 打开Vercel项目，进Settings，找到Environment Variables。
2. 逐个添加`.env`里的变量，名字必须完全一致。注意选对环境：Production（线上正式）、Preview（预览分支）、Development（本地）。
3. **重新部署一次。**官方文档明确：修改环境变量不会影响已有的部署，只对新部署生效。在Deployments里对最新那条选择Redeploy。

有一种特殊情况：Next.js里名字以`NEXT_PUBLIC_`开头的变量，是在构建时直接写进网页代码里的。如果浏览器里是`undefined`，要确认变量名带了这个前缀、写得完整（不要用`process.env[name]`这种动态写法）、并且在配置后重新构建过。

> [!warn] `NEXT_PUBLIC_`、`VITE_`开头的变量会暴露给所有访问网站的人。真正的密钥（OpenAI Key、数据库密码、Supabase的secret key，旧名service_role）不要放进去，应该只在服务端代码里读取。

## 下次怎么避免

新增任何环境变量，同时做两件事：写进`.env.example`（只放变量名），并去线上平台补上。每次部署前让AI对照检查一遍。

## 还不行？

确认配了、也重新部署了，还是不对：检查变量名有没有多余空格、值里有没有换行。查看线上日志，见[log](/words/log)。构建阶段就报错看[vercel-build-failed](/fix/vercel-build-failed)。
