---
title: Vercel部署失败「Command "npm run build" exited with 1」怎么办
slug: vercel-build-failed
summary: 这句只是结果，真正的原因在它上面几十行日志里。先本地跑一遍build，同样报错就能修。
category: 部署上线
match: ["Command \"npm run build\" exited with 1", "exited with 1", "Build failed", "Error: Command", "npm run build", "Failed to compile"]
platform: [all]
updated: 2026-11-18
---

```error
Error: Command "npm run build" exited with 1
```

## 人话：这是怎么回事

Vercel在云端替你运行 `npm run build`（[构建](/words/build)），这一步失败了，所以部署中止。`exited with 1` 只是说「命令失败了」，没说为什么。

真正的原因在这句**上面**的日志里，通常是一行 `Error:`、`Failed to compile` 或 `Type error`。你要做的是找到它。常见原因有：代码里有类型错误或语法错误；少装了依赖；文件名大小写和导入对不上（Mac不区分大小写，Vercel的Linux区分）；缺少 [环境变量](/words/env-var)；Node版本不一致。

## 最快的解法：把这段发给AI

先在Vercel部署页点进失败的那条，展开 Build Logs，复制报错部分，再发：

```prompt 复制给 Claude Code
我的项目在Vercel部署失败，最后一行是 Error: Command "npm run build" exited with 1。真正的报错在下面：

（把Build Logs里"exited with 1"上面的那段红字粘贴到这里）

请：
1. 先在本地运行 npm run build，看能否复现同样的错误。复现不了就对比Node版本和环境变量。
2. 用大白话解释这个错误是什么意思、为什么会出现。
3. 修复根因。不要用关闭类型检查、忽略错误之类的方式绕过去。
4. 修好后再运行 npm run build 确认通过，然后告诉我该提交哪些文件。
```

## 想自己动手的话

```bash
npm run build
```

在本地跑一遍同样的构建。本地也失败，说明是代码问题，看报错的文件名和行号。本地成功但Vercel失败，多半是下面几种：

- 环境变量没在Vercel项目设置里配置，看 [env-missing-in-production](/fix/env-missing-in-production)。
- 文件名大小写不一致，比如代码里写 `./Header`，实际文件叫 `header.js`。
- Node版本不同：在Vercel项目设置里把Node.js版本调成和本地一致，本地用 `node -v` 查看，见 [node-version](/fix/node-version)。
- 依赖没写进 `package.json`，只是你本地碰巧装过。

> [!warn] 看到别人建议「在配置里 ignoreBuildErrors / 忽略 eslint」就能过，别这么做。它只是把报错藏起来，问题会带到线上变成真正的故障。

## 下次怎么避免

每次推送前本地先 `npm run build`。让AI写完功能后加一句「运行构建确认通过再告诉我完成」。

## 还不行？

构建成功了但打开是404，看 [vercel-404](/fix/vercel-404)。依赖安装就失败的，看 [npm-eresolve](/fix/npm-eresolve) 和 [module-not-found](/fix/module-not-found)。把完整的Build Logs发给AI，别只发最后一行。
