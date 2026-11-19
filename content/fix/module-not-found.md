---
title: "Cannot find module / Module not found: Can't resolve怎么办"
slug: module-not-found
summary: 代码引用的包或文件找不到。多半是没装依赖、路径写错，或大小写不对。
category: Node与npm
match: ["Cannot find module", "Module not found: Can't resolve", "MODULE_NOT_FOUND", "Error: Cannot find package", "ERR_MODULE_NOT_FOUND"]
platform: [all]
updated: 2026-11-18
---

```error
Error: Cannot find module 'express'
Require stack:
- /Users/you/my-app/server.js
  code: 'MODULE_NOT_FOUND'
```

Next.js、Webpack里长这样：

```error
Module not found: Can't resolve 'framer-motion'
```

## 人话：这是怎么回事

代码里写了「用某个包」或「引入某个文件」，程序去找，找不到。常见原因有四个：这个包没装；你在错的文件夹里运行；文件路径或大小写写错了；[node_modules](/words/node-modules)文件夹坏了。

AI写代码时经常会用到新包，却没告诉你要装，这个错非常普遍。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code / Codex
我运行项目时报Cannot find module（或Module not found: Can't resolve）。
请先读完整报错，告诉我是哪个包或文件找不到、哪个文件在引用它。
然后判断：是package.json里没有这个依赖，还是路径/大小写写错了。
没有依赖就用npm install装上，并确认它写进了package.json；路径错就修正代码。
最后重新运行npm run dev，确认这个错消失了，并说明你改了哪几个文件。
```

## 想自己动手的话

第一步，确认你在项目文件夹里：

```bash
pwd
ls package.json
```

第二步，如果找不到的是包名（比如express），装上它：

```bash
npm install express
```

这条命令下载包，并登记到package.json。

第三步，如果装了还不行，重装全部依赖：

```bash
rm -rf node_modules
npm install
```

第一行删掉node_modules文件夹（它是装下来的，随时能重新生成，不会丢你的代码），第二行重新装。Windows CMD用`rmdir /s /q node_modules`。

第四步，如果找不到的是自己的文件（路径以 ./开头），检查文件名拼写和大小写。Mac不区分大小写，`Button.tsx`写成`button`也能跑，但部署到Linux服务器（如Vercel）就找不到了。

## 下次怎么避免

让AI每次新增依赖后，都告诉你「已执行npm install xxx」。部署前在本地跑一次`npm run build`。

## 还不行？

- 在Vercel上才报：看[Vercel构建失败](/fix/vercel-build-failed)。
- 装包时报冲突：看[ERESOLVE](/fix/npm-eresolve)。
- Python项目的类似报错：看[Python找不到模块](/fix/python-module-not-found)。
