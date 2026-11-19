---
title: "npm报Missing script: \"dev\"怎么办"
slug: missing-script
summary: 当前项目的package.json里没有这个命令。检查文件夹，或换成项目真正有的命令。
category: Node与npm
match: ["Missing script: \"dev\"", "npm error Missing script", "npm ERR! missing script", "Missing script: \"start\"", "Missing script: \"build\"", "To see a list of scripts"]
platform: [all]
updated: 2026-11-18
---

```error
npm error Missing script: "dev"
npm error
npm error To see a list of scripts, run:
npm error   npm run
```

老版本写作`npm ERR! Missing script: "dev"`。

## 人话：这是怎么回事

`npm run dev`的意思是：去package.json里的"scripts"一栏找一条叫dev的命令来执行。找不到，就是这句报错。

原因通常是：你不在正确的项目文件夹；这个项目启动命令不叫dev（可能叫start）；或者项目还没初始化完。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code / Codex
我运行npm run dev报Missing script: "dev"。
请先运行pwd和ls，确认我所在的目录，再读package.json的scripts字段。
告诉我这个项目实际可用的启动命令是什么。
如果我在错的文件夹（比如项目在子文件夹里），告诉我该cd到哪里。
如果项目确实缺少dev脚本，解释应该加什么，并先问我再改。
最后用正确的命令启动项目，告诉我访问地址。
```

## 想自己动手的话

第一步，看你在哪个文件夹，里面有什么：

```bash
pwd
ls
```

要能看到package.json。看不到，说明进错了文件夹，用`cd 文件夹名`进去。

第二步，让npm列出这个项目有哪些命令：

```bash
npm run
```

输出里列的就是你能用的，比如`start`、`build`。用其中的那个：

```bash
npm start
```

第三步，如果是Vite、Next.js项目却没有dev，可能是package.json被改坏了，让AI对照官方模板检查scripts字段。

## 下次怎么避免

每个项目让AI在README开头写清楚「怎么启动」。

## 还不行？

- 提示找不到package.json：看[package.json不存在](/fix/enoent-package-json)。
- 启动后端口被占：看[端口被占用](/fix/port-in-use)。
- 想弄懂npm：[npm](/words/npm)。
