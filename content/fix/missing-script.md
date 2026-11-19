---
title: "npm报 Missing script: \"dev\" 怎么办"
slug: missing-script
summary: 当前项目的 package.json 里没有这个命令。检查文件夹，或换成项目真正有的命令。
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

老版本写作 `npm ERR! Missing script: "dev"`。

## 人话：这是怎么回事

`npm run dev` 的意思是：去 package.json 里的 "scripts" 一栏找一条叫 dev 的命令来执行。找不到，就是这句报错。

原因通常是：你不在正确的项目文件夹；这个项目启动命令不叫 dev（可能叫 start）；或者项目还没初始化完。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code / Codex
我运行 npm run dev 报 Missing script: "dev"。
请先运行 pwd 和 ls，确认我所在的目录，再读 package.json 的 scripts 字段。
告诉我这个项目实际可用的启动命令是什么。
如果我在错的文件夹（比如项目在子文件夹里），告诉我该 cd 到哪里。
如果项目确实缺少 dev 脚本，解释应该加什么，并先问我再改。
最后用正确的命令启动项目，告诉我访问地址。
```

## 想自己动手的话

第一步，看你在哪个文件夹，里面有什么：

```bash
pwd
ls
```

要能看到 package.json。看不到，说明进错了文件夹，用 `cd 文件夹名` 进去。

第二步，让 npm 列出这个项目有哪些命令：

```bash
npm run
```

输出里列的就是你能用的，比如 `start`、`build`。用其中的那个：

```bash
npm start
```

第三步，如果是 Vite、Next.js 项目却没有 dev，可能是 package.json 被改坏了，让 AI 对照官方模板检查 scripts 字段。

## 下次怎么避免

每个项目让 AI 在 README 开头写清楚「怎么启动」。

## 还不行？

- 提示找不到 package.json：看 [package.json 不存在](/fix/enoent-package-json)。
- 启动后端口被占：看 [端口被占用](/fix/port-in-use)。
- 想弄懂 npm：[npm](/words/npm)。
