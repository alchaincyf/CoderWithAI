---
title: "Node版本太旧：Unsupported engine、语法报错怎么办"
slug: node-version
summary: 项目要求的 Node 比你电脑上的新。装一个 LTS 版本，或用版本管理器切换。
category: Node与npm
match: ["Unsupported engine", "EBADENGINE", "The engine \"node\" is incompatible", "Node.js version", "SyntaxError: Unexpected token '?'", "requires Node"]
platform: [all]
updated: 2026-11-18
---

```error
npm warn EBADENGINE Unsupported engine {
npm warn EBADENGINE   package: 'next@15.x.x',
npm warn EBADENGINE   required: { node: '>=18.18.0' },
npm warn EBADENGINE   current: { node: 'v16.20.0', npm: '8.19.4' }
npm warn EBADENGINE }
```

或者运行时直接崩：

```error
SyntaxError: Unexpected token '?'
```

## 人话：这是怎么回事

[Node.js](/words/nodejs) 是让 JavaScript 在你电脑上跑起来的引擎，它会更新。AI 生成的项目通常用较新的写法，电脑里装的却是几年前的老版本，对不上就报错。错误里写了「需要的版本」和「你现在的版本」，对照一下就知道差多少。

升级 Node 不会弄坏你的项目，常见且可逆。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code / Codex
我的项目报 Node 版本不满足（Unsupported engine / EBADENGINE，或 SyntaxError）。
请先运行 node -v、npm -v，读项目 package.json 里的 engines 字段，再告诉我项目需要哪个版本、我现在是哪个版本。
给我最稳的升级方案（推荐用 nvm 或 fnm 这类版本管理器，不要手动删系统文件）。
如果项目有 .nvmrc 就按它的版本装。升级后重新 npm install，并运行 npm run dev 验证能启动。
```

## 想自己动手的话

第一步，看自己的版本：

```bash
node -v
```

第二步，选一种升级方式：

最直接：去 nodejs.org 下载 LTS 版安装包，覆盖安装。LTS 是长期支持版，适合绝大多数人。装完关掉终端重开，再执行 `node -v`。

想要同时留多个版本：用版本管理器。Mac/Linux 用 nvm（nvm-sh/nvm 的 GitHub 页面有最新安装命令），Windows 用 nvm-windows。装好后：

```bash
nvm install --lts
nvm use --lts
```

第一行装最新 LTS，第二行切换过去。项目里有 `.nvmrc` 时，在项目目录执行 `nvm use` 就会切到它指定的版本。

第三步，重装依赖：

```bash
npm install
```

版本变了以后，让 npm 按新环境重新安装一遍。

补充一点：Claude Code 的 npm 安装方式要求 Node.js 22 或更高（官方文档，截至2026年11月），更推荐直接用官方原生安装脚本，不依赖 Node。

## 下次怎么避免

新项目开始时，让 AI 在 README 里写明 Node 版本，并生成 .nvmrc。

## 还不行？

- 装完 `node -v` 还是旧版本：同时装了两份，用 `which -a node`（Windows 用 `where.exe node`）看有几个。
- 找不到命令：看 [command not found](/fix/command-not-found)。
- 依赖冲突：看 [ERESOLVE](/fix/npm-eresolve)。
