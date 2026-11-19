---
title: "npm报ERESOLVE unable to resolve dependency tree怎么办"
slug: npm-eresolve
summary: 两个包对同一个依赖的版本要求打架。用 --legacy-peer-deps或让AI对齐版本。
category: Node与npm
match: ["ERESOLVE unable to resolve dependency tree", "npm error code ERESOLVE", "npm ERR! code ERESOLVE", "Could not resolve dependency", "--legacy-peer-deps", "peer dependency"]
platform: [all]
updated: 2026-11-18
---

```error
npm error code ERESOLVE
npm error ERESOLVE unable to resolve dependency tree
npm error
npm error While resolving: my-app@0.1.0
npm error Found: react@19.0.0
npm error node_modules/react
npm error   react@"^19.0.0" from the root project
npm error
npm error Could not resolve dependency:
npm error peer react@"^18.0.0" from some-library@2.3.1
npm error
npm error Fix the upstream dependency conflict, or retry
npm error this command with --force or --legacy-peer-deps
npm error to accept an incorrect (and potentially broken) dependency resolution.
```

## 人话：这是怎么回事

项目用到的各个包（[依赖](/words/dependencies)）会各自声明「我需要某某包的某个版本」。上面的例子：你的项目用了React 19，而some-library说「我只认React 18」，npm判断这两个说法对不上，拒绝安装。

这在React、Next.js升级后特别常见，不是你的操作出了问题。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code / Codex
我运行npm install报ERESOLVE unable to resolve dependency tree。
请先读完整报错，用人话告诉我是哪两个包的版本冲突了。
然后按这个顺序尝试：先看冲突的库有没有支持新版本的更新版（npm view包名versions）；有就升级它；没有再考虑npm install --legacy-peer-deps。
不要直接加 --force。如果用了 --legacy-peer-deps，要告诉我风险，并跑npm run build验证项目能编译。
```

## 想自己动手的话

先看报错里`Found`和`peer`两行，就是打架的双方。

方案一，升级那个落后的库：

```bash
npm install 库名@latest
```

库的新版本可能已经支持你用的版本了。

方案二，让npm忽略「peer依赖」的严格检查（`--legacy-peer-deps`，npm 6以前就是这个行为）：

```bash
npm install --legacy-peer-deps
```

很多时候项目能正常跑，但如果之后真的出现奇怪的运行错误，这里就是线索。

想让它以后每次都这样，在项目根目录的`.npmrc`文件里加一行`legacy-peer-deps=true`。这样部署平台（如[Vercel](/words/vercel)）安装时也生效。

> [!warn]报错提示里的`--force`是更强的覆盖，会强行装一套npm认为有问题的组合。先试别的，它是最后手段。

## 下次怎么避免

新项目不要随意把主要框架升到最新大版本，除非你用的库都已经跟上。

## 还不行？

- 问题出在Node版本：看[Node版本太旧](/fix/node-version)。
- 装完运行报找不到模块：看[找不到模块](/fix/module-not-found)。
- 部署时安装失败：看[Vercel构建失败](/fix/vercel-build-failed)。
