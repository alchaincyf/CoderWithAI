---
title: "JavaScript heap out of memory怎么办"
slug: heap-out-of-memory
summary: Node用的内存到了上限。先调高上限应急，再找出真正吃内存的原因。
category: Node与npm
match: ["JavaScript heap out of memory", "FATAL ERROR: Reached heap limit", "Allocation failed", "Last few GCs", "ERR_WORKER_OUT_OF_MEMORY", "heap limit"]
platform: [all]
updated: 2026-11-18
---

```error
<--- Last few GCs --->
...
FATAL ERROR: Reached heap limit Allocation failed - JavaScript heap out of memory
```

## 人话：这是怎么回事

[Node.js](/words/nodejs)给自己设了一个内存上限。项目在构建（[build](/words/build)）或运行时需要的内存超过了它，就当场崩掉，输出这句。

两种可能：项目确实大，需要更多内存；或者代码里有个死循环、一次读了超大文件，在不停吃内存。前者调高上限就行，后者要找原因。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code / Codex
我运行npm run build（或dev）时报JavaScript heap out of memory。
请先告诉我这台电脑的内存大小和node -v，然后用NODE_OPTIONS=--max-old-space-size=4096重试一次，确认是否只是上限不够。
如果调高后仍然崩，或者内存涨得异常快，请帮我排查：是否有死循环、一次加载超大文件或大量图片、是否把node_modules或构建产物纳入了扫描。
不要把上限设得超过电脑物理内存。
最后给出结论：是项目确实需要更多内存，还是代码有问题，并验证构建成功。
```

## 想自己动手的话

Mac / Linux，调高到约4GB（单位是MB）：

```bash
NODE_OPTIONS=--max-old-space-size=4096 npm run build
```

这条命令只对这一次运行有效。

Windows PowerShell：

```powershell
$env:NODE_OPTIONS = "--max-old-space-size=4096"
npm run build
```

如果调到4096才成功，说明项目偏大，可以接受。数字不要超过你电脑实际内存的一半左右，否则整台电脑会卡死。

如果调高后还是崩，或者内存涨到上限的速度很快，就要找原因了：

1. 看终端崩溃前有没有无限重复的输出（死循环的信号）。
2. 检查项目里有没有特别大的文件（几百MB的视频、数据文件放进了代码目录）。
3. 把报错和上面的情况发给AI，让它逐项排查。

在Vercel等平台上构建时遇到同样的错，平台有各自的内存限额，以其官方文档为准，先从减少构建内容入手。

## 下次怎么避免

大文件、视频、数据集不要放进代码目录，放到对象存储里引用。

## 还不行？

- 线上构建失败：看[Vercel构建失败](/fix/vercel-build-failed)。
- 电脑本身卡死、风扇狂转：关掉其他大程序，重启电脑后再试。
- 不确定终端怎么用：[终端](/words/terminal)。
