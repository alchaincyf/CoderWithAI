---
title: "报错「fatal: not a git repository」怎么办"
slug: git-not-a-repo
summary: 你在一个没有被Git管理的文件夹里敲了git命令。先确认位置对不对，再决定要不要初始化。
category: Git
match: ["fatal: not a git repository", "not a git repository (or any of the parent directories)", "not a git repository", ".git"]
platform: [all]
updated: 2026-11-18
---

```error
fatal: not a git repository (or any of the parent directories): .git
```

## 人话：这是怎么回事

[Git](/words/git)要靠项目文件夹里一个叫`.git`的隐藏文件夹记录历史。这句话的意思是：你现在所在的文件夹（和它上面几层）都找不到这个`.git`，所以Git不知道你要管理哪个项目。

九成的情况只是**终端开错了位置**，代码没丢，也没坏。剩下的是这个项目确实还没初始化过Git。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code
我在终端运行git命令时报错：fatal: not a git repository (or any of the parent directories): `.git`

请先不要运行`git init`。按顺序做：
1. 运行pwd和`ls -a`，告诉我当前在哪个目录、里面有没有`.git`。
2. 如果当前目录不对，帮我找到真正的项目目录（上一层或下一层，找带`.git`的）。
3. 如果确认这个项目从来没用过Git，再告诉我为什么要初始化，我同意后再`git init`并做第一次提交。
全程用大白话解释，改完用`git status`验证。
```

## 想自己动手的话

先看你在哪：

```bash
pwd
```

`pwd`会打印当前所在的文件夹路径。

```bash
ls -a
```

`-a`会把隐藏文件也列出来，看有没有`.git`。Windows的PowerShell用`ls -Force`。

如果没有，用`cd`进到真正的项目文件夹再试（把路径换成你的）：

```bash
cd ~/Documents/my-project
git status
```

`git status`能正常输出分支信息，就说明位置对了。

如果这个项目确实从没用过Git，才初始化：

```bash
git init
```

这条命令会在当前文件夹里创建`.git`，从此开始管理。

> [!warn]别在家目录或桌面这种大文件夹里`git init`。Git会把里面所有东西都当成项目，后面提交时会混进一堆无关文件，甚至带上密码和私人资料。初始化前先确认`pwd`显示的就是项目文件夹。

## 下次怎么避免

打开终端后先习惯性敲一下`pwd`。用Claude Code这类工具时，从项目文件夹里启动它，它的工作目录就是对的。

## 还不行？

如果`pwd`位置对、`ls -a`里也有`.git`，还报这个错，可能是`.git`被误删或损坏了。不要自己乱试，把`ls -a`的输出发给AI，让它先判断再动手。命令本身的问题可以看[command-not-found](/fix/command-not-found)。
