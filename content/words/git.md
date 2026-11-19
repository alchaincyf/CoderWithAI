---
title: Git
slug: git
aka: ["Git", "版本控制", "git init", "git status"]
summary: 给项目文件拍「存档快照」的工具，改坏了能退回去。
category: 版本管理
level: 1
related: [repo, commit, github, rollback, terminal]
updated: 2026-11-18
---

## 一句话
Git是装在你电脑上的版本管理工具。它帮你给整个项目一次次拍快照，记下谁在什么时候改了什么，需要时可以退回任何一个快照。

## 打个比方
像游戏里的手动存档。打大Boss前先存一个，打挂了读档重来。每次存档还能写一句备注。

不贴切的地方：游戏存档只有一条线，Git可以同时存在多条平行的线（见[分支](/words/branch)）。另外，Git只记录你「主动存」的时刻，没存档的修改，回不去。

## 你会在哪儿碰到它
- 新建项目后，AI问你「要不要初始化git」，或自己跑 `git init`。
- 终端里出现 `git status`、`git add`、`git commit`、`git push`。
- 项目文件夹里多出一个隐藏文件夹 `.git`，里面就是所有存档记录，不要手动动它。
- 提示词里常见的 `.gitignore`，是告诉Git哪些文件不要记录（比如[密钥](/words/env-var)和 `node_modules`）。

## AI提到它时，你该怎么接
```prompt 复制给 Claude Code
这个项目还没有用git管理。请帮我：
1. 初始化git仓库；
2. 先建好合适的.gitignore（要排除.env、node_modules、构建产物）；
3. 做第一次提交，备注写「初始版本」；
4. 之后每完成一个功能，请你主动提醒我提交一次，并告诉我怎么退回上一个版本。
用最少的命令，每条命令说明它做了什么。
```

## 容易踩的坑
- 一直不提交：项目好好的时候没存档，AI一次大改把它搞坏就无法挽回。养成「能跑通就提交」的习惯。
- 先写代码后写 `.gitignore`：密钥已经被记录进历史，后面删文件也还在记录里。
- 把Git和[GitHub](/words/github)当成一回事：Git是本地工具，GitHub是存放Git项目的网站，两者可以独立使用。
- 看见陌生命令就让AI跑：`git reset --hard`、`git clean -fd` 会丢掉没提交的修改，执行前先问它会丢什么。
