---
title: CLI（命令行工具）
slug: cli
aka: ["CLI", "Command Line Interface", "命令行工具", "Claude Code CLI", "Vercel CLI", "--help"]
summary: 没有图形界面、靠在终端里输入命令来使用的软件。
category: 基础
level: 1
related: [terminal, command, npm, vercel, git]
updated: 2026-11-18
---

## 一句话
CLI 是 Command Line Interface 的缩写，指只能在终端里用命令操作的软件。Claude Code 就是一个典型的 CLI 工具。

## 打个比方
自动售货机和人工柜台的区别。图形软件像柜台，有按钮、菜单，你指着选；CLI 像对着对讲机，必须说对口令，对方才回应。

不贴切的地方：对讲机口令固定，而现在的 CLI 工具（如 Claude Code）允许你直接用自然语言说话，它自己去翻译成命令。

## 你会在哪儿碰到它
- 装 Claude Code、Codex：通常用 npm 全局安装，装好后在终端输入工具的名字启动，见 [npm](/words/npm)。
- 很多服务提供 CLI：Vercel CLI 用来部署，Git 本身也是个命令行工具。
- 几乎所有 CLI 都支持两个通用的求助方式：加 --help 查看用法，加 --version 查看版本号。
- 安装完 CLI 后提示 command not found，是系统找不到它，常因为安装位置不在 PATH 里，或需要重开终端。
- 做一个自己的小工具，也可以做成 CLI，见[脚本](/words/script)。

## AI提到它时，你该怎么接
```prompt
我要使用（工具名）的命令行版本。请告诉我怎么安装、怎么确认安装成功、登录或授权要做什么，以及最常用的 3 个命令各是什么意思。
```

```prompt
终端提示 command not found: （工具名）。请帮我检查：有没有装好、装在哪里、是不是 PATH 的问题，并带我一步步修复。每步都告诉我预期看到什么。
```

## 容易踩的坑
- 安装完不重开终端，提示找不到命令。关掉重开一个窗口再试。
- 全局安装时报权限错误，别急着用管理员权限硬装。让 AI 给出更安全的做法。
- CLI 版本太旧，AI 教的新命令用不了。先看一下版本，必要时升级。
- 在工具里粘贴包含密钥的命令后，终端历史会留下记录。敏感信息尽量放环境变量里，见[环境变量](/words/env-var)。
