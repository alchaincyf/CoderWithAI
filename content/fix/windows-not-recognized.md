---
title: "Windows 报「'npm' 不是内部或外部命令」怎么办"
slug: windows-not-recognized
summary: Node.js 没装或没登记到系统路径。装好后重开终端即可。
category: 安装与环境
match: ["不是内部或外部命令", "is not recognized as an internal or external command", "无法将“npm”项识别为 cmdlet", "is not recognized as the name of a cmdlet", "'node' 不是内部或外部命令", "'claude' is not recognized"]
platform: [windows]
updated: 2026-11-18
---

```error
'npm' 不是内部或外部命令，也不是可运行的程序或批处理文件。
```

PowerShell 里看到的是这种写法：

```error
npm : 无法将“npm”项识别为 cmdlet、函数、脚本文件或可运行程序的名称。请检查名称的拼写，如果包括路径，请确保路径正确，然后再试一次。
```

## 人话：这是怎么回事

Windows 在找 npm 这个程序，没找到。要么 Node.js 没装，要么装了但这个窗口还是装之前打开的，不知道新程序在哪。

这是第一次装开发环境时最常见的错，换个新窗口往往就好了。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code / Codex
我在 Windows 上运行 npm（或 node / claude）报「不是内部或外部命令」。
请先解释原因，再诊断：运行 where.exe node、where.exe npm，并看 $env:PATH 里有没有 nodejs 的安装目录。
如果没装 Node.js，告诉我去哪下载 LTS 版，不要自己用 setx 改环境变量（容易把原有 PATH 截断）。
装好后让我关闭并重开终端，再运行 node -v 和 npm -v，确认都能输出版本号。
```

## 想自己动手的话

先判断装没装。打开 PowerShell：

```powershell
where.exe node
```

这条命令问 Windows：node 在哪。显示一个路径，说明装了，直接跳到第三步；显示 `INFO: Could not find files...`，说明没装。

第二步，去 nodejs.org 下载 Windows 安装包（.msi，选 LTS 版），一路下一步。安装程序默认会把它加进 PATH。

第三步，关掉所有终端窗口，包括 VS Code、Cursor 里的终端，重新打开，再试：

```powershell
node -v
npm -v
```

如果找不到的是 claude，官方文档的做法是把 `%USERPROFILE%\.local\bin` 加进用户 PATH：

```powershell
$currentPath = [Environment]::GetEnvironmentVariable('PATH', 'User')
[Environment]::SetEnvironmentVariable('PATH', "$currentPath;$env:USERPROFILE\.local\bin", 'User')
```

第一行读出现有路径，第二行在后面追加新的，不会覆盖原有的。改完同样要重开终端。

## 下次怎么避免

装完东西先重开终端。编辑器里的终端从编辑器继承路径，要把编辑器整个退出再开。

## 还不行？

- 提示「禁止运行脚本」：看 [PowerShell 禁止运行脚本](/fix/powershell-execution-policy)。
- node 版本不对：看 [Node版本太旧](/fix/node-version)。
- 想弄懂 PATH 是什么：[路径](/words/path)。
