---
title: "Permission denied / Operation not permitted怎么办（Mac隐私权限、文件权限）"
slug: permission-denied-file
summary: 程序没有权限读写那个文件。分清是文件权限，还是Mac的隐私保护。
category: 安装与环境
match: ["Permission denied", "Operation not permitted", "PermissionError: [Errno 13]", "EPERM", "zsh: permission denied", "chmod +x"]
platform: [mac, all]
updated: 2026-11-18
---

```error
zsh: permission denied: ./start.sh
```

另外两种常见写法：

```error
PermissionError: [Errno 13] Permission denied: '/Users/you/Desktop/data.csv'
ls: Desktop: Operation not permitted
```

## 人话：这是怎么回事

这句话有两个来源，要分开看。

第一种叫Permission denied：文件本身没有被允许使用，比如脚本没有「可执行」标记，或文件属于别的账户。

第二种叫Operation not permitted：多发生在Mac。系统的隐私保护不让终端（或Cursor、VS Code等程序）访问桌面、文稿、下载等文件夹，这和文件权限无关，要在系统设置里放行。

两种都是正常的保护，不是电脑出了故障。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code / Codex
我在Mac上执行命令时报Permission denied（或Operation not permitted）：

（在这里粘贴完整报错，含命令和路径）

请先运行`ls -l`看这个文件的权限，再判断是文件权限问题，还是macOS隐私权限（Operation not permitted通常是后者）。
文件权限问题：只对这个文件做最小修改，如chmod +x文件名。
隐私权限问题：告诉我到系统设置的哪个位置、给哪个应用放行，由我自己手动点，你不要尝试绕过。
禁止使用`sudo chmod -R 777`，禁止关闭SIP或任何系统保护。验证原命令能成功。
```

## 想自己动手的话

先分辨是哪种。看完整报错：

```bash
ls -l start.sh
```

输出最前面的`-rw-r--r--`里没有x，说明没有可执行权限。

文件权限问题（脚本没法运行）：

```bash
chmod +x start.sh
```

给这个文件加上「可执行」。只改这一个文件。

Mac隐私权限问题（Operation not permitted）：

1. 打开「系统设置」，进入「隐私与安全性」。
2. 找到「完全磁盘访问权限」，或「文件和文件夹」。
3. 把你正在用的终端程序（终端、iTerm、Cursor、VS Code等）打开开关。
4. 彻底退出该程序再重新打开，让设置生效。

菜单名称会随macOS版本略有不同，以你系统界面为准。

> [!warn]网上常见的建议：`sudo chmod -R 777 某个文件夹`，或关闭系统完整性保护（SIP）。前者让所有人都能改你的文件，后者会拆掉系统的防护层。这两个办法都没必要，上面的最小修改就够。

## 下次怎么避免

项目放在文稿或桌面之外的普通文件夹（如~/Projects），减少触发隐私弹窗。

## 还不行？

- 安装npm全局包时权限不够：看[EACCES](/fix/npm-eacces)。
- 文件被别的程序占用：关掉占用它的程序再试。
- 想了解命令和终端：[命令](/words/command)、[终端](/words/terminal)。
