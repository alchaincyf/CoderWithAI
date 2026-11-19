---
title: "终端报zsh: command not found（claude、npm、node找不到命令）怎么办"
slug: command-not-found
summary: 电脑没装这个程序，或装了但终端找不到。先确认装没装，再补上路径。
category: 安装与环境
match: ["zsh: command not found: claude", "zsh: command not found: npm", "zsh: command not found: node", "command not found: claude", "zsh: command not found", "command not found"]
platform: [mac]
updated: 2026-11-18
---

```error
zsh: command not found: claude
zsh: command not found: npm
```

## 人话：这是怎么回事

终端在找你输入的那个程序，翻遍了它认识的几个文件夹都没找到，就报了这句。只有两种可能：程序根本没装；或者装了，但它所在的文件夹没登记给终端（这份登记名单叫[PATH](/words/path)）。

这是新手装完东西后最常撞上的错，不是你把电脑搞坏了。

## 最快的解法：把这段发给AI

如果你还能打开另一个能执行命令的AI（比如已经装好的Codex），发给它。装不上Claude Code本身的话，直接看下一节。

```prompt 复制给 Claude Code / Codex
我在Mac终端里运行claude（或npm / node）时报zsh: command not found。
请先帮我判断：是没装，还是装了但不在PATH里。
请依次执行：echo $0、which -a claude npm node、ls ~/.local/bin。
先告诉我诊断结论，再动手修。
修复只改我自己的~/.zshrc，不要用sudo，不要改系统文件。
改完让我开一个新的终端窗口，运行claude --version（或node -v）确认能输出版本号。
```

## 想自己动手的话

第一步，确认你用的是zsh：

```bash
echo $0
```

这条命令显示当前用的是哪种终端外壳。显示zsh就按下面做；显示bash，把下面的 .zshrc换成 .bashrc。

第二步，如果是claude找不到，先看程序在不在：

```bash
ls ~/.local/bin/claude
```

官方安装器把claude放在这里。显示出文件，说明装了、只是没登记；显示No such file，说明没装成功，重新跑官方安装命令（见[Claude Code官方安装页](https://code.claude.com/docs/en/setup)）。

第三步，登记路径（官方文档给的做法）：

```bash
echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc
```

第一行把这个文件夹写进配置文件，第二行让当前窗口立刻生效。

如果是npm、node找不到，说明Node.js没装。去nodejs.org下载LTS版安装包，装完关掉终端重开。

## 下次怎么避免

装完任何命令行工具，都先关掉终端窗口、新开一个再试。旧窗口读的是装之前的名单。

## 还不行？

- 新开窗口仍然找不到：用`grep -n '.local/bin' ~/.zshrc`看那一行有没有真的写进去。
- 提示Node版本问题：看[Node版本太旧](/fix/node-version)。
- Windows电脑：看[npm不是内部或外部命令](/fix/windows-not-recognized)。
- 对[终端](/words/terminal)还不熟，先看词典里的解释。
