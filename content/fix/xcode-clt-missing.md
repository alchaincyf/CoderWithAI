---
title: "xcrun: error: invalid active developer path 怎么办（Mac）"
slug: xcode-clt-missing
summary: Mac 缺少或损坏了命令行开发工具，一条命令重新安装。
category: 安装与环境
match: ["xcrun: error: invalid active developer path", "missing xcrun at", "xcode-select: error: command line tools are already installed", "CommandLineTools", "xcode-select --install"]
platform: [mac]
updated: 2026-11-18
---

```error
xcrun: error: invalid active developer path (/Library/Developer/CommandLineTools), missing xcrun at: /Library/Developer/CommandLineTools/usr/bin/xcrun
```

## 人话：这是怎么回事

git、make、编译 Python 包这些基础工具，在 Mac 上由苹果的「命令行开发工具」提供。macOS 大版本升级后，这套工具经常被系统清掉或失效，你再用 git 就会报这句。

这很常见，不是你误删了什么。重装一次即可。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code / Codex
我的 Mac 运行 git 时报 xcrun: error: invalid active developer path。
请先解释原因，然后运行 xcode-select -p 和 xcode-select --install 触发安装。
安装需要我在弹出的窗口里点「安装」，你提示我操作，并等我确认完成后再继续。
装完运行 git --version 和 xcrun --version 验证，不要用 sudo 删除或移动任何系统目录。
```

## 想自己动手的话

在终端运行：

```bash
xcode-select --install
```

这条命令请求安装苹果的命令行工具。屏幕会弹出一个窗口，点「安装」，同意协议，等下载安装完成（几分钟到十几分钟，取决于网速）。

装完验证：

```bash
git --version
```

能输出版本号就好了。

如果提示 `command line tools are already installed`，但仍然报错，说明路径指向错了，试试重置：

```bash
sudo xcode-select --reset
```

这条把开发者目录恢复成默认值，需要输入开机密码，输入时屏幕不显示字符，属正常。

## 下次怎么避免

每次 macOS 大版本升级后，第一次用终端前先跑一次 `xcode-select --install`。

## 还不行？

- 弹窗卡在「正在搜索软件」或下载失败：网络问题，换个网络或稍后再试，见 [终端不走代理](/fix/terminal-proxy)。
- 弹窗提示找不到更新：去 developer.apple.com 的下载页登录后手动下载 Command Line Tools（以苹果官网为准）。
- 做 iOS 开发遇到签名问题：看 [Xcode 签名](/fix/xcode-signing)。
