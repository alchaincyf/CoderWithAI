---
title: "npm安装报EACCES权限错误怎么办"
slug: npm-eacces
summary: npm想写系统目录被拒。别加sudo，换成写你自己的目录。
category: Node与npm
match: ["EACCES", "npm ERR! code EACCES", "npm error code EACCES", "permission denied, mkdir", "permission denied, access", "errno -13"]
platform: [mac, all]
updated: 2026-11-18
---

```error
npm error code EACCES
npm error syscall mkdir
npm error path /usr/local/lib/node_modules/some-package
npm error errno -13
npm error Error: EACCES: permission denied, mkdir '/usr/local/lib/node_modules/some-package'
```

老版本npm写作`npm ERR! code EACCES`，意思一样。

## 人话：这是怎么回事

你用`npm install -g`（全局安装）时，npm想把东西写进系统级文件夹，而你的账户没有这个文件夹的写入权限，被拒了。这是Mac和Linux上很常见的错，包不坏，电脑也没坏。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code / Codex
我在运行npm install -g时报EACCES permission denied。
请先解释为什么，再诊断：运行npm config get prefix、which node、node -v。
不要使用sudo，不要chmod 777，不要改系统目录权限。
推荐方案：把npm的全局目录改到我自己的目录（npm config set prefix ~/.local，并把~/.local/bin加进~/.zshrc的PATH）。
如果我要装的是Claude Code，优先用官方安装脚本而不是npm。
改完用npm install -g npm-check-updates测试，不报错才算成功。
```

## 想自己动手的话

下面是npm官方文档推荐的第二种做法（最推荐的是用nvm之类的Node版本管理器重装Node，见npm官方文档）。

```bash
npm config set prefix ~/.local
```

把全局安装位置改成你自己的~/.local。

```bash
echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc
```

让终端能找到装在那里的命令。第一行写进配置，第二行让当前窗口生效。

测试：

```bash
npm install -g npm-check-updates
```

这次不应再报EACCES。

> [!warn]网上最流行的两个坏建议：`sudo npm install -g`和`sudo chmod -R 777 /usr/local`。前者以后每次更新都要sudo，还会让文件归root所有，埋下新的权限问题；后者把系统目录对所有人开放，是安全隐患。Claude Code官方文档也明确写了不要用sudo安装。

## 下次怎么避免

装Claude Code用官方原生安装脚本，不走npm全局。其他工具尽量放进项目里装（不加 -g）。

## 还不行？

- Windows上出现EACCES / EPERM：先关掉占用文件的编辑器和杀毒软件再试。
- 报错路径指向项目里的node_modules：删掉它重新`npm install`，见[node_modules](/words/node-modules)。
- Node版本太旧：看[Node版本太旧](/fix/node-version)。
