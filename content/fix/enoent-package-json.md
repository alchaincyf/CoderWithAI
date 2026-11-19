---
title: "ENOENT: no such file or directory, open package.json怎么办"
slug: enoent-package-json
summary: 你所在的文件夹里没有package.json，多半是进错了文件夹。
category: Node与npm
match: ["Could not read package.json", "ENOENT: no such file or directory, open", "package.json'", "npm error code ENOENT", "npm ERR! code ENOENT", "enoent ENOENT"]
platform: [all]
updated: 2026-11-18
---

```error
npm error code ENOENT
npm error syscall open
npm error path /Users/you/package.json
npm error errno -2
npm error enoent Could not read package.json: Error: ENOENT: no such file or directory, open '/Users/you/package.json'
npm error enoent This is related to npm not being able to find a file.
```

## 人话：这是怎么回事

npm在你当前所在的文件夹里找package.json（项目的「身份证」文件），没找到。看报错里的路径：如果它停在`/Users/你/`这种个人主目录，说明终端打开时就在主目录，没有进到项目文件夹。

这是新手最容易遇到的错，一步`cd`就能解决，项目没有坏。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code / Codex
我运行npm命令时报ENOENT ... open package.json。
请先运行pwd和ls，告诉我当前所在文件夹以及里面有什么。
然后在附近找我的项目：查找名字含package.json的位置（排除node_modules里的），列出候选项目文件夹。
告诉我应该cd到哪个文件夹，进去后再执行我原来的命令，验证成功。
不要在错的位置创建新的package.json。
```

## 想自己动手的话

第一步，看当前位置：

```bash
pwd
```

Windows CMD用`cd`，PowerShell用`pwd`。

第二步，看里面有没有package.json：

```bash
ls
```

Windows CMD用`dir`。

第三步，进到项目文件夹：

```bash
cd ~/Documents/my-app
```

把路径换成你项目的位置。小技巧：在Mac的访达里把文件夹拖进终端窗口，路径会自动填上。

有时项目套了一层，package.json在子文件夹里（比如`my-app/frontend/`），逐层`cd`进去。

第四步，如果这个项目文件夹里确实没有package.json，说明它不是Node项目，或者还没初始化，让AI看一下项目是什么类型。

## 下次怎么避免

用编辑器（Cursor、VS Code）的内置终端，它默认就在项目文件夹。用Claude Code之前，先`cd`到项目里再启动。

## 还不行？

- 进对了文件夹却提示缺少脚本：看[Missing script](/fix/missing-script)。
- 弄不清文件夹和路径：[路径](/words/path)、[终端](/words/terminal)。
- 提示不是git仓库：看[not a git repository](/fix/git-not-a-repo)。
