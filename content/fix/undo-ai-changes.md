---
title: AI把项目改坏了，怎么回到上一个能用的版本
slug: undo-ai-changes
summary: Claude Code用/rewind，Cursor点Restore Checkpoint，Git能兜底。先备份现状再回退。
category: AI工具
match: ["改坏了", "回退", "撤销AI的修改", "rewind", "restore checkpoint", "git restore", "回到上一个版本"]
platform: [all]
updated: 2026-11-18
---

```error
（没有固定报错。典型场景：AI改了一大堆文件，页面白屏、功能失效，你想回到它动手之前的样子。）
```

## 人话：这是怎么回事

AI改代码是真的在改你磁盘上的文件，改错了不会自动恢复。好在有三层「后悔药」：AI工具自带的检查点、Git版本记录、你自己的备份。越早用越有效，**先别让AI继续「修」，越修越乱。**

截至2026年11月，下面是各工具当前的做法。

## 最快的解法：把这段发给AI

如果还想让AI帮你，先让它只做检查，不要动文件：

```prompt 复制给 Claude Code
项目被改坏了，我想回到改动之前能用的状态。请先不要修改任何文件。

1. 运行 git status 和 git diff --stat，告诉我改了哪些文件、改了多少。
2. 运行 git log --oneline -10，列出最近的提交。
3. 在回退之前，先帮我把当前状态保存起来：git stash push -u -m "ai-broken-backup"。
4. 然后告诉我回到哪个提交最合适，以及具体命令，等我确认再执行。
说明每个命令会丢失什么。
```

## 想自己动手的话

Claude Code：在输入框为空时按两次 `Esc`，或者输入 `/rewind`（别名 `/undo`），选择想回到的那条消息，再选：

- Restore code and conversation：代码和对话一起退回
- Restore conversation：只退回对话，代码保留
- Restore code：只退回代码，对话保留

局限：官方说明它不跟踪Bash命令对文件的改动，也不跟踪子代理（subagent）的编辑，只是会话级的快速恢复，不替代Git。

Cursor：在对话时间线上点击之前那条请求的 Restore Checkpoint 按钮，文件会回到那个状态。官方说明检查点只撤销Agent的修改，与Git独立，文档里没有列出快捷键。

Git兜底（项目已经用Git提交过才有效）：

```bash
git stash push -u -m "backup"
```

把当前所有改动（包括新文件）暂存起来，工作区回到上次提交。要找回就 `git stash pop`。这是最安全的回退。

```bash
git restore .
```

直接丢弃所有未提交的改动，**无法找回**，只在你确定不要时用。

> [!warn] 回退前先备份现状。`git restore .`、`git reset --hard` 这类命令执行后改动就没了。没把握就用 `git stash`。AI提议执行这些命令时，先让它解释会丢什么。

## 下次怎么避免

让AI动手前先 `git commit`，每个能用的状态都留个存档。这是最便宜的保险，概念见 [rollback](/words/rollback) 和 [commit](/words/commit)。大改动先让AI用 [计划模式](/words/plan-mode) 说方案。

## 还不行？

如果项目从没提交过Git、检查点也过期了，可以去编辑器的本地历史（VS Code的 Timeline）里找旧版本。已经部署到 [Vercel](/words/vercel) 的，可以在部署列表里把旧版本重新设为线上版本。
