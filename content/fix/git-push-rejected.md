---
title: git push被拒绝（rejected / non-fast-forward / fetch first）怎么办
slug: git-push-rejected
summary: 远程仓库有你本地没有的新内容，Git不让你直接覆盖。先拉取合并再推送，别急着force。
category: Git
match: ["Updates were rejected", "non-fast-forward", "fetch first", "failed to push some refs", "[rejected]", "refusing to merge unrelated histories"]
platform: [all]
updated: 2026-11-18
---

```error
To github.com:yourname/my-app.git
 ! [rejected]        main -> main (fetch first)
error: failed to push some refs to 'github.com:yourname/my-app.git'
hint: Updates were rejected because the remote contains work that you do not
hint: have locally. This is usually caused by another repository pushing to
hint: the same ref. If you want to integrate the remote changes, use
hint: 'git pull' before pushing again.
hint: See the 'Note about fast-forwards' in 'git push --help' for details.
```

## 人话：这是怎么回事

[push](/words/push)是把本地提交送到[GitHub](/words/github)。被拒绝说明：远程仓库里有你本地没有的提交（可能是你在网页上改过文件、在另一台电脑推过，或者建仓库时勾选了自动生成README）。

Git不允许你直接盖过去，怕把别人的内容弄丢。它是在保护你，不是出故障了。解法是先把远程的内容合并进来，再推送。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code
我执行git push被拒绝了，报错里有"Updates were rejected"和"fetch first"（或non-fast-forward）。

请按这个顺序处理，并且每一步先告诉我在做什么：
1. 运行git status、git log --oneline -5、git remote -v，确认现状。
2. 运行git fetch，对比git log --oneline origin/main..HEAD和HEAD..origin/main，告诉我本地和远程各多了哪些提交。
3. 用git pull --rebase合并远程的新内容。如果出现冲突，停下来解释冲突在哪，不要擅自选择保留哪一边。
4. 再git push。
严禁使用git push --force。完成后用git status和git log验证本地与远程一致。
```

## 想自己动手的话

```bash
git pull --rebase
```

先把远程的新提交拿下来，再把你的提交接在后面。如果提示冲突，看[git-merge-conflict](/fix/git-merge-conflict)。

```bash
git push
```

合并完成后再推送，通常就通过了。

如果报错是`fatal: refusing to merge unrelated histories`，说明本地和远程是两个各自独立开始的项目（典型情况：本地先`git init`，GitHub上又新建了带README的仓库）。确认远程内容可以合并后：

```bash
git pull origin main --allow-unrelated-histories
```

这条命令允许把两段互不相关的历史拼在一起。

> [!warn]网上常见的`git push --force`会用你的版本强行覆盖远程。如果远程有别人（或另一台电脑上的你）的提交，它们会直接消失。多人协作的分支、`main`分支上不要用。只有在你确定这是自己独占的分支、而且知道自己在丢什么时才用，并优先用`--force-with-lease`。

## 下次怎么避免

开工前先`git pull`。一个人用多台电脑时，换机器前先推送、到新机器先拉取。在GitHub网页上直接改文件也会造成这个情况，改完记得本地同步。

## 还不行？

如果报错变成了「Authentication failed」，看[github-password-auth](/fix/github-password-auth)。如果连不上GitHub，看[github-connect-timeout](/fix/github-connect-timeout)。
