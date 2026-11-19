---
title: 推送（push）与拉取（pull）
slug: push
aka: ["push", "pull", "fetch", "推送", "拉取", "git push", "git pull"]
summary: push把本地存档上传到GitHub，pull把云端新内容下载到本地。
category: 版本管理
level: 2
related: [git, github, commit, repo, branch]
updated: 2026-11-18
---

## 一句话
push是把你本地的提交上传到远程仓库（如[GitHub](/words/github)），pull是把远程的新内容下载下来合并到本地。

## 打个比方
像云盘同步：push是「上传我的修改」，pull是「下载别人或另一台电脑上的修改」。

不贴切的地方：云盘是自动同步的，Git不会。你不push，云端就永远是旧的；你不pull，本地就不知道云端有变化。

## 你会在哪儿碰到它
- 终端里出现：

```bash
git push origin main
git pull
```

- 你在A电脑做了修改，换到B电脑接着做，要先 `git pull`。
- 部署平台连着GitHub：你 `push` 到 `main`，平台就自动重新部署。所以push常等于「发布」。

## AI提到它时，你该怎么接
```prompt 复制给 Claude Code
我想把这轮改动发到线上。请按顺序做：
1. git status看一眼有没有没提交的修改，有就先提交；
2. 先git pull，确认没有冲突；
3. git push到main；
4. 告诉我这次push会不会自动触发线上部署，以及去哪里看部署结果。
如果任何一步出错，把报错原文给我，不要自己强行覆盖。
```

## 容易踩的坑
- 报错 `rejected` / `failed to push some refs`：云端有你本地没有的新内容，先 `pull` 再 `push`。
- `git push --force`（强制推送）会覆盖云端历史，别让AI随手用，除非你明白后果。
- 推到了不该推的分支：自动部署的分支一推就上线，试验性修改要用别的[分支](/words/branch)。
- 推送失败卡住，多半是网络或登录授权问题，不是代码问题。
