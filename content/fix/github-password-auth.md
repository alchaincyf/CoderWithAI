---
title: GitHub推送报「Support for password authentication was removed」怎么办
slug: github-password-auth
summary: GitHub早已不接受账号密码推送代码。改用登录授权（gh auth login）、访问令牌或SSH。
category: Git
match: ["Support for password authentication was removed", "Authentication failed for", "personal access token", "Invalid username or password", "Password authentication is not supported"]
platform: [all]
updated: 2026-11-18
---

```error
remote: Support for password authentication was removed on August 13, 2021.
remote: Please see https://docs.github.com/get-started/getting-started-with-git/about-remote-repositories#cloning-with-https-urls for information on currently recommended modes of authentication.
fatal: Authentication failed for 'https://github.com/yourname/my-app.git/'
```

## 人话：这是怎么回事

你用`https://`开头的地址推送代码，Git问你要密码，你填了GitHub登录密码。GitHub从2021年8月13日起不再接受这种方式（官方文档原话：Password-based authentication for Git has been removed）。

这不是你的账号出问题，也不是密码错了。换一种「证明你是你」的方式就行。有三种，新手选第一种最省事。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code
我`git push`到GitHub时报错：Support for password authentication was removed，Authentication failed。

请帮我改成用GitHub CLI登录授权：
1. 先运行`git remote -v`和`gh --version`，告诉我现状（没装gh就告诉我怎么装，不要擅自装）。
2. 指导我运行`gh auth login`（选GitHub.com、HTTPS、浏览器登录），这一步需要我在浏览器里点授权，你等我完成。
3. 运行`gh auth setup-git`，让git使用这个登录状态。
4. 再`git push`验证成功。
不要让我把令牌或密码贴到聊天里，也不要把任何令牌写进文件或远程地址。
```

## 想自己动手的话

方式一：GitHub CLI（推荐）。装好`gh`后：

```bash
gh auth login
```

按提示选GitHub.com、HTTPS，然后在浏览器里点授权。

```bash
gh auth setup-git
```

让Git推送时自动用这个登录状态。之后再`git push`即可。

方式二：个人访问令牌（Personal Access Token）。在GitHub网页的Settings里创建，权限给到仓库内容的读写即可。Git再次问密码时，**把令牌当密码填进去**。官方原话是"When Git prompts you for your password, enter your personal access token"。Mac会把它存进钥匙串，下次不用再填。

方式三：SSH。生成密钥、把公钥加到GitHub账号，远程地址换成`git@github.com:用户名/仓库名.git`。适合愿意多花十分钟的人。

> [!warn]令牌等于你的密码。别贴到聊天窗口、别写进代码、别提交到仓库。万一泄露，马上去GitHub把它删除作废，参考[secret-leaked](/fix/secret-leaked)。

## 下次怎么避免

第一次用某台电脑推送前，先`gh auth login`一次，之后基本不会再遇到。

## 还不行？

如果登录时浏览器打不开或超时，多半是网络问题，看[github-connect-timeout](/fix/github-connect-timeout)和[terminal-proxy](/fix/terminal-proxy)。推送本身被拒绝（不是认证问题）看[git-push-rejected](/fix/git-push-rejected)。
