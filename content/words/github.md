---
title: GitHub
slug: github
aka: ["GitHub", "GitHub Pages", "gh", "Pull Request", "Issue"]
summary: 存放Git项目的网站，既是云端备份，也是协作和部署的入口。
category: 版本管理
level: 1
related: [git, repo, push, vercel, api-key]
updated: 2026-11-18
---

## 一句话
GitHub是全世界最大的代码托管网站。你把本地的[仓库](/words/repo)上传到这里，就有了云端备份，也可以和别人协作，还能一键连接到[Vercel](/words/vercel)等平台自动上线。

## 打个比方
像云盘，但专门为项目的历史记录设计：每次存档都在，谁改了什么一清二楚。

不贴切的地方：它是微软旗下的公司服务，不是公共设施。账号、访问速度、政策都由它说了算，国内访问有时较慢或不稳定。

## 你会在哪儿碰到它
- AI让你「先推到GitHub」，终端里出现 `git remote add origin https://github.com/...`。
- Vercel、Cloudflare Pages的「导入项目」，默认让你选一个GitHub仓库。
- 你看到别人的开源项目，上面有 Star（收藏）和 Fork（复制一份到自己名下）。
- 官方命令行工具叫 `gh`，AI用它来建仓库、开Pull Request。

## AI提到它时，你该怎么接
```prompt 复制给 Claude Code
我还没有GitHub账号配置。请带我一步步完成：
1. 检查我电脑上是否已安装git和gh；
2. 引导我用gh auth login登录（告诉我每一步在浏览器里点什么）；
3. 登录成功后，把当前项目推送为一个私有仓库。
每一步先告诉我会发生什么，再执行。
```

## 容易踩的坑
- 把密钥传上去：公开仓库会被机器人扫描，见[API Key](/words/api-key)。
- 上传一直卡住或超时：先检查网络；国内常见，用HTTPS还是SSH、是否需要代理，可以让AI逐项排查。
- 密码登录已不再支持用于Git操作，要用浏览器登录授权或访问令牌（token）。
- 以为「GitHub上有了」就等于「网站上线了」：上线还需要部署，见[部署](/words/deploy)。
