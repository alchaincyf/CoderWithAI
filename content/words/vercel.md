---
title: Vercel
slug: vercel
aka: ["Vercel", "vercel.app", "Vercel CLI", "Hobby", "预览部署"]
summary: 最受欢迎的网站托管平台之一，连上GitHub，推送代码就自动上线。
category: 上线
level: 3
related: [deploy, nextjs, github, domain, serverless]
updated: 2026-11-18
---

## 一句话
Vercel 是一家提供网站托管的公司。你把代码交给它，它负责构建、发布，并给你一个可以访问的网址。

## 打个比方
像一家「全托管的店铺」：你把货（代码）送到，它负责摆货、开门、看门，你不用管水电。

不贴切的地方：全托管的另一面是你不能随意改规则，免费套餐有用量限制，超了要升级或被限制。

## 你会在哪儿碰到它
- 官网 vercel.com，用 GitHub 账号登录。
- 导入 [GitHub](/words/github) 上的一个仓库，Vercel 会识别它是什么框架，自动设置，点 Deploy。
- 之后每次 [push](/words/push) 到主分支，自动发布新版本；推到其他分支，会生成一个预览网址，方便先看再上线。
- 得到的网址形如 xxx.vercel.app。绑定自己的[域名](/words/domain)在项目 Settings 的 Domains 里。
- 环境变量在 Settings 的 Environment Variables 里设置，见[环境变量](/words/env-var)。
- 个人用的 Hobby 套餐免费。截至2026年11月，用量和商用限制请以官方页面 vercel.com/pricing 为准，做商业项目前尤其要看清条款。

## AI提到它时，你该怎么接
```prompt
我想把这个项目部署到 Vercel。请带我完成：把代码推到 GitHub、在 Vercel 导入仓库、设置环境变量、第一次部署。每步告诉我要点哪里，最后帮我确认线上网址能打开。
```

```prompt
Vercel 上这次部署失败了，构建日志如下（粘贴）。请找出原因并修复。修完后告诉我怎么重新部署。
```

## 容易踩的坑
- 国内网络访问 vercel.app 的稳定性因地区、运营商而异，面向国内用户的项目，要自己实际测试，必要时考虑绑自己的域名或换方案。
- 忘记在 Vercel 里配置环境变量，导致线上报错。本地的 .env 文件不会自动上传。
- 误以为在 Vercel 上能随便跑长时间任务。它的函数有执行时长限制，见 [Serverless](/words/serverless)。
- 免费额度被爬虫或攻击用满。上线后偶尔看看用量页面。
