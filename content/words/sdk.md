---
title: SDK
slug: sdk
aka: ["SDK", "Software Development Kit", "客户端库", "官方库", "package"]
summary: 服务商官方打包好的代码工具箱，让你调用它的接口更省事。
category: 连接
level: 5
related: [api, npm, dependencies, api-key, supabase]
updated: 2026-11-18
---

## 一句话
SDK是服务商写好的代码工具箱。你不必自己拼网络请求，直接调用里面现成的函数，就能用上它的[API](/words/api)。

## 打个比方
API像一份「手写点单格式说明」，SDK像一个已经印好格子的点单本，你只要打勾。两者最后点到的是同一份菜。

不贴切的地方：用SDK等于多带了一个别人写的包，它会增加项目体积，也可能随版本更新出现改动。

## 你会在哪儿碰到它
- AI说「先安装官方SDK」，然后让你运行：

```bash
npm install @supabase/supabase-js
```

- 项目里出现`openai`、`stripe`、`@anthropic-ai/sdk`这类依赖，就是各家的SDK。
- 同一服务常有不同语言的SDK：JavaScript、Python、Swift等。

## AI提到它时，你该怎么接
```prompt 复制给 Claude Code
我要接入<服务名>。请先去官方文档确认：它有没有官方的JavaScript SDK，当前推荐的安装方式和最新用法是什么。
然后按官方文档的写法接入，不要凭记忆写。完成后告诉我装了哪些新依赖，版本号分别是多少。
```

## 容易踩的坑
- AI按旧版本写法：SDK升级后接口会变，网上搜到的示例可能已经过期。让AI对照官方文档与你实际装的版本。
- 凭记忆编造不存在的函数：这是典型的[幻觉](/words/hallucination)，报错`xxx is not a function`时要想到它。
- 装了又不用，越装越多：依赖多了构建慢、也增加风险，见[依赖](/words/dependencies)。
- 把需要保密的[密钥](/words/api-key)配置进了前端使用的SDK里。
