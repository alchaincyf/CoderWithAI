---
title: Next.js
slug: nextjs
aka: ["NextJS", "Next", "App Router", "Vercel Next"]
summary: 基于React的全栈框架，同一个项目里能写页面，也能写简单的后端。
category: 网页
level: 2
related: [react, vercel, framework, nodejs, deploy]
updated: 2026-11-18
---

## 一句话
Next.js 是在 [React](/words/react) 之上做的框架，由 Vercel 公司维护。它帮你处理页面路由、打包、前后端写在一起这些麻烦事。

## 打个比方
React 是一堆好用的食材和刀具；Next.js 是配好菜谱、灶台、上菜流程的整间厨房。拎包入住，但得按它的动线走。

不贴切的地方：厨房不会每年大改，Next.js 却会。不同大版本的写法差别可能不小。

## 你会在哪儿碰到它
- AI 新建项目时常默认选它。项目里会看到 app 文件夹（较新的写法叫 App Router），里面有 page.tsx、layout.tsx。
- 文件夹结构就是网址结构：app/about/page.tsx 对应网站的 /about 页面。
- 在本地启动：npm run dev，默认地址通常是 http://localhost:3000，见[开发服务器](/words/dev-server)。
- 发布到 [Vercel](/words/vercel) 最顺手，因为它们是同一家公司的产品。但它也能部署到别处。
- 报错时常见一个叫「.next」的文件夹，这是它生成的中间产物，不用管。

## AI提到它时，你该怎么接
```prompt
这个项目用的是 Next.js。请先告诉我它的版本，以及用的是 App Router 还是 Pages Router。后面改代码时按这个版本的官方文档来，不要用旧写法。
```

```prompt
我想新增一个 /pricing 页面。请告诉我应该新建哪个文件，写完之后我怎么在本地打开它看效果。
```

## 容易踩的坑
- 版本差异。网上很多教程是旧版本写法，和你项目对不上。让 AI 以当前官方文档为准。
- 本地正常，build 失败：Next.js 构建时检查很严格，类型错误或未用变量都可能中断，见[构建](/words/build)。
- 把需要保密的密钥用在会发送到浏览器的代码里。Next.js 有规则：以 NEXT_PUBLIC_ 开头的环境变量会暴露给浏览器，不要用来放密钥。
- 把它当纯静态站，却用了需要服务器的功能，部署到纯静态托管上会失败。
