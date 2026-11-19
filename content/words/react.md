---
title: React
slug: react
aka: ["React.js", "ReactJS", "组件", "JSX", "TSX"]
summary: 最流行的前端工具库，把页面拆成一个个可复用的「组件」来搭。
category: 网页
level: 2
related: [nextjs, frontend, framework, web-basics, node-modules]
updated: 2026-11-18
---

## 一句话
React是一个用来做网页界面的JavaScript工具库，由Meta（原Facebook）开源。它的核心想法是：页面由许多小零件（组件）拼成。

## 打个比方
乐高。按钮是一块积木，卡片是一块积木，导航栏也是。做好一块，到处都能用；改一块的样子，所有用到它的地方一起变。

不贴切的地方：乐高拼好是死的，React的组件会根据数据变化自己重新「长」出新样子。

## 你会在哪儿碰到它
- AI说「我用React写」，你会看到 .jsx、.tsx后缀的文件，里面JavaScript里混着长得像HTML的标签。
- 文件夹里常有components，每个文件是一个组件，比如Header、Footer。
- AI说「把这个状态存在state里」，意思是让组件记住某个会变的值，比如输入框里的文字。
- 大部分你让AI新做的网页，如果没特别指定，很可能是React加上[Next.js](/words/nextjs)或Vite。

## AI提到它时，你该怎么接
```prompt
请用React做这个页面：（描述）。把页面拆成几个组件，并用一张简单的列表告诉我每个组件负责什么，方便我之后说「改某某组件」时你知道指哪个。
```

```prompt
我想改页面上的某个部分，但不知道它叫什么。我把截图发给你，请告诉我它对应哪个组件、在哪个文件里。
```

## 容易踩的坑
- 一个组件越写越长，AI容易改乱。当文件长到几百行，让AI帮你拆分。
- React的版本更新后，一些旧写法被淘汰。报错里出现deprecated，是在提醒你，让AI按新版改。
- 页面一闪一闪、无限刷新，往往是代码让组件反复重绘。把现象和[控制台](/words/devtools)里的报错一起发给AI。
- React只管界面，路由、数据请求、部署它都不管，所以才有Next.js这类上层框架。
