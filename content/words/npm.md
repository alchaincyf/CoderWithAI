---
title: npm与安装包
slug: npm
aka: ["npm", "pnpm", "yarn", "bun", "npm install", "包管理器", "package.json"]
summary: JavaScript世界的「应用商店」和安装工具，用来下载别人写好的代码包。
category: 基础
level: 1
related: [nodejs, dependencies, node-modules, command, dev-server]
updated: 2026-11-18
---

## 一句话
npm 是随 [Node.js](/words/nodejs) 一起装好的工具，用来下载、安装、更新别人写好的代码（叫「包」），也用来运行项目里的各种命令。

## 打个比方
手机的应用商店：想要一个功能，不用自己写，搜到、点安装。npm 的 install 就是「安装」。

不贴切的地方：应用商店有上架审核，npm 上谁都能发包，质量参差不齐，安装前要看看是否主流、是否有人维护。

## 你会在哪儿碰到它
- 项目里的 package.json 是清单，记着项目叫什么、用了哪些包，还有一块 scripts，定义了可以跑的命令。
- 最常见的三条：

```bash
npm install        # 按清单把所有包装好
npm run dev        # 启动开发服务器
npm run build      # 构建
```

- 还会听到 pnpm、yarn、bun，它们是功能类似的替代品，用法几乎一样。项目里有 pnpm-lock.yaml 就用 pnpm，有 package-lock.json 就用 npm，不要混着用。
- 国内网络慢时，AI 可能让你换镜像源，例如把 registry 设为 npmmirror 的地址。这是常见做法，换之前问清楚怎么换回来。

## AI提到它时，你该怎么接
```prompt
请检查这个项目该用 npm、pnpm 还是 yarn（看 lock 文件），然后用对的那个把依赖装好，最后告诉我怎么启动。
```

```prompt
npm install 卡住或报错了，输出如下（粘贴）。请判断是网络问题、版本问题还是别的，并给我最小的修复步骤。
```

## 容易踩的坑
- 在没有 package.json 的文件夹里运行 npm install，会报错找不到文件。先确认位置。
- 看到「vulnerabilities」安全警告就让 AI 执行 audit fix --force。这可能升级到不兼容的版本，把项目搞坏。先问 AI 影响。
- 一个项目里混用 npm 和 pnpm，导致依赖不一致。
- 随便装不明来源的包。让 AI 说明包的作用和知名度。
