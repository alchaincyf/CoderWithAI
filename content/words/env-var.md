---
title: 环境变量（.env）
slug: env-var
aka: ["Environment Variable", ".env", "env", ".env.local", "环境变量"]
summary: 把密钥、地址这类「随环境变化的设置」放在代码之外的配置。
category: 连接
level: 5
related: [api-key, github, deploy, vercel, nodejs]
updated: 2026-11-18
---

## 一句话
环境变量是放在代码之外的一张设置清单：「叫什么名字、值是多少」。代码运行时去清单里读，而不是把密码直接写进代码。

## 打个比方
像把家门钥匙放在门口的地垫下，而不是刻在门上。代码是门，谁都能看到；钥匙放在清单里，各人用各人的。

不贴切的地方：这张清单不是加密保险箱，只是不跟着代码走。清单本身泄露一样危险。

## 你会在哪儿碰到它
- 项目根目录里出现`.env`、`.env.local`、`.env.example`文件。
- 里面是一行行这样的内容：`OPENAI_API_KEY=sk-xxxx`。
- 部署到[Vercel](/words/vercel)等平台时，要在网页后台的Environment Variables里再填一遍。
- 代码里写`process.env.XXX`（Node），或`os.getenv("XXX")`（Python）。

## AI提到它时，你该怎么接
```prompt 复制给 Claude Code
请检查这个项目的环境变量用法：
1. 列出代码里读取了哪些环境变量，每个是做什么的；
2. 生成一份.env.example，只写变量名和说明，不写真实值；
3. 确认.env和.env.local都在.gitignore里；
4. 告诉我部署到Vercel时，需要在后台填哪几个，哪些前端能看到、哪些只在服务端。
```

## 容易踩的坑
- `.env`被提交到GitHub：这是最常见的泄露方式。要先写进`.gitignore`，再创建文件。
- 前端变量并不保密：Next.js里`NEXT_PUBLIC_`开头、Vite里`VITE_`开头的变量，会被打包进网页，所有人可见。密钥不要用这种前缀。
- 本地能跑，上线报错：线上平台不会读你电脑上的`.env`，需要在平台后台单独配置，配好后通常还要重新部署才生效。
- 改了`.env`没反应：本地开发服务器要重启一次才会重新读取。
