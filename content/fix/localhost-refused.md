---
title: "localhost拒绝连接（ERR_CONNECTION_REFUSED）怎么办"
slug: localhost-refused
summary: 浏览器去敲门，但没有服务在里面。通常是开发服务器没启动或端口不对。
category: 页面问题
match: ["ERR_CONNECTION_REFUSED", "localhost 拒绝了我们的连接请求", "localhost refused to connect", "This site can’t be reached", "无法访问此网站", "Unable to connect"]
platform: [all]
updated: 2026-11-18
---

```error
无法访问此网站
localhost 拒绝了我们的连接请求。
ERR_CONNECTION_REFUSED
```

英文版是`This site can’t be reached — localhost refused to connect.`

## 人话：这是怎么回事

[localhost](/words/localhost)指你自己这台电脑。浏览器去`localhost:3000`敲门，却发现里面没有服务在听，于是回你「拒绝连接」。

最常见的原因：开发服务器没启动，或已经崩了、被关了；你输的端口号和实际的不一样。网站本身没问题，是服务没在跑。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code / Codex
我在浏览器打开 http://localhost:3000 提示ERR_CONNECTION_REFUSED。
请先检查开发服务器是否在运行：看终端里有没有npm run dev的进程，或运行lsof -i :3000（Windows用netstat -ano | findstr :3000）。
没在运行就启动它，读终端输出，告诉我它实际监听的地址和端口。
如果启动失败，把报错用人话解释给我并修复。
最后用curl -I对该地址验证返回200，再让我刷新浏览器。
```

## 想自己动手的话

第一步，去运行项目的终端窗口看一眼。

- 窗口里能看到类似`Local: http://localhost:3000`的字样，说明服务在跑，看下一步。
- 窗口关了或有红色报错，重新启动：

```bash
npm run dev
```

第二步，以终端显示的地址为准。端口可能是3000、5173（Vite）、8080等，别凭记忆输。

第三步，把localhost换成127.0.0.1再试：

```text
http://127.0.0.1:3000
```

有时电脑把localhost解析成IPv6地址，而服务只听IPv4，换成数字地址能避开这个问题。

第四步，如果启动时报端口被占用，看[端口被占用](/fix/port-in-use)。

## 下次怎么避免

开发时保持运行服务器的那个终端窗口不关，另开窗口做别的事。

## 还不行？

- 启动命令本身报错：看[Missing script](/fix/missing-script)。
- 能打开但页面空白：看[页面空白](/fix/blank-page)。
- 想了解开发服务器：[开发服务器](/words/dev-server)、[端口](/words/port)。
