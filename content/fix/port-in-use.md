---
title: "EADDRINUSE: address already in use :::3000怎么办"
slug: port-in-use
summary: 3000端口被占用，多半是上一次的服务没关。找到并结束它，或换个端口。
category: 页面问题
match: ["EADDRINUSE", "address already in use", "address already in use :::3000", "Port 3000 is already in use", "listen EADDRINUSE"]
platform: [mac, windows]
updated: 2026-11-18
---

```error
Error: listen EADDRINUSE: address already in use :::3000
```

## 人话：这是怎么回事

电脑上的每个服务要占一个「[端口](/words/port)」，就像门牌号，同一时间一个号码只能给一个服务。你启动[开发服务器](/words/dev-server)要用3000，但这个号已经被占了。

最常见的原因：上次启动的服务没关干净（关了终端窗口但进程还在），或者你开了两个窗口各启动了一遍。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code / Codex
我启动开发服务器时报EADDRINUSE: address already in use :::3000。
请先查出是哪个进程占用了3000端口（Mac用lsof -i :3000，Windows用netstat -ano | findstr :3000），把进程名和PID告诉我。
如果是我之前启动的开发服务（node / next / vite），先征求我同意，再结束该进程。
如果是别的重要程序，不要结束它，改用别的端口启动（如PORT=3001）。
最后确认服务正常启动，并告诉我访问地址。
```

## 想自己动手的话

Mac：

```bash
lsof -i :3000
```

列出占用3000端口的程序，看PID那一列的数字。确认是node之类你自己的开发服务后：

```bash
kill 12345
```

把12345换成你看到的PID。它会礼貌地请这个进程退出。如果过几秒还在，再用`kill -9 12345`强制结束。

Windows PowerShell：

```powershell
netstat -ano | findstr :3000
taskkill /PID 12345 /F
```

第一行找出占用端口的进程号（最后一列），第二行强制结束它。

不想结束它，就换一个端口：

```bash
PORT=3001 npm run dev
```

Windows PowerShell写法：`$env:PORT=3001; npm run dev`。Next.js的开发服务器遇到被占用时，有时会自己提示改用3001，看终端输出的地址。

> [!warn]先看清进程是什么再结束。如果占用端口的是你不认识的系统程序，别kill，换端口。

## 下次怎么避免

关服务用Ctrl+C，不要直接叉掉终端窗口。

## 还不行？

- 结束后启动了，浏览器还打不开：看[localhost拒绝连接](/fix/localhost-refused)。
- 想弄懂localhost：[localhost](/words/localhost)。
