---
title: "npm安装卡住或报ETIMEDOUT、ECONNRESET怎么办（国内网络）"
slug: npm-network-timeout
summary: 连不上npm官方源。换成npmmirror镜像，或检查代理设置。
category: 网络
match: ["ETIMEDOUT", "ECONNRESET", "network request to https://registry.npmjs.org", "npm error network", "npm ERR! network", "socket hang up", "ESOCKETTIMEDOUT"]
platform: [all]
updated: 2026-11-18
---

```error
npm error code ETIMEDOUT
npm error syscall connect
npm error errno ETIMEDOUT
npm error network request to https://registry.npmjs.org/react failed, reason: connect ETIMEDOUT 104.16.x.x:443
npm error network This is a problem related to network connectivity.
```

还有一种是`ECONNRESET`（连接被中途断开），或者什么都不报，进度一直卡着。

## 人话：这是怎么回事

npm默认从海外的registry.npmjs.org下载依赖包。国内网络访问它时常常很慢或中断，于是超时。你的代码和电脑都没问题，是路上堵了。

最常用的办法是换成国内的镜像源，它同步了官方的内容。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code / Codex
我在国内运行npm install卡住或报ETIMEDOUT / ECONNRESET。
请先解释原因，再运行npm config get registry、npm config get proxy、npm config get https-proxy。
然后把registry设成 https://registry.npmmirror.com，再重新 npm install。
不要改动我项目里的代码和package-lock.json以外的文件。
安装成功后告诉我怎么改回官方源，并验证npm run dev能启动。
```

## 想自己动手的话

查看现在用的源：

```bash
npm config get registry
```

换成npmmirror（npmmirror.com官网给出的镜像地址）：

```bash
npm config set registry https://registry.npmmirror.com
```

重新安装：

```bash
npm install
```

需要改回官方源：

```bash
npm config set registry https://registry.npmjs.org/
```

如果你的电脑已经在用代理，注意npm本身可能没有走它，可以设置：

```bash
npm config set proxy http://127.0.0.1:7890
npm config set https-proxy http://127.0.0.1:7890
```

端口号7890只是示例，换成你自己代理软件显示的端口。不用了用`npm config delete proxy`和`npm config delete https-proxy`清掉，否则代理软件一关，npm会全部连不上。

另外：有些包（如electron、puppeteer）会在安装时额外从GitHub下载文件，换registry管不到这一段，需要它们各自的镜像设置，以对应包的文档为准。

## 下次怎么避免

新电脑第一天就把registry配好。项目里如果有`.npmrc`，它的优先级比全局配置高，改源时留意一下。

## 还不行？

- 提示证书错误：看[证书错误](/fix/ssl-certificate-error)。
- 浏览器能上外网、终端不行：看[终端不走代理](/fix/terminal-proxy)。
- 部署在Vercel上国内打不开：看[Vercel在国内被屏蔽](/fix/vercel-blocked-in-china)。
