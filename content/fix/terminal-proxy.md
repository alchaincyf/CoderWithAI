---
title: "浏览器能翻墙、终端不行怎么办（终端不走代理）"
slug: terminal-proxy
summary: 终端里的程序不读系统代理。给终端单独设置代理环境变量。
category: 网络
match: ["Failed to connect to 127.0.0.1 port 7890", "Could not resolve host", "curl: (28)", "curl: (7) Failed to connect", "HTTPS_PROXY", "proxy"]
platform: [mac, windows]
updated: 2026-11-18
---

```error
curl: (28) Failed to connect to github.com port 443 after 75003 ms: Couldn't connect to server
```

另一种相反的情况：代理软件关了，终端还在连旧代理。

```error
curl: (7) Failed to connect to 127.0.0.1 port 7890 after 0 ms: Couldn't connect to server
```

## 人话：这是怎么回事

你在浏览器里打开了代理软件的系统代理，网页能访问。但终端里的git、npm、curl这些程序并不看系统代理设置，它们只认「环境变量」。所以浏览器通、终端不通。

做法是在终端里单独告诉它们：代理在哪个本地端口。这里默认你使用的是合法合规、自己有权使用的网络环境（比如公司提供的）。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code / Codex
我的浏览器能正常访问外网，但终端里git/npm/curl连不上。我的代理软件本地端口是 【在此填写，在代理软件主界面里看】。
请先解释原因，然后在当前终端会话里设置HTTPS_PROXY、HTTP_PROXY（只对当前窗口生效，不要写进系统配置）。
设置后用curl -I https://github.com 验证，并告诉我如何取消设置。
不要帮我下载或推荐任何代理软件，也不要关闭SSL校验。
```

## 想自己动手的话

先在代理软件的界面里找本地端口（常见的有7890、7897、1080等，各软件不同，以你软件显示为准）。

Mac终端，只对当前窗口生效：

```bash
export https_proxy=http://127.0.0.1:7890
export http_proxy=http://127.0.0.1:7890
export all_proxy=socks5://127.0.0.1:7890
```

Windows PowerShell：

```powershell
$env:HTTPS_PROXY = "http://127.0.0.1:7890"
$env:HTTP_PROXY = "http://127.0.0.1:7890"
```

验证：

```bash
curl -I https://github.com
```

返回`HTTP/2 200`之类，说明通了。取消设置：

```bash
unset https_proxy http_proxy all_proxy
```

Windows里关掉PowerShell窗口即可。

另一条路是代理软件里的「TUN模式」或「增强模式」，开启后它接管整台电脑的流量，终端不用再设变量。具体怎么开以你软件的说明为准。

## 下次怎么避免

想长期生效，让AI帮你写进~/.zshrc，同时一并写一个关闭代理的快捷命令，避免代理软件关了以后全局失灵。

## 还不行？

- 报`127.0.0.1 port 7890`连不上：代理软件没开，或端口号写错了。
- 只有git不通：看[GitHub连接超时](/fix/github-connect-timeout)。
- 只有npm不通：看[npm网络超时](/fix/npm-network-timeout)。
- 报证书错误：看[证书错误](/fix/ssl-certificate-error)。
