---
title: "报错「TypeError: fetch failed」怎么办"
slug: fetch-failed
summary: Node发出的请求没连上目标。看下面cause那一行才是真原因：超时、域名解析失败，或需要走代理。
category: 网络
match: ["TypeError: fetch failed", "fetch failed", "ETIMEDOUT", "ECONNRESET", "ENOTFOUND", "ECONNREFUSED", "UND_ERR_CONNECT_TIMEOUT"]
platform: [all]
updated: 2026-11-18
---

```error
TypeError: fetch failed
    at node:internal/deps/undici/undici:13502:13
    at process.processTicksAndRejections (node:internal/process/task_queues:105:5) {
  [cause]: Error: connect ETIMEDOUT 142.250.0.0:443
```

具体行号和地址因版本而异，要看的是最后的 `[cause]` 一行。

## 人话：这是怎么回事

`fetch failed` 是个很笼统的外壳，意思是「Node程序（不是浏览器）想访问某个网址，没连上」。真正的原因藏在下面的 `[cause]` 里：

- `ETIMEDOUT`：连接超时，网络不通或被拦。
- `ENOTFOUND`：域名解析失败，网址拼错了或DNS有问题。
- `ECONNRESET`：连接被中途掐断。
- `ECONNREFUSED`：对方没在监听，比如本地服务没启动。

国内读者最常见的是第一种：程序要访问海外服务（OpenAI、Google、GitHub、npm上游等），而Node默认**不会**使用你电脑上开着的代理。你浏览器能上，不代表Node能上。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code
我的Node程序报错 TypeError: fetch failed，下面是完整输出，包含 [cause] 部分：

（粘贴完整报错）

请：
1. 根据 [cause] 判断是超时、域名解析、连接被重置还是对方没启动，用大白话解释。
2. 用 curl -v 请求同一个网址，对比终端能不能通。
3. 检查我的终端有没有设置 HTTP_PROXY / HTTPS_PROXY，以及 node -v 的版本。
4. 如果判断是需要走代理：告诉我怎么让Node使用我已有的代理（不要教我如何获取代理），修改后重跑验证。
5. 网址拼写和环境变量也一并检查。
```

## 想自己动手的话

先看版本和 `cause`：

```bash
node -v
```

查看Node版本。

用终端直接测一下目标网址是否可达（换成你的）：

```bash
curl -I https://api.example.com
```

`curl` 也连不上，说明是网络问题，不是代码问题。

如果你本机已有合法的代理工具在运行，让Node使用它。根据Node官方文档，从 v24.0.0 和 v22.21.0 起，可以通过环境变量开启对代理的读取（该功能标注为「Active Development」）：

```bash
NODE_USE_ENV_PROXY=1 HTTPS_PROXY=http://127.0.0.1:7890 node app.js
```

`NODE_USE_ENV_PROXY=1` 让Node启动时读取 `HTTP_PROXY`、`HTTPS_PROXY`、`NO_PROXY`。端口 `7890` 只是示例，要换成你代理软件里显示的端口。Node版本更低时需要升级，或让AI按你的版本给出替代方案，见 [node-version](/fix/node-version)。

终端层面的代理设置见 [terminal-proxy](/fix/terminal-proxy)。

> [!warn] 官方文档强调，代理支持是给「防火墙要求走授权代理」这类场景用的，请只使用你有权使用的代理。别让AI去"想办法绕过"网络限制。

## 下次怎么避免

遇到 `fetch failed` 先看 `cause`，别只看第一行。写调用外部服务的代码时，让AI加上超时和清晰的错误提示。

## 还不行？

`ENOTFOUND` 先检查网址拼写。`ECONNREFUSED` 到 localhost，看 [localhost-refused](/fix/localhost-refused)。访问GitHub不通看 [github-connect-timeout](/fix/github-connect-timeout)。证书相关报错看 [ssl-certificate-error](/fix/ssl-certificate-error)。
