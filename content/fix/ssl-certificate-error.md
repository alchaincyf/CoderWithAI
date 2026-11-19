---
title: "UNABLE_TO_GET_ISSUER_CERT_LOCALLY、证书错误怎么办"
slug: ssl-certificate-error
summary: 网络环境（公司代理、杀毒软件）换了证书，程序不认。别关校验，装好证书。
category: 网络
match: ["UNABLE_TO_GET_ISSUER_CERT_LOCALLY", "unable to get local issuer certificate", "SELF_SIGNED_CERT_IN_CHAIN", "self signed certificate in certificate chain", "CERT_HAS_EXPIRED", "unable to verify the first certificate"]
platform: [all]
updated: 2026-11-18
---

```error
npm error code UNABLE_TO_GET_ISSUER_CERT_LOCALLY
npm error errno UNABLE_TO_GET_ISSUER_CERT_LOCALLY
npm error request to https://registry.npmjs.org/xxx failed, reason: unable to get local issuer certificate
```

其他常见写法：`self signed certificate in certificate chain`、`unable to verify the first certificate`、`CERT_HAS_EXPIRED`。

## 人话：这是怎么回事

访问网站时，程序会检查对方的「身份证」（证书）是不是由可信机构签发的。这句报错的意思是：它收到的证书，它不认识签发者。

最常见的原因有三个：公司网络或防火墙在中间换了证书；杀毒软件在扫描 HTTPS 流量；电脑的日期时间不对。这是环境问题，不是项目问题。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code / Codex
我运行 npm install（或 git、claude）时报 UNABLE_TO_GET_ISSUER_CERT_LOCALLY / self signed certificate。
请先解释原因，再诊断：检查系统日期时间是否正确，我是否在公司网络或开着杀毒软件的 HTTPS 扫描。
如果是公司网络，告诉我向 IT 要根证书 PEM 文件，并用 NODE_EXTRA_CA_CERTS 指向它。
严禁建议我 npm config set strict-ssl false、NODE_TLS_REJECT_UNAUTHORIZED=0 或 git config http.sslVerify false。
修好后重新执行原命令验证。
```

## 想自己动手的话

第一步，先看系统日期和时间对不对，不对就改成自动设置。证书对时间很敏感，时间错了会报 `CERT_HAS_EXPIRED`。

第二步，临时关掉杀毒软件的「HTTPS 扫描」或「网络防护」功能再试，能通就是它的问题，去它的设置里把 Node、git 加入白名单。

第三步，如果是公司网络，向 IT 要一个公司根证书的 PEM 文件，然后让 Node 信任它（Claude Code 官方文档的做法）：

```bash
export NODE_EXTRA_CA_CERTS=/path/to/corporate-ca.pem
```

这条命令让 Node 在系统自带证书之外，再多信任这一个。路径换成你的文件位置。Windows PowerShell 写法：

```powershell
$env:NODE_EXTRA_CA_CERTS = "C:\path\to\corporate-ca.pem"
```

> [!warn] 网上流传的「万能解法」是 `npm config set strict-ssl false`、`NODE_TLS_REJECT_UNAUTHORIZED=0`、`git config http.sslVerify false`。它们都是把检查身份证这一步直接关掉，之后你下载的任何东西都可能被中间人替换，密码和密钥也可能泄露。看起来好了，实际是把门拆了。

## 下次怎么避免

用公司电脑装开发环境前，先问 IT 有没有统一的证书配置。

## 还不行？

- 国内网络超时而不是证书：看 [npm 网络超时](/fix/npm-network-timeout)。
- 浏览器能开、终端不行：看 [终端不走代理](/fix/terminal-proxy)。
- 在 Mac 上 curl 报 TLS 错误：升级 macOS 会更新根证书。
