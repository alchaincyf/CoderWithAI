---
title: "Failed to connect to github.com port 443 怎么办（克隆、推送超时）"
slug: github-connect-timeout
summary: 电脑连不上 GitHub。配置终端代理，或让 SSH 走 443 端口。
category: Git
match: ["Failed to connect to github.com port 443", "Couldn't connect to server", "Connection timed out", "unable to access 'https://github.com", "Could not resolve host: github.com", "Recv failure: Connection was reset"]
platform: [all]
updated: 2026-11-18
---

```error
fatal: unable to access 'https://github.com/xxx/yyy.git/': Failed to connect to github.com port 443 after 21090 ms: Couldn't connect to server
```

也可能是这几种写法：`Connection timed out`、`Could not resolve host: github.com`、`OpenSSL SSL_read: Connection was reset`。

## 人话：这是怎么回事

[git](/words/git) 通过网络去 [GitHub](/words/github) 取或送代码，这条路在你的网络环境里不通或不稳，超时了。你的代码没丢，仓库也没坏。

国内访问 GitHub 时不稳定很常见，有两个正规的办法：让 git 走你已有的代理，或者改用 SSH 并走 443 端口。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code / Codex
我在国内用 git clone / push 时报 Failed to connect to github.com port 443。
请先解释原因，再诊断：运行 git config --global --get http.proxy，curl -I https://github.com，并问我代理软件的本地端口。
如果我有代理，就用 git config --global http.proxy 设置；设置前告诉我怎么撤销。
没有代理的话，帮我把 ~/.ssh/config 配成通过 ssh.github.com 的 443 端口连接。
不要关闭 SSL 校验，不要改系统文件。最后用 git ls-remote 对目标仓库验证能连通。
```

## 想自己动手的话

方法一，已有代理软件，让 git 使用它（端口以你的软件显示为准，这里 7890 仅是示例）：

```bash
git config --global http.proxy http://127.0.0.1:7890
git config --global https.proxy http://127.0.0.1:7890
```

不再需要时撤销，否则代理软件关掉后 git 会全部失败：

```bash
git config --global --unset http.proxy
git config --global --unset https.proxy
```

方法二，改用 SSH 走 443 端口。GitHub 官方文档提供了这种做法，适合 22 端口被封的网络。在 ~/.ssh/config 里加入：

```text
Host github.com
  Hostname ssh.github.com
  Port 443
  User git
```

然后测试：

```bash
ssh -T -p 443 git@ssh.github.com
```

看到 `Hi 用户名! You've successfully authenticated` 就通了。之后仓库地址要用 SSH 形式（git@github.com:用户名/仓库.git）。

## 下次怎么避免

用 SSH 地址克隆仓库，并提前配好 SSH key。不要为了「连得上」去关 SSL 校验。

## 还不行？

- 浏览器能开 GitHub、终端连不上：看 [终端不走代理](/fix/terminal-proxy)。
- 推送被拒绝：看 [push 被拒](/fix/git-push-rejected)。
- 提示不支持密码验证：看 [GitHub 密码验证失败](/fix/github-password-auth)。
