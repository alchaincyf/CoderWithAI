---
title: "Failed to connect to github.com port 443怎么办（克隆、推送超时）"
slug: github-connect-timeout
summary: 电脑连不上GitHub。配置终端代理，或让SSH走443端口。
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

[git](/words/git)通过网络去[GitHub](/words/github)取或送代码，这条路在你的网络环境里不通或不稳，超时了。你的代码没丢，仓库也没坏。

国内访问GitHub时不稳定很常见，有两个正规的办法：让git走你已有的代理，或者改用SSH并走443端口。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code / Codex
我在国内用git clone / push时报Failed to connect to github.com port 443。
请先解释原因，再诊断：运行git config --global --get http.proxy，curl -I https://github.com，并问我代理软件的本地端口。
如果我有代理，就用git config --global http.proxy设置；设置前告诉我怎么撤销。
没有代理的话，帮我把~/.ssh/config配成通过ssh.github.com的443端口连接。
不要关闭SSL校验，不要改系统文件。最后用git ls-remote对目标仓库验证能连通。
```

## 想自己动手的话

方法一，已有代理软件，让git使用它（端口以你的软件显示为准，这里7890仅是示例）：

```bash
git config --global http.proxy http://127.0.0.1:7890
git config --global https.proxy http://127.0.0.1:7890
```

不再需要时撤销，否则代理软件关掉后git会全部失败：

```bash
git config --global --unset http.proxy
git config --global --unset https.proxy
```

方法二，改用SSH走443端口。GitHub官方文档提供了这种做法，适合22端口被封的网络。在~/.ssh/config里加入：

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

看到`Hi 用户名! You've successfully authenticated`就通了。之后仓库地址要用SSH形式（git@github.com:用户名/仓库.git）。

## 下次怎么避免

用SSH地址克隆仓库，并提前配好SSH key。不要为了「连得上」去关SSL校验。

## 还不行？

- 浏览器能开GitHub、终端连不上：看[终端不走代理](/fix/terminal-proxy)。
- 推送被拒绝：看[push被拒](/fix/git-push-rejected)。
- 提示不支持密码验证：看[GitHub密码验证失败](/fix/github-password-auth)。
