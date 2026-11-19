---
title: 绑了域名打不开（DNS没生效、DNS_PROBE_FINISHED_NXDOMAIN）怎么办
slug: domain-not-working
summary: 多半是DNS记录没配对或还没传开。核对解析记录，等生效，再看证书状态。
category: 部署上线
match: ["DNS_PROBE_FINISHED_NXDOMAIN", "Invalid Configuration", "ERR_NAME_NOT_RESOLVED", "ERR_SSL_VERSION_OR_CIPHER_MISMATCH", "Your connection is not private", "DNS_PROBE_FINISHED_NO_INTERNET"]
platform: [all]
updated: 2026-11-18
---

```error
This site can't be reached
mysite.com's server IP address could not be found.
DNS_PROBE_FINISHED_NXDOMAIN
```

## 人话：这是怎么回事

[域名](/words/domain)要靠[DNS](/words/dns)把名字翻译成服务器地址。`NXDOMAIN`的意思是：全网查不到这个名字对应的记录。要么你还没配解析，要么配错了，要么刚配完还没传开。

访问网站要过三关：域名解析对不对、平台认不认这个域名、[HTTPS](/words/https)证书签发了没有。哪一关没过，都会打不开。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code
我把域名yourdomain.com绑到了Vercel项目，但打开提示DNS_PROBE_FINISHED_NXDOMAIN（或证书错误）。

请帮我诊断：
1. 运行nslookup yourdomain.com和nslookup www.yourdomain.com（也可以用dig），告诉我解析到了什么，是否为空。
2. 对照Vercel项目Settings > Domains里显示的记录要求，我会把页面上显示的记录类型和值粘贴给你：（粘贴在这里）
3. 判断是缺记录、记录类型错、还是存在冲突的旧记录，用大白话告诉我要去域名服务商后台改什么。
4. 你不能代我登录域名后台，请给出一步步的操作清单。
```

## 想自己动手的话

1. 在[Vercel](/words/vercel)项目Settings的Domains里，看你的域名状态。如果显示Invalid Configuration，说明解析还没对上，页面会列出需要的记录。
2. 按官方文档：根域名（如`example.com`）用**A记录**，子域名（如`www.example.com`）用**CNAME记录**。具体的值**以Vercel页面显示的为准**，每个项目可能不同，别抄别人的。
3. 到你买域名的地方（阿里云、腾讯云、Cloudflare、GoDaddy等）的DNS解析页面，添加或修改这条记录。检查有没有指向别处的旧A、AAAA、CNAME记录，冲突的要删掉。
4. 等待生效。DNS变更需要时间传播，通常几分钟到几小时，偶尔更久。

自己验证：

```bash
nslookup yourdomain.com
```

看返回的地址是否变成了你配置的值。把`yourdomain.com`换成你的域名。

证书：域名解析正确后，Vercel会自动签发HTTPS证书，需要一小会儿。这期间浏览器提示证书不安全是暂时的，等几分钟再刷新。

> [!warn]不要为了「立刻生效」反复改记录或频繁换服务商，每次改动都会重新计时。改一次，耐心等。

## 下次怎么避免

绑域名放在方便等待的时间段做，别在上线前一刻。同时给根域名和www都添加，并设置其中一个跳转到另一个。

## 还不行？

只有国内用户打不开，看[vercel-blocked-in-china](/fix/vercel-blocked-in-china)。证书一直不通过，看[ssl-certificate-error](/fix/ssl-certificate-error)。部署本身404，看[vercel-404](/fix/vercel-404)。
