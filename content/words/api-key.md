---
title: API Key（密钥）
slug: api-key
aka: ["API Key", "密钥", "Secret Key", "Token", "access key", "sk-"]
summary: 调用某个服务的专属密码，也是它向你收费的凭证。
category: 连接
level: 5
related: [env-var, api, auth, github, sdk]
updated: 2026-11-18
---

## 一句话
API Key是一长串字符，服务商用它认出「是你在调用」，并把费用记在你头上。谁拿到了你的Key，谁就能花你的钱、用你的额度。

## 打个比方
像一张充了值的储值卡，卡号印在卡上。没有人会把卡号贴在公告栏上，可是新手常常把Key直接贴进代码、上传到公开的GitHub。

不贴切的地方：储值卡可能有密码保护，Key本身就是全部凭证，看见即可用。

## 你会在哪儿碰到它
- 接AI模型（Claude、OpenAI、DeepSeek、Gemini等）、地图、短信、支付、[Supabase](/words/supabase)时，都要先到对应后台生成一个Key。
- 样子通常是 `sk-` 开头的一串字符，或名为 `API_KEY`、`SECRET`、`TOKEN` 的配置项。
- AI会说「请把你的Key填进.env文件」。

## AI提到它时，你该怎么接
```prompt 复制给 Claude Code
我刚拿到一个API Key，要接到项目里。
请帮我：1. 创建.env文件，把Key放进去，变量名你起；2. 确认.env已在.gitignore里；3. 代码里用环境变量读取，不要出现Key原文；4. 检查整个项目和git历史，确认没有任何地方已经写进了Key；5. 告诉我在Vercel上线时该怎么配置这个变量。
```

## 容易踩的坑
- 上传到GitHub：公开仓库里的Key会被机器人扫描，通常很快被盗刷。一旦泄露，先去服务商后台删除并重新生成，再处理代码。
- 发给AI聊天、截图发群：同样等于泄露。
- 前端代码里的变量，所有访问者都看得见。只有放在服务端的才算安全。
- 没设置用量上限：在服务商后台开启预算提醒和上限，是最便宜的保险。
- 订阅（如Claude Pro）和API Key是两套计费，API按用量单独付费。
