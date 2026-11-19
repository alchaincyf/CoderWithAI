# 闯关路线写作说明（八关）

站点定位见 _GUIDE.md。闯关路线是全站的主干：一个完全不会写代码的人，从第0关读到第7关，能做出一个有人用、能收钱的东西。

## 八关（slug｜标题｜这一关只解决的那件事｜过关标准）
0 what-is-ai-coding｜先搞懂AI编程是怎么回事｜建立正确心智：你是产品经理+验收人，AI是工程师；能做到什么、做不到什么；三类工具（网页一句话生成、AI编辑器、命令行Agent）；要花多少钱；国内外两条线怎么选｜读完能说清自己该走哪条线、准备多少预算
1 setup｜装好工具｜按两条线分别装好：国际线（Claude Code或Codex）、国内线（TRAE国内版；想用命令行的可选Kimi Code等）；以及Node.js、Git；第一次对话测试｜在终端或编辑器里让AI回一句话，并让它新建一个文件
2 first-page｜做出第一个网页｜新建项目文件夹→一句需求→AI生成→浏览器打开→提修改意见→迭代3轮；怎么提需求、怎么验收、怎么让AI解释它做了什么；第一次用Git存档｜自己电脑上能打开一个按你意思改过三轮的网页，并且存过档
3 go-live｜上线，让别人打开｜GitHub账号与仓库、推送、Vercel导入部署、拿到网址、改了再发；国内访问的现实（vercel.app访问不稳、自定义域名、国内平台与备案的取舍）｜手机4G打开你的网址能看到网页
4 save-data｜让它记住数据｜什么时候需要数据库；localStorage够用的情况；Supabase建表、登录、RLS；环境变量和密钥安全｜一个能注册登录、数据存云端、换设备还在的小应用
5 add-ai｜接上AI能力｜大模型API是什么、怎么拿API Key（国内选DeepSeek/Kimi/通义等，国外OpenAI/Anthropic）、为什么Key不能放前端、做一个「帮我总结」功能、费用怎么估、怎么防被刷｜作品里有一个调大模型的功能，Key安全地放在服务端
6 app-or-miniprogram｜做成App或小程序｜三条路的取舍：PWA（最省事）、微信小程序（要注册主体、审核、合法域名）、iOS App（Xcode、开发者账号$99/年、TestFlight、审核）；各自的成本与门槛｜作品以其中一种形态装进手机
7 get-paid｜让人付费、长期运营｜收钱的现实路径（国内：微信/支付宝需要营业执照或个体户，面包多/爱发电/小报童之类的平台；海外：Stripe、Lemon Squeezy、Creem等MoR）；让人找到你（SEO、内容分发、在社交平台Build in Public）；数据埋点；持续迭代与维护；什么时候该停｜有一个公开的收款入口和一个能看到访问数据的后台

## 每一关的 frontmatter
---
title: 第N关 · 标题
slug: （见上）
order: N
level: N
summary: 一句话（≤50字），说清这一关做完你会有什么
time: 预计用时（如「40分钟」「一个周末」）
cost: 这一关要花的钱（如「0元」「可能要20美元/月」）
goal: 过关标准（一句话，可验证）
words: [这一关会遇到的词典slug，5–10个]
fixes: [这一关最可能遇到的报错slug，3–6个]
updated: 2026-11-18
---

## 正文结构
## 这一关你要做成什么   （过关标准讲具体，告诉读者「做到这个就算过」）
## 先懂一件事   （这一关唯一必须懂的概念，用比方讲透；链到词典）
## 动手   （分步骤 ### 第1步……，每步：做什么→复制什么提示词或命令→你会看到什么→看到别的怎么办；国内线/国际线有差异就分开写）
## 验收：怎么知道真的成了   （可操作的检查清单）
## 最常卡在这几个地方   （3–5条，链到 /fix/<slug>）
## 这一关的提示词合集   （把本关所有提示词再按顺序列一遍，方便一次性复制）

篇幅：每关2500–5000字，第0关可以短一些。宁可具体不要长：每个步骤都要有读者真的会在屏幕上看到的东西。

## 事实来源
工具、价格、可用性只能用 /Users/alchain/Documents/_开发项目/coderwithai-2026/research/tools-facts.json 里 confidence 为 high/medium 的条目，或你自己查到的官方页面。所有价格后面写「（2026年11月）」。安装命令、菜单名必须查官方文档（Claude Code: docs.claude.com / code.claude.com；Codex: developers.openai.com/codex；TRAE：trae.cn；Vercel、GitHub、Supabase官方文档）。查不到的不写。
重要：Anthropic官方受支持地区名单不含中国大陆——国际线要如实说明，不教绕过、不推荐具体机场或代购。

可链接的词典slug：terminal, command, path, editor, frontend, backend, web-basics, framework, react, nextjs, nodejs, npm, dependencies, node-modules, localhost, port, dev-server, build, log, devtools, json, markdown, deploy, server, vercel, domain, dns, https, icp-filing, cdn, static-site, serverless, python, script, cli, database, supabase, sql, crud, auth, localstorage, api, api-key, env-var, webhook, sdk, cors, git, repo, commit, branch, github, push, rollback, llm, token, context-window, prompt, agent, vibe-coding, mcp, skill, agents-md, plan-mode, subagent, hallucination, app-store, testflight, mini-program, xcode, pwa, browser-extension, payment
可链接的急救slug：command-not-found, windows-not-recognized, powershell-execution-policy, npm-eacces, node-version, xcode-clt-missing, npm-network-timeout, github-connect-timeout, terminal-proxy, ssl-certificate-error, claude-code-region, pip-externally-managed, npm-eresolve, module-not-found, port-in-use, missing-script, enoent-package-json, blank-page, hydration-mismatch, json-unexpected-token, heap-out-of-memory, localhost-refused, python-module-not-found, permission-denied-file, git-not-a-repo, git-push-rejected, github-password-auth, git-merge-conflict, secret-leaked, undo-ai-changes, vercel-build-failed, vercel-404, env-missing-in-production, domain-not-working, vercel-blocked-in-china, cors-blocked, supabase-rls, api-401, api-429, fetch-failed, claude-usage-limit, context-full, ai-stuck-loop, api-overloaded, xcode-signing, app-review-rejected, miniprogram-domain, miniprogram-appid
可链接的配方slug：personal-page, invitation, habit-tracker, mini-game, excel-merge, photo-rename, landing-waitlist, ledger-app, ai-chatbot, browser-extension-summary, data-dashboard, daily-digest
