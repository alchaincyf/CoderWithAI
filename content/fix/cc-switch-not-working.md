---
title: "CC Switch切换后Claude Code还是报错、没生效怎么办"
slug: cc-switch-not-working
summary: 在CC Switch里启用了供应商，Claude Code或Codex却没换模型、报错或还要登录。按官方README列的几个原因逐项查。
category: AI工具
match: ["CC Switch", "cc-switch", "Connectivity check", "PROXY_MANAGED", "Upstream Format", "404", "405"]
platform: [all]
updated: 2026-11-18
---

```error
404 / 405（OpenAI兼容接口），或切换后模型还是旧的、请求失败
```

CC Switch自己也可能显示「Connectivity check」通过，但真实对话仍然失败。

## 人话：这是怎么回事

CC Switch的工作方式，是改Claude Code、Codex等工具的配置文件，把请求地址、密钥、模型换成你选的供应商。所以出问题，一般落在这几处：没真正启用、该重启的没重启、密钥或模型名填错、接口格式选错。下面的内容来自CC Switch官方README（github.com/farion1231/cc-switch），没有写到的情况，这里不猜。

## 最快的解法：把这段发给AI

```prompt 复制给你正在用的AI
我用CC Switch把Claude Code接到DeepSeek官方API，但切换后没生效或报错。
请只用官方资料帮我排查：
1. CC Switch官方README：github.com/farion1231/cc-switch
2. DeepSeek官方文档：api-docs.deepseek.com/zh-cn/quick_start/agent_integrations/claude_code
让我依次检查：供应商是否已点「启用／切换」、「跳过 Claude Code 初次安装确认」是否已开、请求地址是否是官方文档给的地址、密钥和模型名是否正确、CC Switch的Usage里有没有请求日志、~/.cc-switch/logs/cc-switch.log里有没有报错。
先告诉我每一步看哪里，不要替我改配置文件，也不要让我把密钥发给你。
```

## 想自己动手的话

按顺序查：

1. 供应商有没有真正启用。在CC Switch应用页点供应商卡片上的「启用」或「切换」，状态变成「使用中」；也可以在系统托盘里，在对应工具的子菜单里点供应商名字切换。
2. 要不要重启。按README：Claude Code切换后不需要重启；Codex、Gemini CLI、Grok Build切换后需要重启终端或工具；Claude Desktop需要完全退出再打开。
3. 「Connectivity check」通过不代表能用。README写明，这个检查只测地址通不通，不发真实的模型请求，验证不了密钥和模型名。请重新检查密钥、模型名，并在「Usage」里看请求日志。
4. 密钥和请求地址。添加时请从「模型厂商」分类里选DeepSeek预设，它会自动填好地址；DeepSeek官方文档里，Claude Code用的地址是`https://api.deepseek.com/anthropic`，密钥来自platform.deepseek.com/api_keys。密钥错误见[API 401](/fix/api-401)；余额不足是DeepSeek错误码402，去platform.deepseek.com充值，见[DeepSeek错误码](https://api-docs.deepseek.com/zh-cn/quick_start/error_codes)。
5. 模型名。README写明，模型属于供应商字段：你在Claude Code里用`/model`临时选的模型，切换后会被目标供应商保存的模型替换。要长期用某个模型，请在CC Switch里改这个供应商。
6. 接口格式。如果报404或405，README说通常是「Upstream Format」选错，或没有启动路由。在「Advanced Options」里选与供应商接口匹配的格式。
7. 路由模式。README说，页面上「Direct」和「Routing」标签只是切换查看，不改配置，点「Start routing」才真正启用路由，默认监听`http://127.0.0.1:15721`，这时工具配置里写的是这个地址和占位密钥`PROXY_MANAGED`，真实密钥保存在CC Switch里，这是正常的，不是配置错了。
8. 看日志。README给出的日志位置是`~/.cc-switch/logs/cc-switch.log`（Windows是`C:\Users\用户名\.cc-switch`下的同名位置），反馈问题时附上它，但记得先检查里面有没有你的密钥再发给别人。
9. WSL用户：README说WSL2默认的NAT网络下，WSL里的`127.0.0.1`访问不到Windows上的本地路由，需要改用镜像网络模式（mirrored networking）。
10. 运行`claude`时提示要登录、或出现初始化引导：官方手册的做法是，打开CC Switch的「设置→应用配置」，在Claude Code一块开启「跳过 Claude Code 初次安装确认」，然后重新启动Claude Code。这个开关会在`~/.claude.json`里写入`hasCompletedOnboarding: true`，关闭时删除该字段。

想回到官方登录：切换到列表里内置的官方供应商（如「Claude Official」），重启工具后按它自己的流程登录。README说CC Switch第一次写入配置前会备份原始配置，位置在`~/.cc-switch/backups/live-first-write/`。

如果你之前在终端里手动`export`过`ANTHROPIC_`开头的环境变量：官方手册说CC Switch会检测以`ANTHROPIC`开头的系统环境变量，冲突时界面顶部显示黄色警告横幅，点「查看详情」能看到来源并删除（删除前自动备份）。看到横幅就处理掉，只保留一种方式，不要混用。

> [!warn]CC Switch的GitHub页面上挂着一些API中转站的赞助广告。不要因为排错不顺就去买中转服务。本站只推荐各模型厂商的官方API，原因见[中转站](/words/api-relay)。

## 下次怎么避免

- 每次切换后，先在终端里让AI回一句话，确认通了再开始干活。
- 密钥只放在CC Switch里，不要发给别人，也不要贴进聊天记录。

## 还不行？

- 密钥或余额问题：[API 401](/fix/api-401)。
- 地区提示：[Claude Code登录失败、地区不支持](/fix/claude-code-region)。
- npm或下载超时：[npm安装卡住](/fix/npm-network-timeout)。
- `claude`命令找不到：[command not found](/fix/command-not-found)。
