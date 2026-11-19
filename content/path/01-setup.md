---
title: 第1关 · 装好工具
slug: setup
order: 1
level: 1
summary: 装好你那条线的AI工具、Node.js和Git，并让AI回你一句话、新建一个文件。
time: 60到90分钟
cost: 国内线：工具免费，DeepSeek API按量付费（先小额充值）；国际线约20美元/月起
goal: 在终端或浏览器里让AI回一句话，并让它在文件夹里新建一个文件
words: [terminal, command, path, cli, nodejs, npm, git, agent, editor, api-key, env-var, api-relay]
fixes: [command-not-found, windows-not-recognized, powershell-execution-policy, node-version, npm-network-timeout, xcode-clt-missing, claude-code-region, cc-switch-not-working, api-401]
updated: 2026-11-18
---

## 这一关你要做成什么

做到这三件事就算过：

1. 电脑上装好了你选的AI工具，并且能用。
2. 在一个空文件夹里打开它，它能回你一句话。
3. 你让它新建一个文件，文件夹里真的出现了这个文件。

装的东西一共四样：Node.js、Git、AI工具，以及一把「钥匙」。国内线的钥匙是DeepSeek API Key，国际线是有订阅的账号。下面的命令在2026年11月对照官方文档核实过。安装命令会变，如果和你屏幕上看到的不一致，以官方页面为准。

## 先懂一件事：终端是什么

终端是一个只能打字的窗口。你敲一行字，电脑执行，把结果打印回来。AI命令行工具就住在里面。

不熟的话先扫一眼[终端](/words/terminal)和[命令](/words/command)。后面所有「在终端里运行」，意思都是：把我给的那一行原样粘贴进去，按回车。行首如果有`$`符号，不要复制，它只是提示符。

打开终端：

- Mac：按Command+空格，输入「终端」，回车。
- Windows：点开始菜单，搜「PowerShell」，打开。你会看到蓝色或黑色窗口，提示符类似`PS C:\Users\你的名字>`。

先把自己的线选好（没选，回[第0关](/start/what-is-ai-coding)）。下面先讲国内线的路线A和路线B，再讲国际线，最后是两条线都要装的Git。

## 动手

### 第1步：装Node.js（两条线都要装）

国内线的路线A和路线B要先装它，国际线后面的项目也会用到。Node.js是很多AI工具的运行基础，见[Node.js](/words/nodejs)。

打开nodejs.org，下载标有LTS（长期支持）的版本，按你的系统安装，一路按提示走。不要选带「Current」字样的。

装完关掉终端重开，运行：

```bash
node -v
```

你会看到：一个以v开头的版本号。走路线A的话，DeepSeek Harness这个包写明需要v24.9或更高（npm包元数据），版本更低请重新下载LTS安装。

如果看到别的：提示找不到命令，见[command not found](/fix/command-not-found)；提示版本太旧，见[Node版本太旧](/fix/node-version)。nodejs.org下载慢或打不开，换个时间重试，或用手机热点试一下。

### 国内线第2步：申请DeepSeek API Key

API Key是你调用DeepSeek模型的「钥匙」，也是它向你收费的凭证，见[API Key](/words/api-key)。

1. 浏览器打开DeepSeek开放平台：platform.deepseek.com，注册并登录。
2. 打开API keys页面：platform.deepseek.com/api_keys，创建一个Key。DeepSeek官方文档只给了这个入口，没有写更细的步骤，页面文字以实际为准。
3. Key创建后立刻复制，存到密码管理器或备忘录里。很多平台只在创建时显示一次完整Key。
4. 调用API是按量付费的，需要账户里有余额。充值方式和起充金额在平台里看，以平台页面为准。建议先小额充值。
5. 价格以官方价格页为准：api-docs.deepseek.com/zh-cn/quick_start/pricing。

> [!warn]这把Key等于你的钱包。不要发给任何人，不要贴到聊天群，不要写进网页的前端代码，不要上传到GitHub。后面第5关会专门讲怎么把Key放在服务端。

### 路线A：DeepSeek Harness

DeepSeek官方的编码Agent，国内最省事的一条。官方标注它是开发者预览，更新可能不兼容；也尚未接受安全审计。官方要求只授予最小权限、优先在专用环境里运行、备份它能访问的文件，详见[官方安全说明](https://github.com/deepseek-ai/deepseek-harness/blob/master/SAFETY.zh.md)。所以只在下面这个专门的`ai-test`空文件夹里用，里面不要放重要文件。

#### 第1步：启动

在终端里运行：

```bash
npx @deepseek-ai/dsh web
```

注意要带`web`，不是单独的`dsh`。官方说明它启动后在本地浏览器打开，地址是`http://127.0.0.1:3080`。`dsh`会把你运行命令时所在的文件夹当成默认的工作位置，所以先建好文件夹再启动：

Mac：

```bash
mkdir -p ~/Desktop/ai-test
cd ~/Desktop/ai-test
npx @deepseek-ai/dsh web
```

Windows PowerShell：

```powershell
mkdir $HOME\Desktop\ai-test
cd $HOME\Desktop\ai-test
npx @deepseek-ai/dsh web
```

你会看到：终端里出现服务启动的信息，里面有访问地址。第一次运行npx要下载包，要等一会儿。如果终端问你是否继续（比如`Ok to proceed? (y)`），输入y回车。

如果看到别的：卡住或报`ETIMEDOUT`，是国内连npm官方源慢，见[npm安装卡住](/fix/npm-network-timeout)。Windows报「禁止运行脚本」，见[PowerShell执行策略](/fix/powershell-execution-policy)。关闭这个终端窗口，服务也就停了，用的时候保持它开着。

#### 第2步：填Key

浏览器打开`http://127.0.0.1:3080`。按官方文档，进入设置，找到模型，在DeepSeek卡片上的「API 密钥」输入框里粘贴你的Key，点保存。官方说明保存后立即生效，不用重启。

#### 第3步：选工作文件夹，跟AI说话

官方文档说，全新的界面里还没有选工作区，会话输入框是灰的，要先添加工作区：选你启动`dsh`时所在的那个文件夹（`ai-test`）。

然后开始一个会话，粘贴：

```prompt 复制给 DeepSeek Harness
你好。请你做两件事：
1. 用一句中文告诉我你现在是什么模型、当前工作区的文件夹叫什么。
2. 在当前工作区里新建一个叫hello.txt的文件，内容是「今天我装好了AI编程工具」。
做完后告诉我你创建了哪个文件。
```

你会看到：AI回复一句话，然后`ai-test`文件夹里出现`hello.txt`。它要改文件或跑命令时，如果当前权限策略需要批准，界面会先问你，同意之前先看清它要运行什么命令、改哪个文件。

如果看到别的：官方文档提到，出现`MISSING_CREDENTIAL`说明还没保存Key，回设置里的模型页保存。其他界面细节以官方文档为准：deepseek-harness.github.io/deepseek-harness。

### 路线B：Claude Code或Codex，加CC Switch接DeepSeek

这条路多两步，换来的是本站后面所有教程都按它写的、生态最全的工具。核心思路：Claude Code这个程序照常装，让CC Switch把它背后的模型指向DeepSeek的官方API，调用走DeepSeek，不走Anthropic。

先如实说一件事：Anthropic官方把Claude Code列为仅在支持地区提供的应用，中国大陆不在名单内。用CC Switch把它接到国内模型API这种用法，Anthropic没有说明是否允许；DeepSeek等国内厂商官方提供了接入文档。本站不替你判断是否合规，风险请自行评估。Codex命令行是开源软件（Apache-2.0），换模型使用的限制少一些，在意这一点的话，可以优先用Codex加CC Switch，或直接用路线A。

再说清红线：只接DeepSeek（或Kimi、GLM、Qwen）自己的官方API。CC Switch的GitHub页面挂着一些第三方中转站的赞助广告，不要点，不要买，原因见[中转站](/words/api-relay)。

#### 第1步：装Claude Code

按Claude Code官方文档（code.claude.com/docs），Mac和Linux运行：

```bash
curl -fsSL https://claude.ai/install.sh | bash
```

Windows，PowerShell（提示符里有`PS`）：

```powershell
irm https://claude.ai/install.ps1 | iex
```

Windows如果用的是CMD（提示符没有`PS`），官方给的是另一条命令：

```batch
curl -fsSL https://claude.ai/install.cmd -o install.cmd && install.cmd && del install.cmd
```

官方文档特别说明，安装过程不显示进度。敲完回车后很久没动静是正常的，等提示符重新出现。

诚实说一句：这几条命令要从claude.ai下载文件，国内网络能不能顺利下载，我们没有实测，官方文档也没有保证。如果失败，官方还提供npm安装的方式（需要Node.js 22或更新版本）：

```bash
npm install -g @anthropic-ai/claude-code@latest
```

不要加`sudo`。国内连npm官方源慢或超时，见[npm安装卡住](/fix/npm-network-timeout)，换成国内镜像后再试。npm方式同样没有在国内网络下由我们实测。

装完关掉终端，重新打开一个，验证：

```bash
claude --version
```

你会看到：一行版本号，后面带`(Claude Code)`。注意现在不要运行`claude`去登录，等配好CC Switch再用。

如果看到别的：`command not found`或「不是内部或外部命令」，见[command not found](/fix/command-not-found)（Mac）或[Windows不是内部或外部命令](/fix/windows-not-recognized)。

#### 第2步：装CC Switch

CC Switch是免费开源的桌面软件（MIT协议），官方仓库：github.com/farion1231/cc-switch。README写明唯一官方网站是ccswitch.io，下载请认这两个入口，搜索结果里的仿站不要点。

- Mac：用Homebrew运行`brew install --cask cc-switch`，或到仓库的Releases页面下载`.dmg`。
- Windows：到Releases页面下载`.msi`安装包（也有便携版`.zip`）。

国内访问GitHub下载可能慢，见[GitHub连不上](/fix/github-connect-timeout)。

首次启动时，它会把你电脑上已有的Claude Code、Codex等配置导入成一个名为`default`的供应商，不用担心丢东西。官方还说明，它第一次写入配置前会备份原始配置。

#### 第3步：添加DeepSeek并启用

1. 打开CC Switch，在侧边栏选Claude Code。
2. 点右上角的「+」，在左侧分类里选「模型厂商」，找到DeepSeek预设，粘贴你的Key。预设会自动填好请求地址，只需要填Key。不要选「第三方平台」分类里的条目，那里面有转售模型的中转站。
3. 保存后，在应用页点这个供应商卡片上的「启用」（已有其他供应商时按钮叫「切换」），状态变成「使用中」。
4. 打开CC Switch的「设置→应用配置」，找到Claude Code一块，开启「跳过 Claude Code 初次安装确认」开关。不开的话，Claude Code首次启动可能提示你登录。这个开关会在`~/.claude.json`里写入`hasCompletedOnboarding: true`。

官方说明，Claude Code切换供应商后不用重启。

#### 第4步：让Claude Code回你一句话

进入`ai-test`文件夹，运行`claude`。Mac：

```bash
mkdir -p ~/Desktop/ai-test
cd ~/Desktop/ai-test
claude
```

Windows PowerShell：

```powershell
mkdir $HOME\Desktop\ai-test
cd $HOME\Desktop\ai-test
claude
```

过程中它可能问你是否信任这个文件夹，确认是你自己的`ai-test`就选信任。然后粘贴：

```prompt 复制给 Claude Code
你好。请你做两件事：
1. 用一句中文告诉我你现在是什么模型、当前所在的文件夹叫什么。
2. 在当前文件夹里新建一个叫hello.txt的文件，内容是「今天我装好了AI编程工具」。
做完后告诉我你创建了哪个文件。
```

你会看到：它先回答，然后要新建文件时停下来问你，选Yes批准。随后文件夹里出现`hello.txt`。退出：输入`/exit`。

如果看到别的：要求你登录，先确认上一步开了「跳过 Claude Code 初次安装确认」，再重启Claude Code。报错或没生效，见[CC Switch切换后还是报错](/fix/cc-switch-not-working)。Key错误见[API 401](/fix/api-401)；提示余额不足（DeepSeek错误码402），去platform.deepseek.com充值，见[DeepSeek错误码](https://api-docs.deepseek.com/zh-cn/quick_start/error_codes)。

#### 不想装CC Switch：DeepSeek官方的环境变量写法

DeepSeek官方文档也给了不用CC Switch的做法。注意不要和CC Switch混用：CC Switch会检测以`ANTHROPIC`开头的系统环境变量，冲突时顶部显示黄色警告横幅，可以点进去查看并删除。这种写法的缺点是密钥会留在终端历史里，更稳妥的是走CC Switch。想用的话，在启动`claude`之前，在同一个终端窗口里设置环境变量（[环境变量](/words/env-var)只对当前窗口有效，关掉就没了）。Mac和Linux：

```bash
export ANTHROPIC_BASE_URL=https://api.deepseek.com/anthropic
export ANTHROPIC_AUTH_TOKEN=<你的 DeepSeek API Key>
export ANTHROPIC_MODEL=deepseek-flash[1m]
export ANTHROPIC_DEFAULT_OPUS_MODEL=deepseek-flash[1m]
export ANTHROPIC_DEFAULT_SONNET_MODEL=deepseek-flash[1m]
export ANTHROPIC_DEFAULT_HAIKU_MODEL=deepseek-flash
export CLAUDE_CODE_SUBAGENT_MODEL=deepseek-flash
export CLAUDE_CODE_EFFORT_LEVEL=max
export CLAUDE_CODE_AUTO_COMPACT_WINDOW=786432
```

把`<你的 DeepSeek API Key>`连同尖括号一起换成你的Key（保留尖括号会报错），再运行`claude`。Windows PowerShell的写法（`$env:变量名="值"`）见官方页面：api-docs.deepseek.com/zh-cn/quick_start/agent_integrations/claude_code。这些变量名和值来自官方文档，之后可能变化，以官方页面为准。

#### Codex怎么接

DeepSeek官方有专门的「接入Codex」说明（api-docs.deepseek.com/zh-cn/quick_start/agent_integrations/codex），要改Codex的`~/.codex`配置，官方还提供了一键脚本。CC Switch也支持Codex，切换后需要重启终端。因为改配置文件的步骤较多，且和Claude Code的做法不同，这一关只带你跑通Claude Code；想用Codex，请先打开官方页面照着做，或让AI按官方页面帮你配。

#### 进阶：Magpie

Magpie（github.com/yetone/magpie）是在你电脑上运行的模型网关，可以给每个工具单独选模型，比如Claude Code用Kimi、Codex用DeepSeek。功能比CC Switch多，也更复杂，版本还很新，新手先用CC Switch。

它有一个「读取本机订阅账号跨工具使用」的功能，会读取你本机Claude Code、Codex的登录状态并跨工具使用，这可能违反对应厂商的服务条款，请在设置里关掉，只用官方API Key。CC Switch自己的「授权中心」（Beta）也有用订阅账号登录的功能，官方README同样提醒可能违反服务条款，不要用。它的GitHub页面同样有中转站的赞助广告，同样不要点。

### 其他国内选择：TRAE国内版

想要图形界面、不想碰终端，可以用字节的TRAE国内版，它是AI编辑器，不用先学终端。

打开浏览器，访问trae.cn，点类似「下载桌面端」的按钮，按系统下载安装。第一次打开按界面提示登录。截至2026年11月，官网定价页写着：免费版每月500积分，所有功能可用。登录后新建空文件夹`ai-test`，在TRAE里「打开文件夹」选它，保持IDE模式（看得见文件，更安心），在对话区粘贴同样的「你好……」提示词即可。不同系统的安装步骤和界面文字以官网为准。

### 其他国内选择：Kimi Code（本站不作首选）

想在终端里用国产模型，可以装Kimi Code。以下命令来自其官方资料整理，请以kimi.com/code页面为准。Mac：

```bash
curl -fsSL https://code.kimi.com/kimi-code/install.sh | bash
```

Windows PowerShell：

```powershell
irm https://code.kimi.com/kimi-code/install.ps1 | iex
```

装完运行`kimi`，再输入`/login`登录。使用需要Kimi会员，价格以官网为准。

### 国际线：Claude Code

前提：你人在Anthropic官方支持的地区，并且有Pro、Max等订阅（免费版不含）。大陆读者请回第0关改走国内线。

#### 第1步：安装

Mac，打开终端，粘贴：

```bash
curl -fsSL https://claude.ai/install.sh | bash
```

Windows，打开PowerShell（提示符里有`PS`），粘贴：

```powershell
irm https://claude.ai/install.ps1 | iex
```

Windows如果用的是CMD（提示符没有`PS`），官方给的是另一条命令：

```batch
curl -fsSL https://claude.ai/install.cmd -o install.cmd && install.cmd && del install.cmd
```

你会看到：官方文档特别说明，安装过程不显示进度。所以敲完回车后很久没动静是正常的，等提示符重新出现。

如果看到别的：

- Windows上看到`The token '&&' is not a valid statement separator`，说明你在PowerShell里用了CMD那条命令；看到`'irm' is not recognized`，说明你在CMD里用了PowerShell那条。换对窗口。
- 看到`403`或别的失败，先看[command not found](/fix/command-not-found)，并以官方排错页为准（code.claude.com/docs/en/troubleshoot-install）。

#### 第2步：验证装好

关掉终端，重新打开一个，运行：

```bash
claude --version
```

你会看到：一行版本号，后面带`(Claude Code)`。

如果看到别的：`command not found`或「不是内部或外部命令」，是路径没登记，见[command not found](/fix/command-not-found)（Mac）或[Windows不是内部或外部命令](/fix/windows-not-recognized)。

#### 第3步：登录并对话

先在桌面建一个空文件夹`ai-test`，然后在终端进入它。Mac：

```bash
cd ~/Desktop/ai-test
claude
```

Windows PowerShell：

```powershell
cd $HOME\Desktop\ai-test
claude
```

如果你的桌面被OneDrive接管，路径可能不同，用`cd`到你实际建文件夹的地方即可。

你会看到：第一次运行，它会提示你登录，按官方说法，订阅账号或Console账号在浏览器里完成授权。授权完成回到终端，会出现Claude Code的输入框，上方显示版本、当前模型和工作目录。过程中它还可能问你是否信任这个文件夹，确认是你自己的`ai-test`就选信任。

如果看到别的：浏览器提示「App unavailable in region」之类，是地区问题，见[登录失败、地区不支持](/fix/claude-code-region)。

然后粘贴：

```prompt 复制给 Claude Code
你好。请你做两件事：
1. 用一句中文告诉我你现在是什么模型、当前所在的文件夹叫什么。
2. 在当前文件夹里新建一个叫hello.txt的文件，内容是「今天我装好了AI编程工具」。
做完后告诉我你创建了哪个文件。
```

你会看到：它先回答，然后要新建文件时停下来问你，类似选项里有「Yes」。选Yes批准。随后文件夹里出现`hello.txt`。

退出：输入`/exit`，或按两次Ctrl+D。

### 国际线：Codex

前提同上，用ChatGPT账号登录。OpenAI官方的[受支持国家和地区名单](https://developers.openai.com/api/docs/supported-countries)不包含中国大陆，名单之外使用可能被封号或停用，和Claude Code一样不适合大陆直接使用，本站不教绕过的办法。

安装命令（来自OpenAI官方仓库的说明）：

Mac，终端：

```bash
curl -fsSL https://chatgpt.com/codex/install.sh | sh
```

Windows，PowerShell：

```powershell
powershell -ExecutionPolicy ByPass -c "irm https://chatgpt.com/codex/install.ps1 | iex"
```

也可以在装好Node.js后用npm：`npm install -g @openai/codex`，或在Mac用Homebrew：`brew install --cask codex`。

进入`ai-test`文件夹后运行：

```bash
codex
```

你会看到：首次运行让你选登录方式，选「Sign in with ChatGPT」，在浏览器里授权。成功后终端里出现一个横幅，显示模型、当前目录和几个斜杠命令（如`/init`、`/status`），然后是输入提示。粘贴上面那段「你好……」的提示词，它会在建文件前征求你的许可。

如果看到别的：PowerShell报「禁止运行脚本」，见[PowerShell执行策略](/fix/powershell-execution-policy)。

### 两条线都要装：Git

Git用来存档你的每一次修改，第2关会用到。

Mac：在终端运行：

```bash
git --version
```

如果没装过，系统通常会弹出一个窗口，提示安装命令行开发者工具，点安装、等它装完。完成后再运行一次，应显示版本号。没弹出或报错，见[Xcode命令行工具缺失](/fix/xcode-clt-missing)。

Windows：（国际线的Claude Code官方文档建议装Git for Windows，这样它能用Bash工具，不装也能用。）访问git-scm.com，进入Windows的下载页，下载安装程序运行。安装过程会连续问你很多选项，不确定就保持默认，一路下一步。装完重新打开PowerShell，运行`git --version`。

你会看到：一行类似`git version ...`的输出。

## 验收：怎么知道真的成了

- 在`ai-test`文件夹里能找到AI创建的`hello.txt`，内容是我指定的那句话。
- 终端运行`node -v`和`git --version`都有版本号输出。
- 国内线路线A：DeepSeek Harness的浏览器界面里完成了同样的对话。
- 国内线路线B：`claude --version`有输出，CC Switch里DeepSeek已启用，`claude`完成了同样的对话。
- 国际线：`claude --version`或`codex`能正常启动。
- 我的DeepSeek API Key没有发给别人，也没有写进任何网页代码。

## 最常卡在这几个地方

1. 装完提示找不到命令：多数是没新开终端，或路径没登记。看[command not found](/fix/command-not-found)。
2. Windows报「不是内部或外部命令」：见[Windows not recognized](/fix/windows-not-recognized)。
3. PowerShell禁止运行脚本：见[执行策略](/fix/powershell-execution-policy)。
4. Node版本太旧：见[Node版本](/fix/node-version)。
5. 路线B里Claude Code还是要登录：先开CC Switch的「跳过 Claude Code 初次安装确认」，仍不行或报错见[CC Switch切换后还是报错](/fix/cc-switch-not-working)。
6. npm下载超时：见[npm安装卡住](/fix/npm-network-timeout)。
7. 国际线登录失败：先看是不是地区问题，见[claude-code-region](/fix/claude-code-region)。

## 这一关的提示词合集

```prompt
你好。请你做两件事：
1. 用一句中文告诉我你现在是什么模型、当前所在的文件夹叫什么。
2. 在当前文件夹里新建一个叫hello.txt的文件，内容是「今天我装好了AI编程工具」。
做完后告诉我你创建了哪个文件。
```
