---
title: Skills专题：给AI的「技能包」是什么，怎么挑，怎么装
summary: 读懂Skill是什么、为什么值得用，学会在Claude Code、Codex、DeepSeek Harness里安装，再按场景挑出可信的现成技能包。
updated: 2026-11-18
---

## Skill是什么

Skill是一个文件夹，里面最重要的是一份叫`SKILL.md`的说明书，写着「遇到某一类任务，该怎么做」。你可以把它理解成给AI的工作手册，或者一个技能包。

比方说，你每周都要把一堆数据整理成周报。没有Skill的时候，你每次都要把要求重新讲一遍。有了Skill，你把「周报该包含什么、用什么格式、哪里容易出错」写进去一次，之后AI遇到类似的活，会自己翻开这份手册照着做。

一个Skill文件夹长这样：

```text
my-skill/
├── SKILL.md       必须有：名字、描述，加上具体步骤
├── scripts/       可选：AI可以直接运行的脚本
├── references/    可选：需要时才去翻的参考资料
└── assets/        可选：模板、素材
```

文件开头有一小段说明，至少写明`name`（名字）和`description`（它做什么、什么时候该用）。AI就是靠这段描述，判断眼前的任务该不该用这个Skill。这也是为什么描述写得好不好，决定了它灵不灵。

Skill最早由Anthropic提出，后来作为公开格式发布（[agentskills.io](https://agentskills.io)），被越来越多的AI工具采用。词典里有更短的版本：[Skill（技能包）](/words/skill)。

## 为什么值得用

三个原因，都来自官方文档的设计说明。

**不用每次重讲。** 提示词管的是这一次对话，Skill管的是以后所有对话。你教过一次的流程，AI下次就会了。

**少占AI的记忆。** AI一次能读的内容有上限（见[上下文窗口](/words/context-window)）。Skill采用「渐进式加载」：一开始AI只看到每个Skill的名字和描述，Claude平台文档给的量级是每个约100个token；只有当任务对上了，才会把正文读进来，正文建议控制在5k token以内（Claude Code文档则按行数建议，`SKILL.md`别超过500行，两份文档量法不同，不矛盾）；更深的参考文件和脚本，用到才打开。这比把所有内容一股脑塞进对话省得多，对比一下，写在[AGENTS.md](/words/agents-md)里的内容是整份都每次会话占位置的。

但省不等于零。Claude Code官方文档说得很直接：列表里的每个Skill，每一轮对话都会占一点上下文，不管这一轮用没用到它，每个Skill的描述最多保留1,536个字符。所以按需要装，别囤。在Claude Code里输入`/skill-doctor`，能看到每个Skill花了多少、用了几次，不常用的可以关掉。

**能带工具，不只是文字。** Skill里可以放脚本和模板，让AI直接动手：生成PPT、清洗表格、渲染视频，而不是只给你一段建议。

花叔写过一本免费的《Agent Skills》橙皮书，从概念讲到怎么自己做，适合想系统了解的人：

[https://www.huasheng.ai/orange-books/agent-skills/](https://www.huasheng.ai/orange-books/agent-skills/)

## 怎么装

先记住一个共同点：各家工具都是去几个固定的文件夹里找Skill。装一个Skill，本质上就是把它的文件夹放到对的位置。下面三家，位置以各自官方文档为准，截至2026年11月。

### Claude Code

官方文档给的位置：

```text
~/.claude/skills/<技能名>/SKILL.md      个人，所有项目都能用
.claude/skills/<技能名>/SKILL.md        只在当前项目里用，可随项目分享
<插件>/skills/<技能名>/SKILL.md          通过插件安装，名字带插件前缀
```

有三种装法：

1. 插件市场。在Claude Code里输入`/plugin marketplace add 仓库名`添加一个市场，再输入`/plugin install 插件名@市场名`安装。输入安装命令后，Claude Code会先打开这个插件的详情面板，列出它会装进来的内容，再让你选范围：只给你自己（所有项目）、给这个仓库的所有协作者，或者只在当前仓库给你自己。**插件不等于纯文字的Skill**：官方文档提醒，插件还可能带hooks（在特定时机自动运行的命令）和MCP服务器（让AI连接外部工具），所以装之前先读详情面板。比如superpowers就带了一个会话开始时自动运行的hook，装完以后每次开新会话，它都会在你的电脑上跑一段脚本。例如Anthropic官方仓库：

```text
/plugin marketplace add anthropics/skills
/plugin install document-skills@anthropic-agent-skills
```

2. 让AI替你装。对Claude Code说：「帮我安装这个skill：仓库地址。先把SKILL.md和scripts里的文件读一遍，用人话告诉我它会做什么，我同意了你再装。」它会自己下载并放到正确位置。别省掉中间那句，不然AI会直接装。
3. 手动放文件夹。只适用于仓库根目录本身就是一个Skill（根目录里有`SKILL.md`）的情况：用`git clone 仓库地址 ~/.claude/skills/技能名`。很多仓库把Skill放在子文件夹里（比如anthropics/skills的`skills/pdf/`），整个克隆下来会多出一层目录，Claude Code按上面的位置表只认`~/.claude/skills/<技能名>/SKILL.md`这一层，找不到。这种仓库请改用插件或`npx skills add`，或者只把里面那个带`SKILL.md`的文件夹复制过去。

装好后，在对话里输入`/`能看到技能列表，也可以输入`/技能名`直接调用。官方文档说，Claude Code会监视这些文件夹，新增或修改通常当场生效；如果刚新建了`skills`文件夹却没出现，输入`/reload-skills`或重开会话。

另外，很多开源仓库的说明里写着`npx skills add 作者/仓库`。这是一个第三方开源安装工具（[vercel-labs/skills](https://github.com/vercel-labs/skills)），需要先装好[Node.js](/words/nodejs)，它会问你装到哪个AI工具里。用不用都行，以对应仓库的README为准。两点要知道：它会收集匿名使用数据，想关掉，就把环境变量`DISABLE_TELEMETRY=1`或`DO_NOT_TRACK=1`设上（仓库README的说法）；它要从npm下载，国内网络下可能超时，见[npm下载超时怎么办](/fix/npm-network-timeout)。

### Codex

Codex官方文档说明，它读取这些位置：

```text
<项目>/.agents/skills/        当前项目（会一直往上找到仓库根目录）
~/.agents/skills/             个人，所有项目都能用
```

格式同样是一个带`SKILL.md`的文件夹，开头需要有`name`和`description`。安装有两种办法：在Codex里输入`$skill-installer 技能名`，它能装官方整理好的，也能从其他仓库获取；或者直接把文件夹放进上面的目录。调用时输入`$`加技能名，或者输入`/skills`查看列表；Codex也会根据描述自己选用。新装的Skill通常自动出现，不出现就重启Codex。

### DeepSeek Harness

DeepSeek Harness（命令是`dsh`）是DeepSeek官方开源的agent工具，目前处于开发者预览阶段，官方README提醒版本之间可能有不兼容的改动。它的官方文档里，skill同样是`<名字>/SKILL.md`的文件夹，会按顺序扫描这些位置：

```text
<项目>/.dsh/skills/       当前项目，优先级最高
<项目>/.agents/skills/    当前项目
~/.agents/skills/         个人，所有项目都能用（优先级较低）
```

文档里还列了`dsh`自己的用户目录和自定义目录，细节见官方说明。名字要用小写字母加短横线。因为也认`.agents/skills`这个位置，放在`~/.agents/skills/`里的Skill，Codex和DeepSeek Harness能共用一份。官方没有写专门的安装命令，做法就是把文件夹放进上面的位置。官方文档地址：

[https://github.com/deepseek-ai/deepseek-harness/blob/main/docs/subsystems/skills.md](https://github.com/deepseek-ai/deepseek-harness/blob/main/docs/subsystems/skills.md)

> [!tip] 同一个Skill能不能三家通用，主要看它有没有依赖某个工具独有的功能。仓库README里一般会写「支持哪些工具」。换工具前先看那一行。

### 国内网络装不上

上面几乎所有装法都要访问GitHub。如果报`Failed to connect to github.com port 443`，不是Skill的问题，是网络连不上。按这一页处理：[GitHub连接超时怎么办](/fix/github-connect-timeout)。用`npx skills add`的，npm也可能超时，见[npm下载超时怎么办](/fix/npm-network-timeout)。解决之后再回来装。

那一页的办法主要是用你已有的代理软件，或者改走SSH 443。如果这两样你都没有，上面的装法在你这边可能就是装不上，目前没有绕开GitHub的正规办法。别用来路不明的「加速」「镜像」去下载，Skill装好以后是在你电脑上运行的。

## 怎么挑

一句话：Skill不只是文字，它可以带脚本，能让AI去运行命令、读写你电脑上的文件、联网。Anthropic的文档对此说得很直接：只使用你自己写的，或来自Anthropic的Skill，来自其他来源的要像装软件一样对待。

> [!warn] 装别人的Skill之前，打开文件夹看一遍：`SKILL.md`在让AI做什么，`scripts/`里有没有访问网络、读你密码或密钥文件、删除文件的代码。如果装的是插件，还要看它带不带hooks（自动运行的命令）和MCP服务器（连接外部工具）。看不懂就把整个文件夹交给AI，让它用人话告诉你每个脚本会干什么。

几条够用的判断标准：

1. 来源可查。作者是谁、仓库有没有人维护、最近有没有更新、许可证是什么。官方、大公司、长期维护的个人作者，比来路不明的「一键神器」靠谱得多。
2. star多只能说明被很多人收藏，不能说明安全。大仓库也出过问题，所以star只用来筛选，不能代替检查。
3. 看它要什么权限。需要给系统权限（比如屏幕录制、辅助功能）、填账号密钥、装浏览器插件，或者装插件时带hooks的，新手先别装，等你熟悉了、看得懂它要什么再说。需要联网的，问清楚它把什么发给了谁。
4. 先在不重要的文件夹里试。第一次用，别在装着重要资料的目录里跑。
5. 进阶：用NVIDIA开源的SkillSpector这类扫描工具先体检一遍。注意它默认会开启大模型分析，把被扫描的文件内容发给你配置的模型服务，还要你自己准备API密钥；加上`--no-llm`才只在本机做静态检查（以它的README为准）。装它也要先有uv和Python 3.12或3.13，新手可以跳过，前四条够用。扫描通过也不等于绝对安全。

## 怎么自己写一个

最适合写成Skill的事，是你反复在做、步骤基本固定的事：每周的周报、固定格式的邮件、整理某类资料。一次性的事不值得。

不用自己敲格式。让AI替你写，你的任务是把「平时怎么做」讲清楚。Anthropic官方仓库里有一个叫`skill-creator`的Skill，专门帮人从零写Skill，也能给已有的做测试。把下面这段交给Claude Code：

```prompt 复制给 Claude Code
我想把一件反复做的事做成一个Skill，请你来带我完成。

这件事是：（用两三句话写清楚，比如「每周五把本周的销售表格整理成给老板看的一页周报」）
我平时的做法：（粘贴你的步骤、模板、满意的旧成品；没有就写「请你一步步问我」）
容易出错的地方：（比如口径、格式、客户名称的写法）

请你：
1. 先查Claude Code官方文档，确认当前对Skill格式的要求，再动手。
2. 一次只问我一个问题，补齐你需要的信息，别一次问一大堆。
3. 把Skill放在当前项目的 .claude/skills/（英文小写加短横线的名字）下面，写好SKILL.md。
4. description要写明两件事：它做什么，什么情况下该用它。写在开头，别超过两三句话。
5. SKILL.md正文控制在500行以内，细节拆到旁边的参考文件。
6. 写完后教我怎么输入 /技能名 试一次，用我给的真实材料跑一遍，再把结果和我的旧成品对比，告诉我哪里不一样。
```

写完之后，隔几天回头看：它有没有在该用的时候被用上？输出符合你的期待吗？不灵，多半是`description`没写清楚，或者步骤太笼统。让AI按实际表现改一轮，比一开始追求完美有用。

## 现成的推荐

> [!warn] 先别急着复制。Skill装好以后是在你电脑上运行的，装之前请先翻到下面的「怎么挑」，把装之前要看的东西过一遍：看一遍文件夹、插件有没有带hooks、要什么权限。标了「进阶，权限高」的，新手先别装。

下面的推荐按「你想干什么」分类，包含花叔自己开源的Skill、Anthropic官方的Skill，以及社区里口碑较好的Skill。每一条都标了来源、安装方式、许可证和需要留意的地方。

每张卡片上的灰色命令只有一条，可以整块复制。以`/plugin`开头的，在Claude Code的对话框里输入；其余的在终端里运行。需要第二步的，写在命令下面的说明里。数字（star数）带着统计日期，会变化，以仓库当前页面为准。
