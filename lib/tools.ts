// 工具事实库（核实于 2026-11-18；来源：research/tools-facts.json，每条附官方定价页）
// 价格只写官方页上的标价；medium/low 置信度的在页面上提示「以官网为准」。
export type Cat = "web" | "ide" | "cli";
export interface Tool {
  id: string;
  name: string;
  maker: string;
  cat: Cat;
  what: string;
  free: string;
  paid: string;
  cn: "yes" | "no" | "unknown";
  cnNote: string;
  bestFor: string;
  weak: string;
  site: string;
  pricing: string;
  confidence: "high" | "medium" | "low";
}

export const VERIFIED = "2026-11-18";

export const CAT_LABEL: Record<Cat, string> = {
  web: "网页一句话生成",
  ide: "AI编辑器",
  cli: "命令行Agent",
};

export const TOOLS: Tool[] = [
  {
    id: "claude-code", name: "Claude Code", maker: "Anthropic", cat: "cli",
    what: "装在电脑上的AI程序员：你用中文说需求，它自己读文件、写代码、跑命令、修错。也有桌面App、编辑器插件和网页版。",
    free: "没有免费档（Claude免费版不含Claude Code）",
    paid: "Pro 20美元/月；Max 100美元/月起",
    cn: "no", cnNote: "中国大陆不在Anthropic官方受支持地区名单内",
    bestFor: "想把一个真项目从零做完并长期维护的人；Skills、MCP、子代理生态最完整",
    weak: "大陆不能直接合规使用；终端界面对新手有门槛",
    site: "https://code.claude.com/docs/en/overview", pricing: "https://claude.com/pricing", confidence: "high",
  },
  {
    id: "codex", name: "OpenAI Codex", maker: "OpenAI", cat: "cli",
    what: "OpenAI的AI程序员，有终端版、ChatGPT桌面和手机里的Codex、编辑器插件、云端任务。用ChatGPT账号登录。",
    free: "ChatGPT免费版可做轻量任务",
    paid: "Go 8美元/月（轻量）；Plus 20美元/月；Pro 100美元/月起",
    cn: "no", cnNote: "OpenAI官方受支持国家名单不含中国大陆，名单外使用可能被封号",
    bestFor: "已经订了ChatGPT的人；想要网页、手机、终端多个入口的人",
    weak: "用量按5小时窗口加周限额估算，不透明",
    site: "https://learn.chatgpt.com/docs", pricing: "https://learn.chatgpt.com/docs/pricing", confidence: "medium",
  },
  {
    id: "kimi-code", name: "Kimi Code", maker: "月之暗面", cat: "cli",
    what: "Kimi出的AI程序员，有命令行、桌面端和VS Code插件，用Kimi会员登录。",
    free: "有免费档",
    paid: "Plus 19美元/月起（美元区页面标价，人民币价以官网为准）",
    cn: "yes", cnNote: "官方文档提供大陆专用接口地址",
    bestFor: "想在国内网络下用命令行Agent的人",
    weak: "会员档位和模型权限的对应关系写得不够清楚",
    site: "https://www.kimi.com/code", pricing: "https://www.kimi.com/membership/pricing", confidence: "medium",
  },
  {
    id: "qwen-code", name: "Qwen Code", maker: "阿里通义千问团队", cat: "cli",
    what: "开源的终端AI编程工具，可以接通义千问，也能接DeepSeek、Kimi、智谱等任意模型。",
    free: "工具开源免费，模型要自己准备（订阅或API Key）",
    paid: "按所接模型计费",
    cn: "yes", cnNote: "安装包托管在国内，可接中国站模型",
    bestFor: "想用开源工具、不想绑死一家模型的人",
    weak: "没有开箱即用的免费额度，新手要先搞懂接哪个模型",
    site: "https://github.com/QwenLM/qwen-code", pricing: "https://qwenlm.github.io/qwen-code-docs/en/users/overview/", confidence: "medium",
  },
  {
    id: "antigravity", name: "Google Antigravity", maker: "Google", cat: "cli",
    what: "Google的Agent平台：桌面App、命令行（agy）和SDK，可以并行派多个AI干活。Gemini CLI的个人免费用户已迁到这里。",
    free: "个人免费档（额度按周刷新，具体数字官方未写）",
    paid: "随Google AI Pro / Ultra订阅提升额度",
    cn: "unknown", cnNote: "Google服务在大陆通常需要海外网络，请自行确认",
    bestFor: "有Google账号、想免费试Agent式编程的人",
    weak: "免费额度不透明，可用模型变动频繁",
    site: "https://antigravity.google/", pricing: "https://antigravity.google/pricing", confidence: "medium",
  },
  {
    id: "trae-cn", name: "TRAE 国内版", maker: "字节跳动", cat: "ide",
    what: "字节的AI编程工具国内版：像写文档一样跟AI说需求，它在编辑器里把项目做出来。免费版也能用全部功能。",
    free: "免费版每月500积分，所有功能可用",
    paid: "Lite 低至39元/月、Pro 低至69元/月（页面标价49元、99元，「低至」的条件以官网为准）",
    cn: "yes", cnNote: "国内版，人民币计价",
    bestFor: "国内零基础用户，先用免费额度上手",
    weak: "积分消耗规则不直观，难预估一个项目要多少积分",
    site: "https://www.trae.cn", pricing: "https://www.trae.cn/pricing", confidence: "high",
  },
  {
    id: "qoder-cn", name: "Qoder 国内版", maker: "Qoder", cat: "ide",
    what: "「智能体编程平台」：桌面IDE、手机端、JetBrains插件、命令行和云端Agent一套。",
    free: "体验版有限额度，新用户含2周专业版试用",
    paid: "专业版59元/月（新客首月有优惠）",
    cn: "yes", cnNote: "国内站，人民币",
    bestFor: "想要IDE、手机、命令行全家桶的国内用户",
    weak: "积分制，促销价多，下单前看清",
    site: "https://qoder.cn", pricing: "https://qoder.cn/pricing", confidence: "high",
  },
  {
    id: "codebuddy", name: "CodeBuddy", maker: "腾讯", cat: "ide",
    what: "腾讯的AI编程三件套：独立IDE、编辑器插件、命令行。国内版微信扫码登录。",
    free: "体验版每月500积分",
    paid: "标准版约70元/月（折后价，以页面实时价为准）",
    cn: "yes", cnNote: "国内版，微信登录，人民币",
    bestFor: "国内用户想零成本上手",
    weak: "折扣、限免、积分叠加，价格不好比",
    site: "https://www.codebuddy.cn", pricing: "https://www.codebuddy.cn/pricing/", confidence: "medium",
  },
  {
    id: "cursor", name: "Cursor", maker: "Cursor", cat: "ide",
    what: "长得像VS Code的AI编辑器，右边对话框说需求它就改代码；也有命令行和云端Agent。",
    free: "免费档有有限的Agent和对话次数",
    paid: "付费档从每月数美元到200美元不等（档位近期改过，以官网为准）",
    cn: "unknown", cnNote: "官方页未写，请自行确认",
    bestFor: "想要VS Code手感加AI、愿意看代码改动的人",
    weak: "用量池计费，第三方模型另算，成本难预估",
    site: "https://cursor.com", pricing: "https://cursor.com/pricing", confidence: "low",
  },
  {
    id: "lovable", name: "Lovable", maker: "Lovable", cat: "web",
    what: "在网页里一句话做出能上线的网站或应用，不用装任何东西，对话继续改。",
    free: "每天5个构建额度（每月最多30）",
    paid: "Pro 25美元/月",
    cn: "unknown", cnNote: "请自行确认",
    bestFor: "想最快看到成品的完全新手",
    weak: "额度用完就得升级；项目复杂后不好接手",
    site: "https://lovable.dev", pricing: "https://lovable.dev/pricing", confidence: "medium",
  },
  {
    id: "bolt", name: "Bolt.new", maker: "StackBlitz", cat: "web",
    what: "聊天里描述需求，在浏览器里直接生成网站和应用，自带托管、数据库和登录。",
    free: "每月100万token，每天上限30万",
    paid: "Pro 25美元/月",
    cn: "unknown", cnNote: "请自行确认",
    bestFor: "快速做原型、落地页",
    weak: "按token计量，复杂项目容易烧光",
    site: "https://bolt.new", pricing: "https://bolt.new/pricing", confidence: "medium",
  },
  {
    id: "v0", name: "v0", maker: "Vercel", cat: "web",
    what: "Vercel的AI建站工具：描述需求或上传设计图，生成界面和应用，一键发布到Vercel。",
    free: "每天7条消息",
    paid: "Plus 30美元/月（页面有促销标价，以官网为准）",
    cn: "unknown", cnNote: "请自行确认",
    bestFor: "要做好看前端界面的人",
    weak: "偏前端",
    site: "https://v0.app", pricing: "https://v0.app/pricing", confidence: "medium",
  },
  {
    id: "replit", name: "Replit Agent", maker: "Replit", cat: "web",
    what: "在浏览器里把想法变成带登录、数据库、托管的完整应用，一站式。",
    free: "有免费模式（额度以官网为准）",
    paid: "Core 20美元/月",
    cn: "unknown", cnNote: "请自行确认",
    bestFor: "不想自己拼数据库、托管等服务的人",
    weak: "高档位说明不全",
    site: "https://replit.com", pricing: "https://replit.com/pricing", confidence: "medium",
  },
];

// 最近一年的变化：老教程里最容易过期的地方。每条都有官方出处。
export const CHANGES = [
  { date: "2026-11-18", what: "Google Antigravity免费档里的Claude Sonnet 4.6、Opus 4.6、GPT-OSS-120b下线", src: "https://antigravity.google/pricing" },
  { date: "2026年", what: "Firebase Studio已不再支持新建工作区和新用户注册，正在关停；官方建议迁到Google AI Studio或Antigravity", src: "https://firebase.google.com/docs/studio" },
  { date: "2026-06-18", what: "Gemini CLI的个人免费用户迁到Antigravity CLI（命令agy），Gemini CLI主要留给企业许可证和API Key用户", src: "https://github.com/google-gemini/gemini-cli" },
  { date: "2026-06-02", what: "Windsurf更名为Devin Desktop，旧定价页跳转到devin.ai", src: "https://devin.ai/pricing" },
  { date: "2026年", what: "Lemon Squeezy官网写明不再接受新注册，将于2027-03-31关闭——海外收款的老教程如果还推荐它，已经过时", src: "https://www.lemonsqueezy.com" },
];
