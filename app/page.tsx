import { getAvailableLanguages } from "@/lib/tutorials";
import Link from 'next/link';

interface LanguageIntro {
  [key: string]: {
    description: string;
    beginner_friendly: boolean;
    popular_uses: string[];
  }
}

const languageIntros: LanguageIntro = {
  "JavaScript": {
    description: "网页交互的必备语言，也可用于服务器端开发。",
    beginner_friendly: true,
    popular_uses: ["网页开发", "移动应用", "服务器端编程"]
  },
  "Python": {
    description: "简单易学，应用广泛，从网站到人工智能都能胜任。",
    beginner_friendly: true,
    popular_uses: ["数据分析", "人工智能", "网站后端"]
  },
  "Java": {
    description: "企业级应用的首选，也是Android开发的官方语言。",
    beginner_friendly: false,
    popular_uses: ["企业应用", "Android开发", "大数据处理"]
  },
  "C++": {
    description: "高性能计算的王者，游戏开发和系统编程的利器。",
    beginner_friendly: false,
    popular_uses: ["游戏开发", "系统编程", "嵌入式系统"]
  },
  "Ruby": {
    description: "注重简洁和生产力的语言，特别适合快速开发网站。",
    beginner_friendly: true,
    popular_uses: ["网站开发", "脚本编写", "自动化测试"]
  },
  "C#": {
    description: "微软开发的强大语言，用于Windows应用和游戏开发。",
    beginner_friendly: false,
    popular_uses: ["Windows应用", "游戏开发", "企业软件"]
  },
  "PHP": {
    description: "广泛用于Web开发的服务器端脚本语言。",
    beginner_friendly: true,
    popular_uses: ["Web开发", "内容管理系统", "电子商务"]
  },
  "Swift": {
    description: "苹果公司开发的用于iOS和macOS应用开发的语言。",
    beginner_friendly: true,
    popular_uses: ["iOS应用", "macOS应用", "服务器端Swift"]
  },
  "Go": {
    description: "Google开发的简洁、高效的编程语言，适合并发编程。",
    beginner_friendly: false,
    popular_uses: ["云计算", "网络编程", "系统工具"]
  },
  "Rust": {
    description: "注重安全和并发的系统编程语言。",
    beginner_friendly: false,
    popular_uses: ["系统编程", "Web Assembly", "游戏引擎"]
  },
  "TypeScript": {
    description: "JavaScript的超集，添加了静态类型检查。",
    beginner_friendly: false,
    popular_uses: ["大型Web应用", "Angular开发", "Node.js"]
  },
  "Kotlin": {
    description: "现代化的Java替代品，用于Android开发和服务器端编程。",
    beginner_friendly: true,
    popular_uses: ["Android开发", "服务器端开发", "跨平台移动开发"]
  },
  "Scala": {
    description: "结合了面向对象和函数式编程的特性，运行在JVM上。",
    beginner_friendly: false,
    popular_uses: ["大数据处理", "分布式系统", "Web应用开发"]
  },
  "R": {
    description: "专门用于统计计算和图形化的编程语言。",
    beginner_friendly: true,
    popular_uses: ["数据分析", "统计计算", "机器学习"]
  },
  "MATLAB": {
    description: "用于科学计算和工程的高级技术计算语言。",
    beginner_friendly: false,
    popular_uses: ["数值分析", "信号处理", "图像处理"]
  },
  "Dart": {
    description: "Google开发的用于构建移动、桌面、服务器和网络应用程序的语言。",
    beginner_friendly: true,
    popular_uses: ["移动应用开发", "Web开发", "服务器端编程"]
  },
  "Lua": {
    description: "轻量级、高效的脚本语言，常用于游戏开发。",
    beginner_friendly: true,
    popular_uses: ["游戏脚本", "嵌入式系统", "Web应用"]
  },
  "Perl": {
    description: "强大的文本处理能力，适用于系统管理和网络编程。",
    beginner_friendly: false,
    popular_uses: ["文本处理", "系统管理", "网络编程"]
  },
  "Haskell": {
    description: "纯函数式编程语言，具有强大的类型系统。",
    beginner_friendly: false,
    popular_uses: ["学术研究", "金融分析", "并行编程"]
  },
  "Julia": {
    description: "高性能的动态编程语言，适用于科学计算和数据分析。",
    beginner_friendly: false,
    popular_uses: ["科学计算", "数据分析", "机器学习"]
  },
  "HTML": {
    description: "用于创建网页结构的标记语言。",
    beginner_friendly: true,
    popular_uses: ["网页开发", "邮件模板", "静态网站"]
  },
  "CSS": {
    description: "用于设计网页样式和布局的样式表语言。",
    beginner_friendly: true,
    popular_uses: ["网页设计", "响应式布局", "动画效果"]
  },
  "Next.js": {
    description: "基于React的服务端渲染框架，用于构建现代化的Web应用。",
    beginner_friendly: false,
    popular_uses: ["服务端渲染应用", "静态网站生成", "全栈Web开发"]
  },
  "Node.js": {
    description: "基于Chrome V8引擎的JavaScript运行时，用于服务器端编程。",
    beginner_friendly: true,
    popular_uses: ["后端开发", "实时应用", "命令行工具"]
  },
  "React": {
    description: "用于构建用户界面的JavaScript库。",
    beginner_friendly: false,
    popular_uses: ["单页应用开发", "移动应用开发", "大型Web应用"]
  }
};

export default async function Home() {
  const languages = await getAvailableLanguages();

  return (
    <div className="container mx-auto px-4 py-8">
      <h1 className="text-4xl font-bold mb-8 text-center">欢迎来到 CoderWithAI</h1>
      <p className="text-xl mb-8 text-center">选择一门编程语言开始你的学习之旅！</p>
      <div className="grid md:grid-cols-2 lg:grid-cols-3 gap-6">
        {languages.map((lang) => {
          const intro = languageIntros[lang] || {
            description: "一个强大而有趣的编程语言。",
            beginner_friendly: true,
            popular_uses: ["各种应用开发"]
          };
          return (
            <div key={lang} className="bg-white shadow-md rounded-lg p-6">
              <h2 className="text-2xl font-bold mb-4">{lang}</h2>
              <p className="mb-4">{intro.description}</p>
              <p className="mb-2">
                适合初学者：
                <span className={intro.beginner_friendly ? "text-green-500" : "text-yellow-500"}>
                  {intro.beginner_friendly ? "是" : "需要一些编程基础"}
                </span>
              </p>
              <p className="mb-2">常见用途：</p>
              <ul className="list-disc list-inside mb-4">
                {intro.popular_uses.map((use, index) => (
                  <li key={index}>{use}</li>
                ))}
              </ul>
              <Link 
                href={`/${lang}`} 
                className="bg-blue-500 text-white px-4 py-2 rounded hover:bg-blue-600 transition-colors"
              >
                开始学习 {lang}
              </Link>
            </div>
          );
        })}
      </div>
    </div>
  );
}
