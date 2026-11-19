import type { Metadata } from "next";
import Link from "next/link";
import SimplePage from "@/components/SimplePage";

export const metadata: Metadata = {
  title: "旧版编程语言教程已下线",
  description: "CoderWithAI 2024年版的31门编程语言教程已下线，新的站点教你用AI工具直接把想法做出来。",
  alternates: { canonical: "/legacy" },
  robots: { index: false, follow: true },
};

export default function Page() {
  return (
    <SimplePage kicker="旧教程" title={<>你找的那篇教程，<br />已经下线了。</>} color="orange">
      <p>你点开的是CoderWithAI 2024年版的「编程语言教程」（Python、Java、C++等31门语言）。2026年11月，我们把整个站重做了，这批旧教程不再提供。</p>
      <p>原因很简单：今天想做出一个东西，已经不需要先学会一门编程语言的语法。AI会写代码，你需要会的是告诉它做什么、看懂它在说什么、卡住时知道怎么办。</p>
      <h2>你可能想找的</h2>
      <ul>
        <li>想用AI做出第一个东西：从 <Link href="/start/what-is-ai-coding">第0关</Link> 开始</li>
        <li>想知道某个技术词是什么意思：查 <Link href="/words">人话词典</Link>（比如 <Link href="/words/python">Python</Link>、<Link href="/words/react">React</Link>、<Link href="/words/sql">SQL</Link>）</li>
        <li>代码报错了：去 <Link href="/fix">报错急救</Link></li>
        <li>真想系统学某门语言：直接让AI当你的私教——「我想学Python，零基础，请按每天30分钟给我排一个四周计划，每天讲一个概念并出一道练习题，等我做完再讲下一个」。</li>
      </ul>
    </SimplePage>
  );
}
