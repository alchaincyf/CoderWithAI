import type { Metadata } from "next";
import Link from "next/link";
import SimplePage from "@/components/SimplePage";

export const metadata: Metadata = {
  title: "关于本站：为什么做一个教不会写代码的人用AI的网站",
  description: "CoderWithAI是一个免费的AI编程入门站，作者花叔不会写代码，他的所有产品都是AI写的。",
  alternates: { canonical: "/about" },
};

export default function Page() {
  return (
    <SimplePage kicker="关于本站" title={<>你负责想清楚，<br />AI负责写出来。</>} color="lemon">
      <h2>这个站是做什么的</h2>
      <p>CoderWithAI教不会写代码的人，用Claude Code、Codex、TRAE、Cursor这类AI工具，把一个想法做成能上线、能分享、甚至能收钱的东西。</p>
      <p>我们不教编程语法。AI已经能把代码写得很好了，现在挡在普通人前面的不是代码，而是这些东西：听不懂AI在说什么、不知道下一步该让它做什么、终端里一串红字不知道怎么办、网站做好了不知道怎么让别人打开、不知道哪些钱该花哪些不该花。这个站就是为这些时刻写的。</p>
      <h2>为什么是我们</h2>
      <p>本站作者花叔从来不会写代码，他的所有产品都是AI写的。2024年底，他用Cursor做出了「小猫补光灯」。后来他写了《Claude Code从入门到精通》等一系列「橙皮书」，可以在 <a href="https://www.huasheng.ai" target="_blank" rel="noopener">huasheng.ai</a> 免费读到。</p>
      <p>这个站里的路线、配方和急救，都按一个标准写：一个完全不会写代码的人照着做，能做出来。</p>
      <h2>这个站的规矩</h2>
      <ul>
        <li><strong>准确优先。</strong>工具价格、额度、能不能在国内用，变化很快。每一条都写了核实日期，并链到官方页面。查不到的，我们写「请自行确认」，不替你猜。</li>
        <li><strong>不教违规的事。</strong>比如绕过地区限制、共享账号。我们会如实告诉你哪些工具在国内用不了，以及国内线怎么走。</li>
        <li><strong>全部免费。</strong>不需要注册，闯关进度存在你自己的浏览器里。</li>
      </ul>
      <h2>2024年的老教程去哪了</h2>
      <p>这个站2024年上线时，是一套「31门编程语言」的AI生成教程。两年过去，AI编程的样子完全变了，学语法已经不是普通人入门的第一步，所以我们在2026年11月把整个站重做了。老教程的说明见 <Link href="/legacy">这里</Link>。</p>
      <h2>写错了怎么办</h2>
      <p>发现过时或写错的内容，欢迎在 <a href="https://github.com/alchaincyf/CoderWithAI/issues" target="_blank" rel="noopener">GitHub Issues</a> 告诉我们。</p>
    </SimplePage>
  );
}
