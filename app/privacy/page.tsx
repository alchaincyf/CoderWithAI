import type { Metadata } from "next";
import SimplePage from "@/components/SimplePage";

export const metadata: Metadata = { title: "隐私政策", alternates: { canonical: "/privacy" } };

export default function Page() {
  return (
    <SimplePage kicker="隐私政策" title="隐私政策">
      <p>更新于2026年11月18日。</p>
      <h2>我们不收集什么</h2>
      <p>本站不需要注册登录。闯关进度、需求说明书生成器里填的内容，只保存在你自己设备的浏览器本地存储（localStorage）里，不会上传到我们的服务器。你清除浏览器数据，它们就没了。</p>
      <h2>我们收集什么</h2>
      <p>本站使用Google Analytics统计访问情况（比如哪些页面被看得多、访客大致来自哪里），用来改进内容。它会使用Cookie，收集的是匿名的访问数据，比如浏览器类型、来源页面、访问时长。你可以用浏览器设置或拦截插件拒绝这些Cookie，不影响使用本站。</p>
      <p>本站展示Google AdSense广告。Google及其合作伙伴可能使用Cookie，根据你访问本站和其他网站的情况投放广告。你可以在 <a href="https://adssettings.google.com" target="_blank" rel="noopener">Google广告设置</a> 中关闭个性化广告，了解更多见 <a href="https://policies.google.com/technologies/ads" target="_blank" rel="noopener">Google的广告政策</a>。</p>
      <p>本站托管在Vercel上，服务器会按其常规做法记录访问日志（如IP地址、请求时间），用于安全和故障排查。</p>
      <h2>第三方链接</h2>
      <p>本站会链接到各个工具的官方网站。离开本站后，请以对方网站的隐私政策为准。</p>
      <h2>联系我们</h2>
      <p>对隐私有任何疑问，可以在 <a href="https://github.com/alchaincyf/CoderWithAI/issues" target="_blank" rel="noopener">GitHub Issues</a> 联系我们。</p>
    </SimplePage>
  );
}
