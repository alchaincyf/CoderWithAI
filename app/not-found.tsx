import Link from "next/link";
import SimplePage from "@/components/SimplePage";

export default function NotFound() {
  return (
    <SimplePage kicker="404" title={<>这一页不存在。</>}>
      <p>可能是链接写错了，也可能是这页被移走了。试试右上角的搜索，或者：</p>
      <ul>
        <li><Link href="/">回首页</Link></li>
        <li><Link href="/start">看闯关路线</Link></li>
        <li><Link href="/fix">去报错急救</Link></li>
      </ul>
    </SimplePage>
  );
}
