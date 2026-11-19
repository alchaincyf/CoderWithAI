import type { Metadata } from "next";
import PageHead from "@/components/PageHead";
import Mk from "@/components/Mk";
import GroupedIndex from "@/components/GroupedIndex";
import RescueDesk from "@/components/RescueDesk";
import { getDocs } from "@/lib/content";

export const metadata: Metadata = {
  title: "报错急救：粘贴报错，看人话解释和修复提示词",
  description: "用AI编程时终端里出现一串红字？把报错原样贴进来，得到人话解释和一段能直接发给Claude Code、Codex、Cursor的修复提示词。收录新手最常见的报错。",
  alternates: { canonical: "/fix" },
};

export default function Page() {
  const docs = getDocs("fix");
  return (<>
    <PageHead kicker={`报错急救 · ${docs.length}条`} title={<>红字不可怕，<br />先<Mk k="u">翻成人话</Mk>。</>}>
      把终端、浏览器或部署平台里的报错整段贴进来。能匹配上的，给你这条报错的解释和修复提示词；匹配不上的，给你一段通用的求助话术。
    </PageHead>
    <section className="wrap wide fix-desk"><RescueDesk initial="" /></section>
    <div className="wrap wide" style={{ paddingBottom: 80 }}>
      <GroupedIndex docs={docs} order={["安装与环境", "网络", "Node与npm", "页面问题", "Git", "部署上线", "数据与接口", "AI工具", "手机App"]} />
    </div>
  </>);
}
