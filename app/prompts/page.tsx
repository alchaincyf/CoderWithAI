import type { Metadata } from "next";
import fs from "node:fs";
import path from "node:path";
import PageHead from "@/components/PageHead";
import Mk from "@/components/Mk";
import PromptCoach, { type Scenario } from "@/components/PromptCoach";

export const metadata: Metadata = {
  title: "怎么跟AI说话：提需求、修bug、改设计、要流量的提示词技巧",
  description: "花叔从自己和AI的真实对话里提炼的提示词技巧，按场景分：提需求、修bug、改设计、要流量、加功能、先出方案、让AI验收、回退、放手跑长任务。每个场景都能填空生成一段直接发给AI的话。",
  alternates: { canonical: "/prompts" },
};

export default function Page() {
  const data: Scenario[] = JSON.parse(fs.readFileSync(path.join(process.cwd(), "content", "prompts.json"), "utf8"));
  return (<>
    <PageHead kicker={`提示词技巧 · ${data.length}个场景`} title={<>不会写代码，<br />要会<Mk k="u">跟AI说话</Mk>。</>}>
      这些技巧来自花叔自己和AI的真实对话、知识星球和公众号，不是网上抄的通用模板。先挑你现在的场景，看几条技巧，再把右边的空填上，生成一段能直接发给AI的话。
    </PageHead>
    <div className="wrap wide"><PromptCoach data={data} /></div>
  </>);
}
