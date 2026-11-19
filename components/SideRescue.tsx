import Link from "next/link";

export default function SideRescue() {
  return (
    <div className="card sc rescue-card">
      <h4>卡住了？</h4>
      <p className="sub">把终端里的红字原样贴进报错急救，拿到一段能直接发给AI的话。</p>
      <Link className="btn pri" href="/fix">去报错急救</Link>
    </div>
  );
}
