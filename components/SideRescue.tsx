import Link from "next/link";

export default function SideRescue() {
  return (
    <div className="side-card ink">
      <h3>卡住了？</h3>
      <p style={{ marginBottom: 10 }}>把终端里的红字原样贴进报错急救，拿到一段能直接发给AI的话。</p>
      <Link href="/fix">去报错急救 →</Link>
    </div>
  );
}
