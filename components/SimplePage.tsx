export default function SimplePage({ kicker, title, color = "white", children }: { kicker: string; title: React.ReactNode; color?: string; children: React.ReactNode }) {
  return (<>
    <header className={`sec-head ${color}`}><div className="wrap"><div><span className="lab">{kicker}</span><h1>{title}</h1></div></div></header>
    <div className="wrap"><div className="prose" style={{ maxWidth: 760, padding: "44px 0 88px" }}>{children}</div></div>
  </>);
}
