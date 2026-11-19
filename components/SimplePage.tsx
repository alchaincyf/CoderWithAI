import PageHead from "./PageHead";

export default function SimplePage({ kicker, title, children }: { kicker: string; title: React.ReactNode; color?: string; children: React.ReactNode }) {
  return (<>
    <PageHead kicker={kicker} title={title} />
    <div className="wrap wide simple">
      <div className="card sheet narrow"><div className="prose">{children}</div></div>
    </div>
  </>);
}
