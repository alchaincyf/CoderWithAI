// 栏目页头：小标签、宋体标题、一段说明。朱笔只用来划一个词。
export default function PageHead({ kicker, title, children, narrow = false }: { kicker: string; title: React.ReactNode; children?: React.ReactNode; narrow?: boolean }) {
  return (
    <header className="p-head">
      <div className="wrap wide">
        <span className="pill plain">{kicker}</span>
        <h1>{title}</h1>
        {children && <p className={`sum${narrow ? " narrow" : ""}`}>{children}</p>}
      </div>
    </header>
  );
}
