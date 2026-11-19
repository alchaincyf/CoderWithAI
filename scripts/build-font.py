"""把站内所有标题会用到的字，从思源宋体可变字体实例化到 wght=650（接近 macOS 宋体 Bold）后裁出来，生成 public/fonts/cwa-title.woff2。
内容新增后在本机跑一次：python3 scripts/build-font.py（源字体在 ../fonts-src，不进仓库）。"""
import re, pathlib
from fontTools import subset
from fontTools.ttLib import TTFont
from fontTools.varLib import instancer

WGHT = 650

root = pathlib.Path(__file__).resolve().parent.parent
src = root.parent / "fonts-src" / "NotoSerifSC-VF.ttf"
chars = set()
for p in list((root / "content").rglob("*.md")):
    t = p.read_text("utf8")
    fm = re.match(r"^---\n(.*?)\n---", t, re.S)
    if fm: chars.update(re.findall(r"^title:\s*(.*)$", fm.group(1), re.M)[0] if re.findall(r"^title:\s*(.*)$", fm.group(1), re.M) else "")
    for h in re.findall(r"^#{2,3} (.*)$", t, re.M): chars.update(h)
for p in list((root / "app").rglob("*.tsx")) + list((root / "components").rglob("*.tsx")) + list((root / "lib").rglob("*.ts")):
    chars.update(re.findall(r"[　-〿一-鿿＀-￯]", p.read_text("utf8")))
chars.update(chr(c) for c in range(0x20, 0x7f))
chars.update("「」『』（）：；，。、？！…—·《》“”‘’0123456789")
text = "".join(sorted(chars))
opts = subset.Options(); opts.flavor = "woff2"; opts.layout_features = ["*"]; opts.name_IDs = ["*"]
vf = TTFont(str(src))
vf = instancer.instantiateVariableFont(vf, {"wght": WGHT})
inst = root.parent / "fonts-src" / f"NotoSerifSC-{WGHT}.ttf"
vf.save(str(inst))
f = subset.load_font(str(inst), opts)
s = subset.Subsetter(opts); s.populate(text=text); s.subset(f)
out = root / "public" / "fonts" / "cwa-title.woff2"
subset.save_font(f, str(out), opts)
print(len(text), "chars ->", out, out.stat().st_size // 1024, "KB")
