"""去掉中文与英文/数字之间的空格（花叔的排版规则）。跳过代码块（prompt块除外）、行内代码、链接地址、frontmatter（title/summary/goal 除外）。"""
import re, sys, pathlib
CJK = r"[一-鿿　-〿＀-￯]"
A = r"[A-Za-z0-9`*\])%\"'.+#/]"
B = r"[A-Za-z0-9`*\[(\"'@/~$#]"
R1 = re.compile(rf"(?<={CJK}) +(?={B})")
R2 = re.compile(rf"(?<={A}) +(?={CJK})")
def fix_seg(t): return R2.sub("", R1.sub("", t))
def fix_line(line):
    pm = re.match(r"^(\s*(?:#{1,6} |[-*+] |\d+\. |> |\| )*)", line)
    prefix = pm.group(1); line = line[len(prefix):]
    out, i = [prefix], 0
    for m in re.finditer(r"`[^`]*`|\]\([^)]*\)|https?://\S+", line):
        out.append(fix_seg(line[i:m.start()])); out.append(m.group(0)); i = m.end()
    out.append(fix_seg(line[i:]))
    s = "".join(out)
    # 再处理被切开的边界：中文 + 空格 + `code` / `code` + 空格 + 中文
    s = re.sub(rf"(?<={CJK}) +(?=`)", "", s); s = re.sub(rf"(?<=`) +(?={CJK})", "", s)
    s = re.sub(rf"(?<=\)) +(?={CJK})", "", s)
    return s
changed = 0
for p in pathlib.Path("content").rglob("*.md"):
    if p.name.startswith("_"): continue
    lines = p.read_text("utf8").split("\n"); out = []; fence = None; fm = 0
    for idx, line in enumerate(lines):
        if idx == 0 and line == "---": fm = 1; out.append(line); continue
        if fm == 1:
            if line == "---": fm = 2; out.append(line); continue
            out.append(fix_line(line) if re.match(r"^(title|summary|goal):", line) else line); continue
        mm = re.match(r"^\s*(```+)(.*)$", line)
        if mm:
            if fence is None: fence = mm.group(2).strip() or "text"
            else: fence = None
            out.append(line); continue
        if fence is None or fence.startswith("prompt"): out.append(fix_line(line))
        else: out.append(line)
    new = "\n".join(out)
    if new != "\n".join(lines): p.write_text(new, "utf8"); changed += 1
print("changed", changed)
