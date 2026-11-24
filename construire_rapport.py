# Construit rapport.html à partir du README (même texte), puis l'imprimer en PDF avec Chrome headless.
# Lancer depuis le dossier du projet : python construire_rapport.py
import re, markdown, pathlib

ici = pathlib.Path(__file__).resolve().parent
md = (ici / "README.md").read_text(encoding="utf-8")
md = md.split("\n## Reproduire")[0]                       # le rapport s'arrête avant les instructions techniques
md = re.sub(r"\n\| \| \|\n\|---\|---\|\n(\|.*\|\n?)+", "\n", md)   # tableau d'images de l'annexe : retiré
titre, corps = md.split("\n", 1)
html = markdown.markdown(corps, extensions=["tables"])
html = html.replace('src="sorties/', f'src="file:///{ici.as_posix()}/sorties/')
html = re.sub(r"<h2>", '<h2 class="saut">', html)         # chaque partie commence sur une nouvelle page
html = html.replace('<h2 class="saut">1.', "<h2>1.", 1)
html = re.sub(r"<p>(<img[^>]+>)</p>", r"<figure>\1</figure>", html)

page = f"""<!doctype html><html lang="fr"><head><meta charset="utf-8"><title>Une commandite pour la France</title><style>
@page {{ size: A4; margin: 16mm 17mm; }}
body {{ font-family: Calibri, "Segoe UI", Arial, sans-serif; color: #1B2A3A; font-size: 11pt; line-height: 1.45; margin: 0;
       -webkit-print-color-adjust: exact; print-color-adjust: exact; background: #fff; }}
h1 {{ color: #1F5A8C; font-size: 23pt; line-height: 1.15; margin: 0 0 3mm; }}
.auteur {{ font-size: 9pt; color: #4A5568; margin-bottom: 5mm; }}
h2 {{ color: #1F5A8C; font-size: 15pt; margin: 6mm 0 3mm; }}
h3 {{ color: #2E8B57; font-size: 12.5pt; margin: 5mm 0 2mm; }}
p {{ margin: 0 0 3mm; text-align: justify; hyphens: auto; }}
strong {{ color: #1B2A3A; }}
figure {{ margin: 2mm 0 4mm; break-inside: avoid; }}
figure img {{ width: 100%; max-height: 125mm; object-fit: contain; }}
table {{ border-collapse: collapse; width: 100%; font-size: 9.5pt; margin: 2mm 0 4mm; break-inside: avoid; }}
th {{ background: #1F5A8C; color: #fff; text-align: left; padding: 2mm; }}
td {{ border-bottom: 1px solid #E3E8EE; padding: 2mm; vertical-align: top; }}
ul {{ margin: 0 0 3mm 5mm; padding-left: 4mm; }} li {{ margin-bottom: 1.5mm; }}
a {{ color: #1F5A8C; text-decoration: none; }}
.saut {{ break-before: page; }}
</style></head><body>
<h1>{titre.lstrip("# ")}</h1>
<div class="auteur">Baptiste Leclerc · novembre 2026 · données, code et limites : github.com/leclercbaptiste1085-hash/6-commandite</div>
{html}
</body></html>"""
(ici / "rapport.html").write_text(page, encoding="utf-8")
print("rapport.html écrit")
