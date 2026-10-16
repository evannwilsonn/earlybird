"""Builds web/index.html from web/template.html and web/model.json (run from the repo root)."""
import json, pathlib
web = pathlib.Path(__file__).parent
data = json.dumps(json.loads((web / "model.json").read_text()), separators=(",", ":")).replace("</", "<\\/")
body = (web / "template.html").read_text().replace("__DATA__", data)
head = ('<!doctype html><html lang="en"><head><meta charset="utf-8">'
        '<meta name="viewport" content="width=device-width,initial-scale=1,viewport-fit=cover">'
        '<style>:root{padding-top:env(safe-area-inset-top,0px);padding-bottom:env(safe-area-inset-bottom,0px)}'
        'body{margin:0}img{max-width:100%}[hidden]{display:none!important}</style></head><body>')
html = head + body + "</body></html>"
(web / "index.html").write_text(html)
# GitHub Pages serves docs/, so the light version also lives at /earlybird/quick/
(web.parent / "docs" / "quick").mkdir(exist_ok=True)
(web.parent / "docs" / "quick" / "index.html").write_text(html)
print("wrote web/index.html and docs/quick/index.html")
