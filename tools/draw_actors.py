# /// script
# requires-python = ">=3.11"
# dependencies = ["reportlab", "pillow"]
# ///
"""Draw the vignette's actor figure: the APB article's TOC regions, redrawn for apbCatalogR.

The integrator's region holds the APB result and the catalog in the centre; the Bioconductor
developer's overlaps it only around the apb-catalog API, in R, Python and TypeScript, through which
applications such as prolfqua, exploreDE and QFeatures read the result. Run with
`uv run tools/draw_actors.py`.
"""

import subprocess
import tempfile
from pathlib import Path

from PIL import Image
from reportlab.graphics import renderPDF
from reportlab.graphics.shapes import Circle, Drawing, Line, Polygon, Rect, String
from reportlab.lib.colors import HexColor, white
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.ttfonts import TTFont

WIDTH, HEIGHT = 348, 126
OUTPUT = Path(__file__).resolve().parents[1] / "vignettes" / "figures" / "apb_actors.png"
FONT_ROOT = Path("/System/Library/Fonts/Supplemental")
pdfmetrics.registerFont(TTFont("Arial", str(FONT_ROOT / "Arial.ttf")))
pdfmetrics.registerFont(TTFont("Arial-Bold", str(FONT_ROOT / "Arial Bold.ttf")))
INK, EDGE = HexColor("#263238"), HexColor("#60716D")
FILL = {  # box fills of the article's Figure 1
    "tool": HexColor("#EEF6F2"),
    "rules": HexColor("#F4EEF8"),
    "result": HexColor("#EAF2F8"),
    "external": HexColor("#F5F8FA"),
}
REGION = {  # stroke, dash: the line types of the article's Figure 1
    "developer": (HexColor("#B07D2B"), [5, 2.5]),
    "integrator": (HexColor("#2F6F8F"), None),
    "application": (HexColor("#8C5A9E"), [6, 2, 1, 2]),
}
d = Drawing(WIDTH, HEIGHT)
d.initialFontName = "Arial"
d.add(Rect(0, 0, WIDTH, HEIGHT, fillColor=white, strokeColor=None))


def label(text, x, y, size=8, color=INK, font="Arial", within=None):
    extent = pdfmetrics.stringWidth(text, font, size)
    left, right = within or (3, WIDTH - 3)
    assert left <= x - extent / 2 and x + extent / 2 <= right, text
    d.add(String(x, y, text, fontName=font, fontSize=size, fillColor=color, textAnchor="middle"))


def line(x1, y1, x2, y2, color=EDGE, width=0.6):
    d.add(Line(x1, y1, x2, y2, strokeColor=color, strokeWidth=width))


def arrow(points, color=EDGE, head=2.5):
    for a, b in zip(points, points[1:]):
        line(*a, *b, color=color)
    x, y = points[-1]
    px, py = points[-2]
    if x > px:
        vertices = [x, y, x - head, y + head / 2, x - head, y - head / 2]
    else:
        vertices = [x, y, x - head / 2, y - head, x + head / 2, y - head]
    d.add(Polygon(vertices, fillColor=color, strokeColor=None))


def actor(x, head_y):
    d.add(Circle(x, head_y, 2.5, fillColor=white, strokeColor=EDGE, strokeWidth=0.5))
    line(x, head_y - 2.5, x, head_y - 10)
    line(x - 4, head_y - 6, x + 4, head_y - 6)
    line(x, head_y - 10, x - 4, head_y - 15)
    line(x, head_y - 10, x + 4, head_y - 15)


def region(x, y, w, h, who):
    stroke, dash = REGION[who]
    d.add(Rect(x, y, w, h, rx=6, ry=6, fillColor=None, strokeColor=stroke, strokeWidth=1.0, strokeDashArray=dash))


def box(x, y, w, h, kind, lines, font="Arial"):
    """Box in the fill of its kind, its lines centred; external data and software get a dotted border."""
    rounded = 3 if kind == "tool" else 0
    d.add(Rect(x, y, w, h, rx=rounded, ry=rounded, fillColor=FILL[kind], strokeColor=EDGE, strokeWidth=0.6,
               strokeDashArray=[1, 1] if kind == "external" else None))
    for index, text in enumerate(lines):
        label(text, x + w / 2, y + h / 2 - 2.9 + (len(lines) - 1) * 4.75 - index * 9.5, font=font, within=(x, x + w))


# One region per actor. The developer's overlaps the integrator's around apb2 and the parsing rules;
# the Bioconductor developer's overlaps it only around the apb-catalog API, not the result.
region(3, 6, 119, 114, "developer")
region(74, 4, 194, 118, "integrator")
region(190, 6, 155, 114, "application")
for x, names in ((38.5, ("Developer,", "researcher")), (158, ("Integrator",)), (310, ("Bioconductor", "developer"))):
    actor(x, 116)
    for index, name in enumerate(names):
        label(name, x, 92 - 8 * index)
# The bridge's name under the integrator, in its colour.
label("A ProteoBridge", 158, 84, color=REGION["integrator"][0], font="Arial-Bold")

# Quantification software as a deck of cards, back to front: two commercial tools, two open source.
for index, name in enumerate(("DIA-NN", "Spectronaut", "AlphaDIA", "Sage", "…")):
    x, top = 7 + 2 * index, 72 - 10.5 * index
    d.add(Rect(x, top - 18, 56, 18, fillColor=FILL["external"], strokeColor=EDGE, strokeWidth=0.6,
               strokeDashArray=[1, 1]))
    label(name, x + 28, top - 7.6, within=(x, x + 56))

# Conversion along the top row, its rules below; the result and the catalog in the centre.
box(78, 48, 42, 24, "tool", ("apb2",))
box(78, 12, 42, 24, "rules", ("Parsing", "rules"))
box(136, 48, 44, 24, "result", ("APB", "result"))
box(136, 12, 44, 24, "rules", ("Catalog",))
arrow([(65, 60), (78, 60)])
arrow([(120, 60), (136, 60)])
arrow([(99, 36), (99, 48)])
arrow([(120, 24), (136, 24)])

# The apb-catalog API reads the result through the catalog, in three languages; R is apbCatalogR.
d.add(Rect(194, 12, 70, 60, rx=3, ry=3, fillColor=FILL["tool"], strokeColor=EDGE, strokeWidth=0.6))
label("apb-catalog API", 229, 62.5, font="Arial-Bold", within=(194, 264))
for index, name in enumerate(("R: apbCatalogR", "Python", "TypeScript")):
    box(198, 44 - 15 * index, 62, 12, "result", (name,), font="Arial-Bold" if index == 0 else "Arial")
arrow([(180, 60), (194, 60)])
arrow([(180, 24), (194, 24)])
arrow([(264, 60), (276, 60)])

# Bioconductor applications reading the result as a deck of cards, back to front.
for index, name in enumerate(("prolfqua", "exploreDE", "QFeatures", "…")):
    x, top = 276 + 2.5 * index, 76 - 12 * index
    d.add(Rect(x, top - 26, 62, 26, rx=3, ry=3, fillColor=FILL["tool"], strokeColor=EDGE, strokeWidth=0.6))
    label(name, x + 31, top - 8.9)

OUTPUT.parent.mkdir(parents=True, exist_ok=True)
with tempfile.TemporaryDirectory() as scratch:
    pdf = Path(scratch) / "apb_actors.pdf"
    renderPDF.drawToFile(d, str(pdf), author="apbCatalogR authors")
    subprocess.run(["pdftoppm", "-r", "300", "-png", "-singlefile", str(pdf), str(pdf.with_suffix(""))], check=True)
    with Image.open(pdf.with_suffix(".png")) as rendered:
        rendered.convert("RGB").save(OUTPUT, dpi=(300, 300))
print(f"Wrote {OUTPUT}")
