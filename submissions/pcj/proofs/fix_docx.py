import re, sys
D = sys.argv[1]
doc = D + '/word/document.xml'; rel = D + '/word/_rels/document.xml.rels'
x = open(doc, encoding='utf-8').read(); r = open(rel, encoding='utf-8').read()

def rep(old, new, n=1, regex=False):
    global x
    cnt = len(re.findall(old, x)) if regex else x.count(old)
    assert cnt == n, f'{old!r}: expected {n}, found {cnt}'
    x = re.sub(old, new, x) if regex else x.replace(old, new)

# ligature characters baked into the text (break search/copy)
x = x.replace('ﬁ', 'fi').replace('ﬂ', 'fl')

# A2/A3 broken DOIs
rep('https://doi.org/10.3389/feduc.2025.16168 79', 'https://doi.org/10.3389/feduc.2025.1616879')
assert r.count('feduc.2025.16168%2079') == 1
r = r.replace('feduc.2025.16168%2079', 'feduc.2025.1616879')
rep('<w:t>https://doi.org/10.1080/00933104</w:t>', '<w:t>https://doi.org/10.1080/00933104.2014.965860</w:t>')
rep(r'<w:p [^>]*><w:pPr><w:pStyle w:val="PCJReference"/></w:pPr><w:r><w:t>\.2014\.965860</w:t></w:r></w:p>', '', regex=True)
assert r.count('Target="https://doi.org/10.1080/00933104"') == 1
r = r.replace('Target="https://doi.org/10.1080/00933104"', 'Target="https://doi.org/10.1080/00933104.2014.965860"')

# A4 possessive: "Hartmann (2008)|’s| dissertation"
rep(r'developed in Hartmann \(2008\)</w:t></w:r>(<w:r[^>]*>)(<w:rPr>(?:(?!</w:rPr>).)*</w:rPr>)?<w:t[^>]*>’s</w:t></w:r>',
    "developed in Hartmann’s (2008)</w:t></w:r>", regex=True)

# A5-A9 hard hyphens, C1 stray spaces
for a, b in [('substan-tive', 'substantive'), ('mi-nor wording', 'minor wording'),
             ('Recht-sextremer', 'Rechtsextremer'), ('sec-ondary school', 'secondary school'),
             ('Dick-inson', 'Dickinson'), ('construct under-representation', 'construct underrepresentation'),
             ('cognitive- interview', 'cognitive-interview'), ('option- matching', 'option-matching')]:
    rep(a, b)

# A10 ministry name (text + reference)
rep('Ministerstvo školství mládeže', 'Ministerstvo školství, mládeže', 2)
# A11
rep('(e.g. think-aloud', '(e.g., think-aloud')
# A12 comma back inside the closing quote: "procedure”|,| not"
rep(r'sample and procedure”</w:t></w:r>(<w:r[^>]*>)(<w:rPr>(?:(?!</w:rPr>).)*</w:rPr>)?<w:t[^>]*>,</w:t></w:r>',
    'sample and procedure,”</w:t></w:r>', regex=True)
# A13, A14 spacing
rep('>=.20 are statistically', '>= .20 are statistically')
rep('&lt;.05.', '&lt; .05.')
# A15 heading period
rep('<w:t xml:space="preserve">2.2 </w:t>', '<w:t xml:space="preserve">2.2. </w:t>')
# C3 capitalisation
rep('The wiley international', 'The Wiley international')
rep('prospects for canadian social', 'prospects for Canadian social')
# C4 article number
rep('<w:t>, 22(1)</w:t></w:r><w:r w:rsidR="0052140F"><w:t>, 14</w:t>',
    '<w:t>, 22(1)</w:t></w:r><w:r w:rsidR="0052140F"><w:t>, Article 14</w:t>')
# C5 en dash in heading
rep('Substantive Context: Ideology-HPT', 'Substantive Context: Ideology–HPT')

# ---- run-level formatting -------------------------------------------------
RUN = re.compile(r'<w:r(?P<attr>(?: [^>]*)?)>(?P<rpr><w:rPr>(?:(?!</w:rPr>).)*</w:rPr>)?<w:t(?: xml:space="preserve")?>(?P<t>[^<]*)</w:t></w:r>')
def mkrun(attr, rpr, t, add=''):
    if not t: return ''
    inner = (rpr[len('<w:rPr>'):-len('</w:rPr>')] if rpr else '') + add
    rp = f'<w:rPr>{inner}</w:rPr>' if inner else ''
    return f'<w:r{attr}>{rp}<w:t xml:space="preserve">{t}</w:t></w:r>'

def fmt(context, start, length, add, strip_italic=False):
    """In the single run containing `context`, format the slice context[start:start+length]."""
    global x
    hits = [m for m in RUN.finditer(x) if context in m.group('t')]
    assert len(hits) == 1, f'{context!r}: {len(hits)} runs'
    m = hits[0]; t = m.group('t'); i = t.index(context) + start
    rpr = m.group('rpr') or ''
    mid_rpr = rpr.replace('<w:i/>', '').replace('<w:iCs/>', '') if strip_italic else rpr
    new = (mkrun(m.group('attr'), rpr, t[:i]) + mkrun(m.group('attr'), mid_rpr, t[i:i+length], add)
           + mkrun(m.group('attr'), rpr, t[i+length:]))
    x = x[:m.start()] + new + x[m.end():]

IT = '<w:i/><w:iCs/>'
# A16 statistical symbols set upright (italic, as in the submitted PDF)
fmt('(SD = 0.92;', 1, 2, IT)
fmt('significant at p &lt; .001', 15, 1, IT)
fmt(' = −.153, p = .010', 10, 1, IT)
fmt('composite: β = .016, SE = .068', 21, 2, IT)
fmt('.149], p = .817', 7, 1, IT)
fmt('0.21–0.34, p &lt; .001', 11, 1, IT)
fmt('familywise α = .05', 11, 1, IT)
fmt('per-test α = .0056', 9, 1, IT)
fmt('composite: β = .016', 11, 1, IT)
fmt('predictor (β = 0.21', 11, 1, IT)
fmt('estimates (ω = .66', 11, 1, IT)
# A17 Table 4 note label
fmt('Note. ∆CFI and ∆RMSEA', 0, 4, IT)

# A1 chi-square superscript: "χ2" runs -> χ (italic) + ² (superscript, upright)
n = 0
for m in reversed(list(RUN.finditer(x))):
    if m.group('t') == 'χ2':
        rpr = m.group('rpr') or ''; up = rpr.replace('<w:i/>', '').replace('<w:iCs/>', '')
        new = mkrun(m.group('attr'), rpr, 'χ') + mkrun(m.group('attr'), up, '2', '<w:vertAlign w:val="superscript"/>')
        x = x[:m.start()] + new + x[m.end():]; n += 1
assert n == 3, n

# C2 p_raw / p_adj -> subscript
for sub in ('raw', 'adj'):
    fmt('_' + sub + ' = ', 0, 4, '<w:vertAlign w:val="subscript"/>')
rep(r'<w:t xml:space="preserve">_(raw|adj)</w:t>', r'<w:t xml:space="preserve">\1</w:t>', 2, regex=True)

# A18 Table 5: widen the label column so "(rev.)" does not wrap
i = x.find('Zero-Order Pearson'); j = x.find('<w:tbl>', i); k = x.find('</w:tbl>', j)
tbl = x[j:k]
grid = ['2671', '626', '847', '848', '845', '987', '1126', '1070']
cells = ['2689', '567', '850', '851', '850', '992', '1134', '1077']
g_new = [str(int(grid[0]) + 240)] + [str(int(w) - 34) for w in grid[1:]]
c_new = [str(int(cells[0]) + 240)] + [str(int(w) - 34) for w in cells[1:]]
old_grid = ''.join(f'<w:gridCol w:w="{w}"/>' for w in grid)
assert tbl.count(old_grid) == 1
tbl = tbl.replace(old_grid, ''.join(f'<w:gridCol w:w="{w}"/>' for w in g_new))
old_row = ''.join(f'<w:tcW w:w="{w}" w:type="dxa"/>' for w in cells)
def rw(m):
    ws = re.findall(r'<w:tcW w:w="(\d+)"', m.group(0)); assert ws == cells, ws
    s = m.group(0)
    for o, n in zip(cells, c_new):
        s = s.replace(f'<w:tcW w:w="{o}" w:type="dxa"/>', f'<w:tcW w:w="{n}" w:type="dxa"/>', 1)
    return s
tbl, nrows = re.subn(r'<w:tr .*?</w:tr>', rw, tbl); assert nrows == 8
assert tbl.count('<w:tblLook') == 1
tbl = tbl.replace('<w:tblLook', '<w:tblCellMar><w:left w:w="60" w:type="dxa"/><w:right w:w="60" w:type="dxa"/></w:tblCellMar><w:tblLook')
x = x[:j] + tbl + x[k:]

open(doc, 'w', encoding='utf-8').write(x); open(rel, 'w', encoding='utf-8').write(r)
print('ok')
