#! /bin/sh

echo "You should have XeLaTeX (from TeXLive), Linux Libertine TTF font and xpdf. Your TeXlive installation should wrap TeXLive and LModern and pita fonts. In this case the file should compile, and you should see some font features like 'Qu' ligature. Surely all the languages should display.. Additional test for X11 keyboard: all the text was typed in, no symbol required copying." | fmt -w 72
echo 

cd /tmp
mkdir xelatex-test
cd xelatex-test

cat >test.xtx <<EOF

\documentclass[a4paper]{article}
\usepackage{fontspec}

\begin{document}

\fontspec[RawFeature=+hlig;+dlig;+onum;+zero]{Linux Libertine O}

You should have XeLaTeX (from TeXLive), Linux Libertine TTF font and xpdf. Your TeXlive installation should wrap TeXLive and LModern and pita fonts. In this case the file should compile, and you should see some font features like 'Qu' ligature. Surely all the languages should display.. Additional test for X11 keyboard: all the text was typed in, no symbol required copying.

English script: This is in English.

French: Voilà! Cette proposition est en français.

Russian: Немного текста по-русски.

Greek: Ελληνικά γλώσσα.

Esperanto: Ĉi estas unu frazo skribita en esperanto. \\
Note: ŭ as in ankaŭ is using breve, not caron (as in ǔ)

Now ligatures: Qu'est-ce que c'est? The effect of high letter with the foremost part somewhere up. QQ

Numerals: 0123456789

Default glyphs: β θ h

\addfontfeatures{RawFeature=+salt}

Alternative glyphs: β θ h 

\fontspec[RawFeature=+hlig;+dlig;+zero]{Linux Libertine O}

A weak password: 1lO0o 012 1302

\end{document}

EOF

xelatex test.xtx
xpdf test.pdf

cd 

rm -r /tmp/xelatex-test
