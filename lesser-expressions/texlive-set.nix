pkgs:

pkgs.texlive.combine {
  inherit(pkgs.texlive)
    scheme-medium collection-langcyrillic 
    cm-super cm-unicode xcolor pgf beamer moderncv tipa disser urlbst
    moderntimeline texinfo metafont multibib collection-fontutils
    epstopdf epspdfconversion epspdf algorithm2e relsize type1cm logreq
    ucs todonotes xargs forloop pbox varwidth bigfoot environ trimspaces
    forest arydshln pgfopts elocalloc libertine mweights fontaxes
    totpages comment soul multirow threeparttable lastpage preprint
    mnsymbol paralist animate media9 ocgx2 breakcites biblatex bibtex
    xstring wrapfig ifoddpage inlinedef draftwatermark everypage
    enumitem cutwin moreverb mparhack bbding thmtools csquotes stmaryrd
    cleveref cancel ncctools changebar tocbibind shadow datetime fmtcount
    bussproofs l3kernel cclicenses xifthen ifmtarg blindtext changepage
    lstaddons import fancyvrb tocloft tikz-qtree ellipsis nag tkz-euclide
    tkz-base numprint /*tkz-graph*/ a4wide inconsolata newtx upquote acmart
    minted fvextra framed titling units siunitx colortbl xpatch prooftrees
    lkproof acronym listings listingsutf8 biber pgfplots csvsimple fourier
    appendix scalerel showlabels subfigure acro zref translations fixme
    doublestroke hyperxmp lipsum standalone changes truncate fifo-stack
    tabto-ltx tikzmark todo adjustbox collectbox spverbatim eqparbox
    sttools biblatex-ieee elsarticle orcidlink mathabx
    ;
}
