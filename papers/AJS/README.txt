************************
This is an information about the ajs style files (version 1.0) that are based on
the jss style file from Achim Zeileis ("A Document Class for Publications in the 
Journal of Statistical Software"), version 2.2.
************************

All style files contained in the zip file guidelinesAJS.zip are published under the GPL-2 licence. 
Minor modifications of the original source (jss) are carried out by Matthias Templ.

This zip-archive contains the pdfLaTeX infrastructure for
publications in the Journal of Statistical Software. The files
  - ajs.cls (LaTeX2e class)
  - ajs.bst (BibTeX style)
need to be included in your search path (local working directory,
texmf or localtexmf tree).

A manual how to use jss.cls is provided in
  - guidelinesAJS.pdf


AJS papers should be prepared using AJS styles that is almost a 1:1 copy of Achim Zeileis JSS style file; the submission of
the final version needs to include the full sources (.tex, .bib, and
all graphics). A quick check for the most important aspects of the
AJS style is given below; authors should make sure that all of them
are addressed in the final version:  
  - The manuscript can be compiled by pdfLaTeX.
  - References are provided in a .bib BibTeX database and included
    in the text by \cite, \citep, \citet, etc.
  - Titles and headers are formatted as described in the guidelines:
      - \title in title style,
      - \section etc. in sentence style,
      - all titles in the BibTeX file in title style.
  - Figures, tables and equations are marked with a \label and
    referred to by \ref, e.g., "Figure~\ref{...}".
  - Software packes are \cite{}d properly.
For more details, see the style FAQ at http://www.jstatsoft.org/style
and the manual guidelinesAJS.pdf.
