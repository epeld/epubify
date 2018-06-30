# Epubify All The Things!
This project converst PDFs to EPUB (not there yet!).

It does it by first converting from PDF to XML (using `mutool draw -Fstext`), then transforming the XML using various rules (in SWI-Prolog!).

It will be great!

(Nothing works yet though..)

## Basic Structure
*stext.pl* is the makeshift top-level interface. The predicate stext/2 can be invoked with a PDF filename and a page number.