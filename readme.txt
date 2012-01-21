2012.01.21 Noah Silva
shiruba@galapagossoftware.com

This is the preliminary alpha release of the source code for PasViz.
Some of the code (Namely, the PasParse unit and dependencies) are based on
code from a previous project by another author, called USGParse.

In an attempt to modernize some of the code, convert it to FreePascal, and get
it running on MacOS X, many things have been changed, and some bugs have been
introduced (or made more apparent).

The largest problem at present is that the PasParse or TestUtilsLite unit has
"Issues" (Runtime Error 210 or Access Violation) when using DropParsers.
(PopParser could be at fault as well).  I suspect this change resulted from
changing the old custom memory management routines to the standard 
TObject.Free style.

In order to enable the code to run for now with fewer crashes, the calls to
DropParsers have been commented out, which will leak memory.  (This is mainly
an academic issue, since the program will release the memory anyway when it
terminates).  Unfortunately, there is still an issue where some complex
projects can still cause it to crash.

The original license of PasParse and related files is Public Domain, and we have received explicit permission from the author (semi@seznam.cz) to modify and redistribute the code as we wee fit.

The original legacy code (and some updated version of the PAscal parser) may be found at: http://semi.gurroa.cz/AltUtil/Source


I am releasing my code and modifications as GPLv2 as of now.

Please visit the project's web site at http://www.galapagosssoftware.com/developer-tools/pasviz for further information and pre-compiled binaries.
