@echo off
pasvizcli.exe %1
"d:\Program Files\Graphviz 2.28\bin\dot.exe" -Tpng output.gv -o output.png
mspaint output.png



