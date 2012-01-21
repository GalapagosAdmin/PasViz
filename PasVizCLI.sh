#!/bin/bash
./pasvizcli $1
mv output.gv $1.gv
Open /Applications/Graphviz.app $1.gv
