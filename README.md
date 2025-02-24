NORGEUX Léa & ESPOSITO Cléa - 4-IR B1

**MEDIUM PROJECT**

Pour ce qui est du projet medium, nous avons réalisé un système permettant d'attribuer un sport à chaque personne, sachant que ces personnes ont, au préalable fourni une liste de sports les intéressant (sans ordre de préférence). Cette sélection prend la forme d'un fichier dans lequel chaque ligne commence par le prenom de la personne suivi des sports (autant que voulu) souhaités (fichier assosports1.txt). Le code est actuellement réalisé de manière à ce que chaque personne ait un sport attribué maximum et chaque sport n'est disponible que pour une seule personne. Néanmoins, cela peut être facilement modifiable.

Base project for Ocaml project on Ford-Fulkerson. This project contains some simple configuration files to facilitate editing Ocaml in VSCode.

To use, you should install the *OCaml* extension in VSCode. Other extensions might work as well but make sure there is only one installed.
Then open VSCode in the root directory of this repository (command line: `code path/to/ocaml-maxflow-project`).

Features :
 - full compilation as VSCode build task (Ctrl+Shift+b)
 - highlights of compilation errors as you type
 - code completion
 - automatic indentation on file save

A makefile provides some useful commands:
 - `make build` to compile. This creates an ftest.native executable
 - `make demo` to run the `ftest` program with some arguments
 - `make format` to indent the entire project
 - `make edit` to open the project in VSCode
 - `make clean` to remove build artifacts

In case of trouble with the VSCode extension (e.g. the project does not build, there are strange mistakes), a common workaround is to (1) close vscode, (2) `make clean`, (3) `make build` and (4) reopen vscode (`make edit`).
