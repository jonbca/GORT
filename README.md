Guesstimation with Ontologies and Reasoning Techniques
======================================================

This project requires SWI-Prolog version 5.7.12 or greater. If using version 5.7.12 or
lower, it is recommended to replace the `library/semweb/rdf_turtle.pl` file with the
latest version from the [GIT repository for SWI-Prolog](http://www.swi-prolog.org/git.html) for details.

It has been tested with version 5.7.12 with the altered `rdf_turtle.pl` file.

TO RUN
------
Decompress the guessdata.tar.bz2. It will create a directory called guessdata. NOTE: When decompressed, guessdata.tar.bz2 occupies over 600 MB. _Also note, this file is not on Github. I'm adding a shell script to download the necessary ontology data._

Issue the following commands to create a cache folder:

    $ cd guessdata
    $ mkdir .cache

Enter the src folder, and run SWI-Prolog:

    $ swipl

Then, to start the program, run:

    ?- [guess].
