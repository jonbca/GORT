This project requires SWI-Prolog version 5.7.12 or greater. If using version 5.7.12 or
lower, it is recommended to replace the `library/semweb/rdf_turtle.pl` file with the
latest version from the GIT repository for SWI-Prolog. See: http://www.swi-prolog.org/git.html for details.

It has been tested with version 5.7.12 with the altered `rdf_turtle.pl` file.

TO RUN
------
1. Decompress the guessdata.tar.bz2. It will create a directory called guessdata.

NOTE: When decompressed, guessdata.tar.bz2 occupies over 600 MB.

2. Issue the following commands to create a cache folder:

    $ cd guessdata
    $ mkdir .cache

3. Enter the src folder.
4. Run SWI-Prolog:

    $ swipl

Note that the version installed on DICE is too old to run this
software. You must use version 5.7.12.

5. To start the program, run:

    ?- [guess].


