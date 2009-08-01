$Id$

This project requires SWI-Prolog version 5.7.12 or greater. If using version 5.7.12 or
lower, it is recommended to replace the library/semweb/rdf_turtle.pl file with the
latest version from the GIT repository for SWI-Prolog. See:

http://www.swi-prolog.org/git.html

for details.

It has been tested with version 5.7.12 with the altered rdf_turtle.pl file.

TO RUN
~~~~~~

1. Locate the guessdata directory. Inside that directory, create a directory
   called '.cache'.
   
2. Enter the src/ directory, and start SWI-Prolog:
   $ swipl

3. To start the program, type:
   ?- [guess].
