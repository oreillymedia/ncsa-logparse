===============
NCSA Logparser
===============

This implementation of an NCSA Common and NCSA Combined log file parser is intended to accompany the Safari Books Online blog series *High-performance Log Parsing in Haskell* (`part 1 <https://www.safaribooksonline.com/blog/2015/03/30/high-performance-log-parsing-in-haskell-part-one/>`_ and `part 2 <http:/www.example.com>`_).

This code is provided under a `BSD3 open source license <https://en.wikipedia.org/wiki/BSD_licenses#3-clause_license_.28.22Revised_BSD_License.22.2C_.22New_BSD_License.22.2C_or_.22Modified_BSD_License.22.29>`_ and forking and modifying of this library to suit your purposes is encouraged.

Getting Started
---------------

There are a couple of pre-installation steps that vary depending on your platform.

OS X
^^^^^

#. `Install the OS X <http://new-www.haskell.org/downloads/osx>`_ GHC and Cabal
#. Install the PCRE library: ``?> brew install pcre``

Ubuntu
^^^^^^
#. `Install the linux <http://new-www.haskell.org/downloads/linux>`_ GHC and Cabal
#. Install the PCRE library: ``?> sudo apt-get install libpcre3-dev``

Sandbox Setup
^^^^^^^^^^^^^

Once these platform-specific steps are done, you can use `cabal <https://www.haskell.org/cabal/>`_, Haskell's package management and build system to do the rest. Run cabal commands from the root directory of the project after you clone it from GitHub. Begin by installing the project's dependencies and setting up a `cabal sandbox <http://coldwa.st/e/blog/2013-08-20-Cabal-sandbox.html>`_::

	?> cd /path/to/repo/checkout/ncsa-logparse
	?> cabal sandbox init  # Initialize a cabal sandbox
	?> cabal update  #  This will download the most recent list of packages.
	?> cabal install --only-dependencies  # Install the project's dependencies

The install step will take a some time as all dependencies are bing downloaded and compiled. When that step is complete, you can build the project::

	?> cabal build

This project builds an executable called ``ncsa-logparse`` that will be located in the ``dist/build/`` directory which the build step creates.

Testing
^^^^^^^

The project includes a test suite. The command ```cabal test`` will run the suite and output the results.

