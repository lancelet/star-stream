===========
star-stream
===========

.. image:: https://circleci.com/gh/lancelet/star-stream.svg?style=svg
    :target: https://circleci.com/gh/lancelet/star-stream

A starfield renderer.

.. code-block:: bash

  git clone --recurse-submodules git@github.com:lancelet/star-stream.git
  cd star-stream
  cabal new-build
  ghcid --command='cabal new-repl'

What do we have so far? The image below shows an equirectangular
projection of all the stars below (brighter than) magnitude 6. It
currently takes ages to render because we iterate the list of all
stars for each pixel! It also lacks color information. However, this
is our first-light rendering, and it will improve from here.

.. image:: first-light.png

Downloading the Gaia GDR2 Star Catalog
--------------------------------------

In the future, this project will use the Gaia star catalog as a data
source. Gaia_ is a space observatory launched by the European Space
Agency (ESA) in 2013.

The second data release from Gaia, called GDR2, was made public on 25
April 2018. It consists of GZipped CSV files, stored on an ESA server,
comprising a total of ~588 GB of compressed data.

The gaia-fetch_ package contains a utility to download these files.

.. _Gaia: https://en.wikipedia.org/wiki/Gaia_(spacecraft)
.. _gaia-fetch: gaia-fetch
