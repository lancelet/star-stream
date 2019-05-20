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
