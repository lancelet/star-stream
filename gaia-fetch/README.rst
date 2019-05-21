==========
gaia-fetch
==========

.. image:: https://circleci.com/gh/lancelet/star-stream.svg?style=svg
    :target: https://circleci.com/gh/lancelet/star-stream

Utility to download the Gaia GDR2 star catalog.

Gaia_ is a space observatory launched by the European Space Agency
(ESA) in 2013.  The second data release from Gaia, called GDR2, was
made public on 25 April 2018. It consists of GZipped CSV files, stored
on an ESA server, comprising a total of ~588 GB of compressed data.

This package contains an executable, called ``gaia-fetch``, which
downloads this large archive, checking MD5 hashes and caching
already-downloaded files.

.. _Gaia: https://en.wikipedia.org/wiki/Gaia_(spacecraft)
