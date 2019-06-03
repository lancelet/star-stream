===========
star-stream
===========

.. image:: https://circleci.com/gh/lancelet/star-stream.svg?style=svg
    :target: https://circleci.com/gh/lancelet/star-stream

A starfield renderer for the Gaia_ DR2 catalog.

.. code-block:: bash

  git clone --recurse-submodules git@github.com:lancelet/star-stream.git
  cd star-stream
  cabal new-build
  ghcid --command='cabal new-repl'

Results from the Gaia DR2 Star Catalog
--------------------------------------

The image below shows an equirectangular, equatorial-coordinate
projection of all 1.69 billon sources in the Gaia_ DR2 catalog. This
"integrated flux" image was created by reading each of the Gaia source
catalog files (compressed CSVs) and adding the contribution of flux
from each source to each pixel it influences. The pixel influences are
calculated by multiplying the measured flux in each of the three Gaia
channels by a convolution filter (Mitchell-Netravali) centred on each
pixel.

The amount of data to be processed (~588 GB compressed) is too big to
fit in memory on the machines running this job (typically a MacBook
Pro or an iMac), so it is streamed in smaller batches, currently
corresponding to the original source CSV files. The project name
(`star-stream`) comes from this necessity to stream the data and
accumulate the image.

The output from the flux integration process is a floating-point HDR
image, which is tweaked (manually, for now) with a non-linear exposure
in an image editor.

.. image:: gaia-equirectangular-equatorial.jpg
   :scale: 15 %

This is only an initial exploration. Improvements planned for
the future include:

- Add a galactic coordinate Mollweide projection
- Adjust filtering ellipse according to the local anisotropy
  of the projection
- Bucket the source observations into an acceleration structure
  for faster rendering of subsets
- Render temporal offsets for sources with proper motion
- Correlation of sources with a catalog that contains popular
  names of stars
- Etc.

Downloading the Gaia GDR2 Star Catalog
--------------------------------------

This project uses the Gaia star catalog as a data source. Gaia_ is a
space observatory launched by the European Space Agency (ESA) in 2013.

The second data release from Gaia, called GDR2, was made public on 25
April 2018. It consists of GZipped CSV files, stored on an ESA server,
comprising a total of ~588 GB of compressed data.

The gaia-fetch_ package contains a utility to download these
files. Please note that the download may be a long process. (It took 3
weeks in Sydney, Australia!)

.. _Gaia: https://en.wikipedia.org/wiki/Gaia_(spacecraft)
.. _gaia-fetch: gaia-fetch
