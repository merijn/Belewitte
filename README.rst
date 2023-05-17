==============
Belewitte [1]_
==============

.. image:: https://zenodo.org/badge/88159659.svg
   :target: https://zenodo.org/badge/latestdoi/88159659
.. image:: https://zenodo.org/badge/DOI/10.5281/zenodo.6925023.svg
   :target: https://doi.org/10.5281/zenodo.6925023

This repository is made up of 5 (mostly) independent components:

`Graph Tools`_
    A set of executables for the inspection/manipulation of the custom graph
    format used.

`Kernel Runner`_
    Executable for finding, loading, and executing GPU kernel libraries on
    input graphs. [PELGA2015]_ [ARXIV2017]_ [IAAA2018]_

`Benchmark Analysis Tools`_
    Tool for managing and manipulating experimental setups, tracking
    experimental results, performing result analysis, and generating models.
    [ARXIV2017]_ [IAAA2018]_

`Evolutionary Graph Generation`_
    Tool for generating graphs using evolutionary computing. [PELGA2016]_

`Graph Plotting`_
    Tool for plotting the connectivity matrix of a graph.

Prerequisites
=============

The ``Makefile`` is set up to gracefully build as much of the codebase as
possible, depending on the found/available prerequisites. The component
sections below document the prerequisites of each component. The full list of
prerequisites is:

GNU make
    or compatible make implementation

A C++17 supporting compiler
    e.g., g++ >=7.x or clang++ >=5.x

CUDA 10
    The include files and libraries are expected to be under the path specified
    by the ``CUDA_PATH`` environment variable.

OpenCL
    The OpenCL libraries are expected to be in the directory specified by the
    ``OPENCL_LIB`` environment variable.

SLURM Workload Manager
    https://slurm.schedmd.com/documentation.html

    Optional, can be replaced with custom run commands.

Python 3.6 or 3.7
    The python 3.6/3.7 executable is expected to be in the user's ``PATH``

GHC Haskell compiler 8.10.7, 9.2.7, 9.4.5, or 9.6.1
    https://www.haskell.org/ghc/download.html

cabal-install 3.6+
    https://www.haskell.org/cabal/download.html

Intel Threading Building Blocks

The ``Config.mk`` file has several variables that can be used to explicitly
specify the path to the relevant compilers/compiler versions if they are not on
the users path.

Instructions for installing the above on the DAS 5 can be found `below <DAS 5
Instructions_>`_.

Graph Tools
===========

The formats used by [SNAP]_ and [KONECT]_ to store graph datasets is rather
inefficient to load from disk and convert to an in memory representation. To
avoid this we use a custom file format that's faster to load and convert, these
tools are useful for converting, inspecting, and modifying graphs stored in
this format.

``normalise-graph``
    Normalises graphs stored in SNAP and KONECT's file formats to our file
    format and from our format to SNAP's edge lists and the MatrixMarket file
    format.

    Also provides commands to convert vertex ids from the original graph to the
    id in the new format and vice versa.

``print-graph``
    Reports vertex and edge counts of graphs and prints all the incoming and
    outgoing edges for each vertex.

``check-degree``
    Computes the degree of each vertex and prints a report listing the number
    of vertices that have a given degree.

``graph-details``
    Computes and reports various graph statistics, such as the min/lower
    quantile/median/mean/upper quantile/max/standard deviation of the input
    graph(s).

``reorder-graph``
    Reorders the vertices in a graph based on their degree to get best and
    worst case grouping of vertices per warp. Used to investigate how the in
    memory ordering of vertices impacts performance.

Graph Tools Prerequisites
-------------------------

* gmake
* C++ compiler

Kernel Runner
=============

Command line tool that can load the various GPU graph algorithm
implementations for benchmarking. Takes care of finding and loading libraries
containing the GPU kernels, loading graphs into the correct in memory
representation, generating timings, and logging graph metadata.

Can also be used to read experiments from stdin to do batch runs of
experiments. Mostly intended to be used by the `Benchmark Analysis Tools`_ to
drive experiments.

Kernel Runner Prerequisites
---------------------------

* gmake
* C++17 compiler
* CUDA 10
* OpenCL

Benchmark Analysis Tools
========================

Provides 3 tools for managing GPU experiments:

``Ingest``
    Used to register/define experimental setups to execute. Including:

    * Registering platforms
    * Registering algorithms
    * Registering implementations
    * Registering input graphs/datasets
    * Importing results from external tools/experiments

    Also takes care of running the specified experiments, this requires that
    the `Kernel Runner`_ has been compiled successfully and that SLURM's
    ``srun`` is on the user's path.

    Optionally, the use of SLURM can be replaced with other tools. To do this,
    a `run-command` should be configured using `Ingest`. Runs are invoked as
    follows::

        <run-command> <platform> -- kernel-runner <runner args...>

    Here, the `run-command` is either a built-in `srun` invocation, or the
    command configured by the user using `Ingest`. The `platform` is either the
    flags specified for the platform (if any) and otherwise the name specified
    during the registration with `Ingest`.

``Model``
    Used to train and evaluate models using stored experimental results.
    Including:

    * Training new models
    * Querying metadata, parameter importance and mispredictions
    * Validating model accuracy against training and validation datasets
    * Evaluating model performance against the entire dataset
    * Comparing performance results of different implementations
    * Exporting models to runnable C++ code

    Requires python 2.7 and virtualenv for training new models.

``Plot``
    Used to generate plots of various experiments. Including:

    * Plotting implementation performance for all levels of a graph
    * Plotting implementation performance for multiple graphs
    * Plotting implementation performance compared to optimal/external
      runtimes

    Requires python 2.7 and virtualenv for all plot commands.

Benchmark Analysis Tools Prerequisites
--------------------------------------

* gmake
* GHC 8.10.7
* cabal-install 3.6

Optional prerequisities:

* SLURM
* python 3.6/3.7

Evolutionary Graph Generation
=============================

A tool that generates graph using evolutionary computing. Consists of host
program that compares graph fitness compared to evaluation criteria and
distributes new generation tasks to workers running on compute nodes in the
cluster.

Not recently maintained/used, so using/running it may take some work.

Evolutionary Graph Generation Prerequisites
-------------------------------------------

* gmake
* GHC 8.10.7
* cabal-install 3.6
* Intel TBB
* SLURM

Graph Plotting
==============

A tool that plots the connectivity matrix of a graph by plotting a dot at
coordinate `(x, y)` iff there is an edge from vertex `x` to vertex `y`.

Graph Plotting Prerequisites
----------------------------

* GHC 8.10.7
* cabal-install 3.6

DAS 5 Instructions
==================

SLURM, OpenCL, and CUDA 10 can all be loaded via modulefiles, using:

.. code:: bash

    module load cuda10.0/toolkit/10.0.130
    module load opencl-nvidia/10.0
    module load slurm
    module load python/3.6.0

The remaining bits can be installed from binary distributions, the install
location isn't very relevant, as long as they're on your ``PATH`` or the
variables in ``Config.mk`` are edited to point to the proper install location.

For simplicity's sake the commands below assume the environment variable
``INSTALL_PATH`` has been set to the prefix where these tools should be
installed, although they can just as easily be installed into different
locations.

clang++ 8
---------

.. code:: bash

    wget https://releases.llvm.org/8.0.0/clang+llvm-8.0.0-x86_64-linux-sles11.3.tar.xz
    tar xvf clang+llvm-8.0.0-x86_64-linux-sles11.3.tar.xz -C $INSTALL_PATH --strip-components=1
    rm clang+llvm-8.0.0-x86_64-linux-sles11.3.tar.xz

GHC 8.10.7
----------

.. code:: bash


    wget https://downloads.haskell.org/~ghc/8.10.7/ghc-8.10.7-x86_64-centos7-linux.tar.xz
    tar xvf ghc-8.10.7-x86_64-centos7-linux.tar.xz
    rm ghc-8.10.7-x86_64-centos7-linux.tar.xz
    cd ghc-8.10.7
    ./configure --prefix=$INSTALL_PATH
    make install
    hash -r
    cd ..
    rm -r ghc-8.10.7

cabal-install 3.6
-----------------

.. code:: bash

    wget https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal/3.6.2.0/cabal-install-3.6.2.0-x86_64-linux-alpine-static.tar.xz
    tar xvf cabal-install-3.6.2.0-x86_64-linux-alpine-static.tar.xz
    mkdir -p $INSTALL_PATH/bin/
    mv cabal $INSTALL_PATH/bin/
    rm cabal.sig cabal-install-3.6.2.0-x86_64-linux-alpine-static.tar.xz

-------------------------------------------------------------------------------

.. [1] Mythological being associated with precognition/prediction and graves [2]_

.. [2] This is funny if you know Dutch...

.. [SNAP] http://snap.stanford.edu/data/index.html

.. [KONECT] http://konect.uni-koblenz.de/networks/

.. _indirect: `DAS 5 Instructions`_

.. [PELGA2015]
    :Title: “Quantifying the Performance Impact of Graph Structure on Neighbour Iteration Strategies for PageRank”
    :Authors: Merijn Verstraaten, Ana Lucia Varbanescu, and Cees de Laat
    :Workshop: 1:superscript:`st` Workshop on Performance Engineering for Large Scale Graph Analytics
    :Proceedings: European Conference on Parallel Processing
    :Pages: 528–540
    :Year: 2015
    :Publisher: Springer, Cham

.. [PELGA2016]
    :Title: “Synthetic Graph Generation for Systematic Exploration of Graph Structural Properties”
    :Authors: Merijn Verstraaten, Ana Lucia Varbanescu, and Cees de Laat
    :Workshop: 2:superscript:`nd` Workshop on Performance Engineering for Large Scale Graph Analytics
    :Proceedings: European Conference on Parallel Processing
    :Pages: 557–570
    :Year: 2016
    :Publisher: Springer, Cham

.. [ARXIV2017]
    :Title: “Using Graph Properties to Speed-up GPU-based Graph Traversal: A Model-driven Approach”
    :Authors: Merijn Verstraaten, Ana Lucia Varbanescu, and Cees de Laat
    :Year: 2017
    :eprint: arXiv:1708.01159
    :URL: https://arxiv.org/abs/1708.01159

.. [IAAA2018]
    :Title: “Mix-and-Match: A Model-driven Runtime Optimisation Strategy for BFS on GPUs”
    :Authors: Merijn Verstraaten, Ana Lucia Varbanescu, and Cees de Laat
    :Workshop: 8:superscript:`th` Workshop on Irregular Applications: Architectures and Algorithms
    :Proceedings: 2018 IEEE/ACM 8th Workshop on Irregular Applications: Architectures and Algorithms
    :Pages: 53-60
    :Year: 2018
    :Publisher: IEEE
