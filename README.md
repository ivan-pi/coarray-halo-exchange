# Parallel Halo Exchange Using Fortran Coarrays

This repository contains code and test cases for investigating Fortran
coarray implementations of a parallel halo exchange operation associated
with domain decomposition methods for PDE. Here the domain is partitioned
into *P* subdomains, one for each of *P* processes (or images), and each
process calculates the unknown variables defined on its subdomain.
Processes need to exchange values along their subdomain boundary with
their neighbors in a *halo exchange* operation.

## Background
The particular form of the halo exchange considered here originates from the
MPI-parallel code [Truchas](https://gitlab.com/truchas/truchas). Abstractly
one starts with a (global) index set $\{1, 2, ..., N\}$. Given $P$ processes,
an index is assigned to a unique process according to a block partition of
the index set: the first $n_1$ indices are assigned to process 1, the next
$n_2$ to process 2, etc. An index so assigned to a process is said to be
*owned* by that process. In addition to these, a process may include indices
owned by other processes. The owned indices are said to be *on-process* and
these additional indices, if any, are said to be *off-process*. For the
purposes of computation, the collection of all indices known to a process
are mapped to a process-local index set: the block of on-process indices are
mapped, in order, to consecutive local indices starting at 1, and the
off-process indices are mapped to consecutive local indices immediately
following these.

Now the elements of an array indexed by this index set will be distributed
across the $P$ processes according to the distribution of the indices.
Each global index is owned by exactly one process (where it is on-process)
but may exist on other processes as an off-process index. The mapping from
off-process indices to their corresponding on-process index is a many-to-one
mapping. A fundamental operation on the distributed array is to replace the
value at each off-process index with the value at its corresponding
on-process index; this is referred to as a *gather* (of off-process data)
operation.

## Coarray Implementations
In the gather (or halo exchange) operation each image sends some elements
of its on-process data to other images, where it is received to overwrite
off-process data, and likewise receives on-process data from other images
to overwrite its own off-process data. There are 4 different coarray
implementations of this operation. They all perform exactly the same data
exchange, differing only in how it is organized.

* **Method 1:** Scattered read from remote image
Each image reads indirectly-indexed on-process data from remote images
to fill its (contiguous) array of off-process data
* **Method 2:** Blocked-read from remote image
Each image pre-gathers its on-process data destined for other images into
contiguous blocks of a send buffer, and then each image reads its buffer
blocks from remote images to fill corresponding blocks of its off-process
data array.
* **Method 3:** Gathered write to remote image
Each image writes its indirectly indexed on-process data to (contiguous)
blocks of remote images off-process data.
* **Method 4:** Blocked-write to remote image
Each image pre-gathers its on-process data destined for other images into
contiguous blocks of a send buffer, and then writes those blocks to
corresponding blocks of remote image off-process data arrays.

Method 1 is the most straightforward and simple implementation. The other
methods were intended to explore whether there is a performance difference
between reading from or writing to remote images, and whether performance
gains could be made by structuring the transfers to/from remote images in
contiguous blocks of data.

The distributed index set mapping and the associated gather operation are
implemented by the module `index_map_type` whose source is found in the
`coarray/method*` directories for the different methods. The gather
operation is performed by the module procedure `gather_aux`, and the
configuration of the communication pattern used by that procedure is
generated by the module procedure `add_offp_index`.

Note that a fuller-featured, production `index_map_type` module with both
MPI and coarray implementations can be found at
https://github.com/nncarlson/index-map.

Your comments are very much welcome; use the discussions tab to provide
feedback.

## Reference MPI Implementation

An MPI implementation of the halo exchange is found in the `mpi` directory.
This serves as a baseline against which to assess the different coarray
implementations. It uses a graph communicator and MPI-3 neighborhood
collective to perform the halo exchange.

## The Tests
The file `main.f90` is a test driver. It reads data that
describes the partitioning of the index set and then performs the gather
operation on an integer array. The on-process elements of the array are
initialized with their corresponding global IDs and the off-process elements
with invalid data (-1). After the gather operation the off-process elements
should be filled with their global IDs, and this is checked. To get more
accurate timings the gather operation may be repeated multiple times, which
is especially important for the smaller datasets.

The test data is stored in subdirectories of the `test-data` directory,
one for each test. A subdirectory contains a collection of input files,
one per image. Each file (unformatted stream) consists of 2 records. The
first consists of the block size assigned to the image (i.e., the number
of on-process indices) and the number of off-process indices. The second
record is the global IDs of the off-process indices in strictly increasing
order.

The current test data was generated by a version of Truchas hacked to output
this internal data. It comes from an unstructured finite element type mesh
partitioned using METIS and corresponds to the cell index set (there are
also the node, face, and edge index sets that could be obtained). There is
data from a series of meshes ("opencalc-B") of increasing size:

  | Mesh | B0  | B1   | B2   | B3   | B4   | B5
  | ---- | --  | --   | --   | --   | --   | --
  | Cells| 70K | 206K | 562K | 1.6M | 4.4M | 13.4M

Each mesh is partitioned into various numbers of partitions. The mesh and
number of partitions is reflected in the name of the test subdirectory.

### Compiling

The project uses CMake (version 3.22 or later) to compile the tests. You may
need to set your `FC` environment variable to the name or path of your Fortran
compiler before running `cmake` to ensure that CMake finds the correct compiler.
The CMake setup understands how to compile coarray code when using one of the
following Fortran compilers:

* NAG 7.1 or later with its built-in coarray support on shared-memory systems.

* Intel OneAPI with its built-in coarray support. Both the classic ifort and
  and new LLVM-based ifx compilers are supported. The companion Intel MPI
  package must be installed and Intel's setup script run to configure your
  environment. The Intel coarray implementation uses MPI under the hood.

* GFortran with [OpenCoarrays](https://github.com/sourceryinstitute/opencoarrays).
  OpenCoarrays supplies the implementation of coarrays used by the gfortran
  compiler. Be sure the `bin` directory of the opencoarrays installation is in
  your path so that the compiler wrapper `caf` and runner `cafrun` can be found.
  Set `FC=caf` before running `cmake`. OpenCoarrays uses MPI under the hood, and
  at the time of this writing is compatible with MPICH version 4.0, but not 4.1
  or later. Refer to the OpenCoarray website linked above for requirements.

To clone the repository and compile the tests:

```shell
$ git clone https://github.com/nncarlson/coarray-halo-exchange.git
$ cd coarray-halo-exchange
$ mkdir build
$ cd build
$ cmake .. # cmake options go here
$ make
```

Optimized tests will be built by default using CMake's default flags for the
"Release" build type and your specific compiler. Compiler flags can be set
explicitly on the `cmake` command line by defining the `CMAKE_Fortran_FLAGS`
variable; e.g. `-D CMAKE_Fortran_FLAGS="-O3"`

To build the MPI version of the test use this `cmake` command line instead:

```shell
$ cmake .. -D BUILD_MPI_TEST=YES
```

The test executables will be found in `build/coarray` (or `build/mpi`).


### Running

The test executables take 1 or 2 command line arguments. The first is the
path to the directory containing the data files for the test. The second
is the number of times to repeat the gather operation before collecting
timing data. If not specified it defaults to just 1. Only the gather
operation itself is timed, and the average time per gather call is reported.

Here's an example of how to run the coarray1 test from the `build/coarray`
directory using the small "B0" dataset and 4 coarray images, averaging the
time for the gather operation over 1000 iterations:

* Intel OneAPI:
  ```shell
  $ FOR_COARRAY_NUM_IMAGES=4 ./test-coarray1 ../../test-data/opencalc-B0-4 1000
  ```

* GFortran/OpenCoarrays:
  ```shell
  $ cafrun -n 4 ./test-coarray1 ../../test-data/opencalc-B0-4 1000
  ```

* NAG:
  ```shell
  $ NAGFORTRAN_NUM_IMAGES=4 ./test-coarray1 ../../test-data/opencalc-B0-4 1000
  ```

## Results
* [Linux / 32-core Threadripper 7970X / 32 images](results/7970X-32-JUN24.md)
* [Linux / 12-core Threadripper 2920X / 12 images](results/2920X-12-FEB22.md)
* [Variations of version1](results/thelio-12-v1-variations.md); NAG beats MPI
