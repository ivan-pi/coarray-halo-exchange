## Results (December 2024)

These results were collected on a single node of the [SuperMUC-NG](https://doku.lrz.de/supermuc-ng-10745965.html) cluster.
The hardware is a dual-socket Intel Xeon Platinum 8174 (codename "Skylake") with 24 cores per socket and 96 GB of memory.

Reported average gather times are in msec.

### NAG Fortran 7.2.7203

Mesh/images   |  MPI |  v1 |  v2 | v3 |  v4 |
------|------|-----|-----|----|-----|
B1/32 |  | | | | |
B2/32 |  | | | | |
B3/32 |  | | | | |
B4/32 |  | | | | |
B5/32 |  | | | | |

### Intel Fortran 2023.2.0 (ifx) / Intel MPI 2021.11

Mesh/images |  MPI  |   v1  |  v2   |  v3 |  v4 |
------|-------|-------|-------|-----|-----|
B1/32 | 0.0258 | 64.7  | 0.404 | 42.5  | 20.8  |
B2/32 | 0.0290 | 165   | 0.440  | 85.5  | 42.0  |
B3/32 | 0.0500 | 866   | 0.563  | 169 | 82.7  |
B4/32 |  |  |  |  |  |
B5/32 |  |  |  |  |  |
