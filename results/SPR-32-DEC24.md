## Results (December 2024)

These results were collected during the pilot operation of the [SuperMUC-NG Phase 2](https://doku.lrz.de/hardware-of-supermuc-ng-phase-2-222891050.html) system.
The hardware is a dual-socket Intel Xeon Platinum 8480+ (codename "Sapphire Rapids") with 56 cores per socket and 512 GB of DDR5 memory.

Reported average gather times are in msec. (Averaged over 1000 iterations.)

### NAG Fortran 7.2.7203

Mesh/images |  v1  |  v2  |  v3  |  v4  |
------|-------|-------|--------|-------|
B1/32 | 0.128 | 0.942 | 0.139  | 0.758 |
B2/32 | 0.151 | 0.804 | 0.0740 | 0.722 |
B3/32 | 0.109 | 0.894 | 0.0788 | 0.789 |
B4/32 | 0.190 | 0.864 | 0.136  | 0.744 |
B5/32 | 0.407 | 1.01  | 0.261  | 0.870 |

### Intel Fortran 2023.2.0 (ifx) / Intel MPI 2021.11

Mesh/images |  MPI  |   v1  |  v2   |  v3 |  v4 |
------|--------|-------|-------|------|-------|
B1/32 |  |   |   |  |   |
B2/32 |  |   |   |  |   |
B3/32 |  |   |   |  |   |
B4/32 |  |   |   |  |   |
B5/32 |  |   |   |  |   |
