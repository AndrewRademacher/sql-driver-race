# Results

    benchmarking Insert 100/hasql
    time                 604.4 μs   (575.5 μs .. 644.6 μs)
                         0.970 R²   (0.951 R² .. 0.993 R²)
    mean                 586.3 μs   (570.2 μs .. 617.0 μs)
    std dev              72.10 μs   (48.48 μs .. 102.3 μs)
    variance introduced by outliers: 83% (severely inflated)

    benchmarking Insert 100/postgresql-simple
    time                 1.442 ms   (1.406 ms .. 1.487 ms)
                         0.989 R²   (0.974 R² .. 0.997 R²)
    mean                 1.458 ms   (1.420 ms .. 1.501 ms)
    std dev              137.7 μs   (96.16 μs .. 183.4 μs)
    variance introduced by outliers: 68% (severely inflated)

    benchmarking Insert 1000/hasql
    time                 5.225 ms   (4.999 ms .. 5.593 ms)
                         0.978 R²   (0.958 R² .. 0.999 R²)
    mean                 5.157 ms   (5.068 ms .. 5.396 ms)
    std dev              409.3 μs   (174.1 μs .. 769.6 μs)
    variance introduced by outliers: 48% (moderately inflated)

    benchmarking Insert 1000/postgresql-simple
    time                 13.56 ms   (13.16 ms .. 14.15 ms)
                         0.995 R²   (0.987 R² .. 0.999 R²)
    mean                 13.17 ms   (13.03 ms .. 13.43 ms)
    std dev              478.4 μs   (304.0 μs .. 772.5 μs)
    variance introduced by outliers: 11% (moderately inflated)

