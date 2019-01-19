# workload-scheduling-simulator
Multipurpose workload scheduling simulator.

## Building
```
stack setup
stack build
```

## Testing
```
stack setup
stack test
```

## Benchmarking
```
stack setup
stack bench
```
Note that benchmarking results may be affected by CPU scaling. To control that you can use following commands:
```
sudo cpupower frequency-set --governor performance # disable
sudo cpupower frequency-set --governor powersave   # enable
```
