# Benchmarking purescript parsers with big strings

These benchmarks simply pass the following "large strings" (all A's) to each parser:

```
→ Testing string of size: 1024
→ Testing string of size: 2048
→ Testing string of size: 3072
→ Testing string of size: 4096
→ Testing string of size: 5120
→ Testing string of size: 6144
→ Testing string of size: 7168
→ Testing string of size: 8192
→ Testing string of size: 9216
→ Testing string of size: 10240
```

"bench" is then used to collect various statistics that can be found below.

## Repositories

1. https://github.com/Thimoteus/purescript-simple-parser
2. https://github.com/purescript-contrib/purescript-string-parsers
3. https://github.com/purescript-contrib/purescript-parsing

## Note

The following libraries are not included in the benchmarks:

1. purescript-parsing (unmodified): Crashes due to stack limit exceptions
2. purescript-parsing (with Trampoline): Finishes, but, takes ~45+ seconds to complete. Even with a 1 second time limit supplied ti bench, it takes forever.

## Results

![Results](/assets/benchmarks.png?raw=true "Results")

## Result HTML

[Results](assets/benchmarks.html)
