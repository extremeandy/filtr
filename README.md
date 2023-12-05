# filtr

Data structures and tools for representing and evaluating predicate trees.

It's a less-sophisticated version of [predicates](https://docs.rs/predicates/latest/predicates) with a focus on a simple recursive structure for representing predicate trees. This structure is designed with serialisability in mind - whether via serde, or otherwise.

Please read the [API documentation here](https://docs.rs/filtr/).

[![build_status](https://github.com/extremeandy/filtr/actions/workflows/ci.yml/badge.svg)](https://github.com/extremeandy/filtr/actions)
[![crates.io](https://img.shields.io/crates/v/filtr.svg)](https://crates.io/crates/filtr)

How to use with Cargo:

```toml
[dependencies]
filtr = "0.1.0"
```

## License

Dual-licensed to be compatible with the Rust project.

Licensed under the Apache License, Version 2.0
https://www.apache.org/licenses/LICENSE-2.0 or the MIT license
https://opensource.org/licenses/MIT, at your
option. This file may not be copied, modified, or distributed
except according to those terms.
