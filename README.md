# Blockchain for LEARNING and FUN

[![MIT licensed][mit-badge]][mit-license-url]
[![Apache 2.0 licensed][apache-badge]][apache-license-url]
[![Contributor Covenant][cc-badge]][cc-url]

[mit-badge]: https://img.shields.io/badge/License-MIT-blue.svg
[mit-url]: https://opensource.org/licenses/MIT
[mit-license-url]: https://github.com/DariuszDepta/cosmerl/blob/main/LICENSE-MIT
[apache-badge]: https://img.shields.io/badge/License-Apache%202.0-blue.svg
[apache-url]: https://www.apache.org/licenses/LICENSE-2.0
[apache-license-url]: https://github.com/DariuszDepta/cosmerl/blob/main/LICENSE
[apache-notice-url]: https://github.com/DariuszDepta/cosmerl/blob/main/NOTICE
[cc-badge]: https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg
[cc-url]: https://github.com/DariuszDepta/cosmerl/blob/main/CODE_OF_CONDUCT.md

## Overview

Blockchain built using [Erlang/OTP](https://www.erlang.org/). This is an **EXPERIMENTAL PROJECT**.
Its goal is to learn the details of a real blockchain built on
[Cosmos-SDK](https://github.com/cosmos/cosmos-sdk) / [CosmWasm](https://github.com/CosmWasm)
and to master [Erlang/OTP](https://www.erlang.org/). 

> **This is not a production blockchain.
> To build a REAL blockchain use [Cosmos-SDK](https://github.com/cosmos/cosmos-sdk)
> and [CosmWasm](https://github.com/CosmWasm).** 

## Prerequisities

1. [Install Erlang/OTP](https://www.erlang.org/downloads)
2. [Install rebar3](https://www.rebar3.org/docs/getting-started)
3. [Install Task](https://taskfile.dev/installation)
4. [Clone **cosmerl** repository](https://github.com/DariuszDepta/cosmerl)

## Getting started

Open the terminal and head to the directory with cloned
[**cosmerl**](https://github.com/DariuszDepta/cosmerl) repository.

#### Build the blockchain

```shell
task build
```

#### Test the blockchain

```shell
task test
```

#### Run the blockchain in Erlang shell

```shell
task run
```

#### Clean all artefacts generated during compilation or tests

```shell
task clean
```

## References

- [REFERENCES.md](./REFERENCES.md)

## License

Licensed under either of

- [MIT license][mit-url] (see [LICENSE-MIT][mit-license-url]) or
- [Apache License, Version 2.0][apache-url] (see [LICENSE][apache-license-url] and [NOTICE][apache-notice-url])

at your option.

## Contributions

Any contributions to [**cosmerl**](https://github.com/DariuszDepta/cosmerl) are greatly appreciated.
All contributions intentionally submitted for inclusion in the work by you,
shall be dual licensed as above, without any additional terms or conditions.
