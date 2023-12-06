Phylogenetic Haskell Analytic Network Engine (PHANE)
====================================================

# Changelog

PHANE uses [Semantic Versioning (v2.0.0)][SemVer-URI].
The changelog is available [on GitHub][GitHub-Changelog].


## Unreleased (`v0.2.0`)

### [`PHANE-alphabet`][GitHub-Lib-00]

  * Applied [`fourmolu`][Hackage-fourmolu] styling specified by `config/fourmolu.yaml`

  * Moved from PhyG repository


### [`PHANE-dynamic-character`][GitHub-Lib-04]

  * Added initial library version

  * Applied [`fourmolu`][Hackage-fourmolu] styling specified by `config/fourmolu.yaml`


### [`PHANE-dynamic-character-alignment`][GitHub-Lib-07]

  * Added initial library version

  * Applied [`fourmolu`][Hackage-fourmolu] styling specified by `config/fourmolu.yaml`


### [`PHANE-dynamic-character-element`][GitHub-Lib-01]

  * Added initial library version

  * Enhanced `Show` instance of `SlimState` and `WideState` for better readability

  * Applied [`fourmolu`][Hackage-fourmolu] styling specified by `config/fourmolu.yaml`


### [`PHANE-evaluation`][GitHub-Lib-10]

  * Added initial library version

  * Added initial test-suite

  * Added `alterEnvironment`

  * Applied [`fourmolu`][Hackage-fourmolu] styling specified by `config/fourmolu.yaml`

  * Corrected defect where calls to `failWithPhase` might not begin on a new line.

  * Enhanced `Evaluation` Monad `Logger` instance with context sensitive rendering.

  * Enhanced `Evaluation` Monad to include a mutable state reference.

  * Enhanced `setRandomSeed` to perform a mutable update.


### [`PHANE-measure-class`][GitHub-Lib-05]

  * Added initial library version

  * Applied [`fourmolu`][Hackage-fourmolu] styling specified by `config/fourmolu.yaml`


### [`PHANE-measure-transition`][GitHub-Lib-06]

  * Added initial library version

  * Applied [`fourmolu`][Hackage-fourmolu] styling specified by `config/fourmolu.yaml`


### [`PHANE-measure-units`][GitHub-Lib-02]

  * Added initial library version

  * Applied [`fourmolu`][Hackage-fourmolu] styling specified by `config/fourmolu.yaml`


### [`PHANE-transition-matrix`][GitHub-Lib-08]

  * Added initial library version

  * Applied [`fourmolu`][Hackage-fourmolu] styling specified by `config/fourmolu.yaml`


### [`PHANE-transition-matrix-ffi`][GitHub-Lib-09]

  * Added initial library version

  * Applied [`fourmolu`][Hackage-fourmolu] styling specified by `config/fourmolu.yaml`

  * Updated type-class `HasStateTransitionsCompact` to expose opaque structure


### [`PHANE-PhyloLib`][GitHub-Lib-03]

  * Applied [`fourmolu`][Hackage-fourmolu] styling specified by `config/fourmolu.yaml`

  * Moved from PhyG repository


## `v0.1.0`

  * Initial "alpha" state of PHANE domain


[GitHub-Lib-00]: https://github.com/amnh/PHANE/tree/main/pkg/PHANE-alphabet#readme
[GitHub-Lib-01]: https://github.com/amnh/PHANE/tree/main/pkg/PHANE-dynamic-character-element#readme
[GitHub-Lib-02]: https://github.com/amnh/PHANE/tree/main/pkg/PHANE-measure-units#readme
[GitHub-Lib-03]: https://github.com/amnh/PHANE/tree/main/pkg/PHANE-PhyloLib#phane-phylolib
[GitHub-Lib-04]: https://github.com/amnh/PHANE/tree/main/pkg/PHANE-dynamic-character#readme
[GitHub-Lib-05]: https://github.com/amnh/PHANE/tree/main/pkg/PHANE-measure-class#readme
[GitHub-Lib-06]: https://github.com/amnh/PHANE/tree/main/pkg/PHANE-measure-transition#readme
[GitHub-Lib-07]: https://github.com/amnh/PHANE/tree/main/pkg/PHANE-dynamic-character-alignment#readme
[GitHub-Lib-08]: https://github.com/amnh/PHANE/tree/main/pkg/PHANE-transition-matrix#readme
[GitHub-Lib-09]: https://github.com/amnh/PHANE/tree/main/pkg/PHANE-transition-matrix-ffi#readme
[GitHub-Lib-10]: https://github.com/amnh/PHANE/tree/main/pkg/PHANE-evaluation#readme
[GitHub-Changelog]: https://github.com/amnh/PHANE/blob/main/doc/Changelog.md
[Hackage-fourmolu]: https://hackage.haskell.org/package/fourmolu#fourmolu
[SemVer-URI]: https://semver.org/spec/v2.0.0.html

