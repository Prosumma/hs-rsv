# Changelog for `rsv`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 1.2.1.0 - 2024-01-25

### Added

- Added `EncoderContext` newtype wrapper around `Reader ParserConfig`.

### Changed

- `Encoder` is now `EncoderContext Builder` instead of `Reader ParserConfig Builder` (but it's just a newtype wrapper).
- `ValueParser` and `RowParser` are now just `Parser`.

### Removed

- `mappendA`, `foldApp` and `<+>` have been removed in favor of the Monoid instance of `EncoderContext`.
