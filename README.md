# Rows of String Values (RSV)

This Haskell library implements RSV, a clever format designed as a simpler and more reliable replacement for CSV. You can read more about RSV [here](https://github.com/Stenway/RSV-Specification).

## Haskell Implementation

- Any value which implements `ToValue` may be written to RSV.
- Any value which implements `FromValue` may be read from RSV.
- Any type which implements `ToRow` may be written to an RSV row.
- Any type which implements `FromRow` may be read from an RSV row.
- See the unit tests for usage.
