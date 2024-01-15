# Rows of String Values (RSV)

This Haskell library implements RSV, a clever format designed as a simpler and more reliable replacement for CSV. You can read more about RSV [here](https://github.com/Stenway/RSV-Specification).

## Haskell Implementation

- Any value which implements `ToRSV` may be written to RSV.
- Any value which implements `FromRSV` may be read from RSV.
- Any type which implements `ToRSVRow` may be written to an RSV row. This includes any type of the form `(Foldable t, ToRSV a) => t a`, such as lists, vectors, etc. In addition, arbitrary Haskell types may implement `ToRSVRow` to be encoded in this way.
- Any type which implements `FromRSVRow` may be read from an RSV row. Because there is no  