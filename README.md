# Rows of String Values (RSV)

This Haskell library implements RSV, a clever format designed as a simpler and more reliable replacement for CSV. You can read more about RSV [here](https://github.com/Stenway/RSV-Specification).

## Haskell Implementation

- Any value which implements `ToValue` may be written to RSV.
- Any value which implements `FromValue` may be read from RSV.
- Any type which implements `ToRow` may be written to an RSV row.
- Any type which implements `FromRow` may be read from an RSV row.
- Since `ByteString`s are not guaranteed to be UTF-8, they are assumed to be binary data and are encoded in Base-64.
- The `String` type is also not guaranteed to be UTF-8, but since it's so commonly used, it will be written directly to RSV format and assumed to be UTF-8. Its implementation of `toValue` uses `encodeStringUnsafe`. If you write a `String` that is not UTF-8, you will get an exception when reading it back out. For this reason I do not recommend the use of the `String` type with RSV, since it's unsafe.
- It's strongly recommended to use the `Text` type to represent strings.
- Common data types like `Int`, `Integer`, `Double`, `UUID`, etc. can be encoded directly to RSV.
- The `Bool` data type is encoded by default as the strings `true` and `false`. When reading the `Bool` type, the (case-insensitive) strings `true`, `t`, `yes`, `y`, `1`, `false`, `f`, `no`, `n` and `0` will be recognized. If you want to use some other string values, you have two choices. You can create a `newtype` wrapper around `Bool` and implement `ToValue` and `FromValue` for it. Or you can change the `ParserConfig` and pass it to `encodeWith` and `parseWith`. See the unit tests.
- See the unit tests for usage, especially of advanced techniques like writing your own record types as RSV rows.
