# Methods #

| `ToString` | Returns itself. |
|:-----------|:----------------|
| `ToNumber` | Tries to convert the string into a number. If it succeeds, it returns the number, otherwise it returns `NaN` (not a number). |
| `Unicodes` | Returns the sequence of Unicode characters as a list of numbers. |
| `Length` | Returns the number of Unicode characters in the string. |
| `Uppercase` | Returns a copy of this string, except all characters that have an upper case version are converted to that form. |
| `Lowercase` | Returns a copy of this string, except all characters that have a lower case version are converted to that form. |
| `Capitalize` | Returns a copy of this string, except the first character is in upper case if it can be. |
| `Uncapitalize` | Returns a copy of this string, except the first character is in lower case if it can be. |
| `Left n` | Returns a string with the first `n` characters of this string. |
| `Right n` | Returns a string with the last `n` characters of this string. |
| `Between l r` | Returns a sub string starting at character `l` and ending just before character `r`. |
| `From l n` | Returns a sub string of length `n` starting at character `l`. |
| `Less other` | Returns true if this string comes before the `other` string lexicographically. |
| `Greater other` | Returns true if this string comes after the `other` string lexicographically. |
| `LessEqual other` | Returns true if this string doesn't come after the `other` string lexicographically. |
| `GreaterEqual other` | Returns true if this string doesn't come before the `other` string lexicographically. |
| `Equal other` | Returns true if the `other` string represents the same sequence of characters. |