# Methods #

| `ToString` | Returns a string representation (`"true"` or `"false"`). |
|:-----------|:---------------------------------------------------------|
| `Then t` | Calls `t` if the boolean is true and returns it's value, or `()` if the boolean is false. |
| `Else e` | Calls `e` if the boolean is false and returns it's value, or `()` if the boolean is true. |
| `ThenElse t e` | Calls `t` if the boolean is true, or `e` if it's false. The result of the called function is returned. |
| `AndThen f` | Returns false without calling `f` if the boolean is false, otherwise returns the truth value of calling f. |
| `OrElse f` | Returns true without calling `f` if the boolean is true, otherwise returns the truth value of calling f. |
| `And b` | Returns true if this boolean is true and `b` is true, otherwise false. |
| `Or b` | Returns true if this boolean is true, `b` is true or both, otherwise false. |
| `Xor b` | Returns true if this boolean is true or `b` is true, but not both. |
| `Not` | Returns true if the boolean is false, otherwise it returns true. |
| `Equal other` | Returns true if the `other` value is a boolean and they're both _true_ or both _false_. |