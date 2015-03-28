# Methods #

| `ToString` | Returns a string representation of the list. |
|:-----------|:---------------------------------------------|
| `Length` | Returns the number of elements in the list. |
| `Get index` | Returns the element at position `index`. |
| `Head` | Returns the first element of the list. |
| `Tail` | Returns all elements of the list, except the first element, in the same order. |
| `Reverse` | Returns a list with the same elements, but in reverse order. |
| `Concatenate other` | Returns a list where all the elements of this list is followed by all the elements of the `other` list. |
| `Each f` | Calls the `f` function on each element in the list. Equivalent to `(f(list.Get(0)); f(list.Get(1)); ...; f(list.Get(list.Length - 1)); ())`. |
| `Map f` | Returns a list where each element has been passed through the `f` function. Equivalent to `[f(list.Get(0)), f(list.Get(1)), ..., f(list.Get(list.Length - 1))]`. |
| `Filter p` | Returns a list only containing the elements for which the `p` function returns true. |
| `FoldLeft s f` | Accumulates a list by calling `f` with the arguments `s` and the first element of the list, then calling `f` with the return value of the last call and the second element, then the return value and the third element, until there are no more elements in the list. Equivalent to `(f ... (f (f s list.Get(0)) list.Get(1)) ... list.Get(list.Length - 1))`. |
| `FoldRight s f` | Same as `FoldLeft`, except it starts with the last element, then the second last element, etc. |
| `Sort before` | Returns the elements in sorted order such that an element `a` comes before another element `b` if `before a b` returns true. The sort is stable, meaning that it only changes the order of two elements if strictly necessary. |
| `All p` | Returns true precisely if `p` returns true for all the elements in the list. It returns true for the empty list. |
| `Any p` | Returns true precisely if `p` returns true for at least one element in the list. It returns false for the empty list. |
| `Equal other` | Returns true if the lists have the same length and the elements of the `other` list are all equal to the corresponding elements in this list. |