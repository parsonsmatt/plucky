# plucky

Haskell [has a problem with problems](https://www.parsonsmatt.org/2018/11/03/trouble_with_typed_errors.html).
We have `IO` exceptions for unchecked exceptions - the same sort that plague Ruby, Python, JavaScript, and other languages.
We have `ExceptT` for checked exceptions, but Java has us beat on ergonomics and usability of checked exceptions.
Can we do better?
Well, the blog post points to [`generic-lens`](https://hackage.haskell.org/package/generic-lens), but that's kind of hard to use.
Can we do better, and easier?

Yes! I alluded to this in my blog post [Plucking Constraints](https://www.parsonsmatt.org/2020/01/03/plucking_constraints.html).
This library provides an API and demonstration on 'plucking' error constraints.
