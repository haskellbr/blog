---
categories:
- Haskell
- Hackage
- safe
- safety
- exceptions
- PureScript
- Elm
- psychology
comments: true
date: 2015-12-16T08:00:00-05:00
layout: post
title: "24 days of Hackage, 2015: day 16: safe; what is safety anyway?"
---
## Table of contents for the whole series

A table of contents is at the top of the article for [day 1](/blog/2015/11/30/haskell-tidbits-24-days-of-hackage-2015-day-1-introduction-and-stack/).

## Day 16

([Reddit discussion](https://www.reddit.com/r/haskell/comments/3x41la/24_days_of_hackage_2015_day_16_safe_what_is/))

Today I'm doing something strange: up till now, I've discussed
libraries and tools I actually use. Today I'm discussing a library I
do *not* use, while thinking about why I not, and why you or I might
want to use it. This library is
[`safe`](http://hackage.haskell.org/package/safe), which aims to
protect against the unfortunate "unsafety" of common functions in the
standard Prelude.

That's a good thing, right? After all, on
[day 7](/blog/2015/12/07/24-days-of-hackage-2015-day-7-semigroups-nonempty-list-and-a-case-study-of-types-and-tests/),
I promoted the use of the `NonEmpty` list, and again used in
[day 14](/blog/2015/12/14/24-days-of-hackage-2015-day-14-earley-a-promising-newer-parser-library-for-haskell/). I
like safety, but what does "safety" mean anyway?

<!--more-->

## Safety is relative

A notion of *safety* is always relative to some criterion, some
expectation, and even more generally, in the context of some *way of
life*. And ways of life can be safeguarded through different
mechanisms, from strict protection to "style guides" to implicit
social contracts and ostracism.

In the context of the `safe` package, the kind of safety we are
concerned about is not ever wanting to see a running program crash
with an *exception* from pure code like

{% highlight console %}
*** Exception: Prelude.head: empty list
{% endhighlight %}

This happens if you call `head` on an empty list:

{% highlight haskell %}
-- | Crashes if nobody in line.
unsafeReportFirstInLine :: [Int] -> String
unsafeReportFirstInLine nums =
  "next up: customer " ++ show (head nums)
{% endhighlight %}

{% highlight console %}
> unsafeReportFirstInLine []
"next up: customer *** Exception: Prelude.head: empty list
{% endhighlight %}

The "safe" solution is **don't ever call `head` on a list that might
be empty**. There are different ways to achieve this.

## Solving a human psychology problem?

One possible solution is to change the type of `head`.

Providing functions such as `head` in the Haskell Prelude is arguably
a historical legacy mistake from 1990, because it encourages
programmers (especially those starting out with Haskell, especially
when using instructional materials that use `head`!) to call
`head`. *If you give people the easy ability to do something unsafe,
they will surely do it, and often.*

Newer language communities attack the human psychology problem by *not
providing* an unsafe `head`: for example, the standard PureScript
ecosystem provides a
[safe `Data.List.head`](https://github.com/purescript/purescript-lists/blob/master/docs/Data/List.md#head)
with type `forall a. List a -> Maybe a` but also provides an entire
[`Data.List.Unsafe`](https://github.com/purescript/purescript-lists/blob/master/docs/Data/List/Unsafe.md)
module that includes
[`Data.List.Unsafe.head`](https://github.com/purescript/purescript-lists/blob/master/docs/Data/List/Unsafe.md#head)
with type `forall a. List a -> a`.

And Elm
[`List` module](http://package.elm-lang.org/packages/elm-lang/core/3.0.0/List)
provides
[`List.head`](http://package.elm-lang.org/packages/elm-lang/core/3.0.0/List#head)
of type `List a -> Maybe a`.

Marking something as unsafe at least enables the writer and reader of
code to make note that something might go wrong, so I think this is a
good starting point for a solution. Furthermore, psychology research
has shown clearly that defaults matter: if the goal is to promote
safety, it is better to have the default be safe, and "opt out"
explicitly to be unsafe, rather than have the default be unsafe, and
"opt in" to be safe.

Unfortunately, for historical reasons, if you're working with lists
and some other data structures in Haskell, you are forced to opt in to
safety, rather than opt out to unsafety.

The `safe` package allows you to opt in to safety.

You get functions such as
[`Safe.headMay`](http://hackage.haskell.org/package/safe-0.3.9/docs/Safe.html#v:headMay)
with type `[a] -> Maybe a`.

{% highlight haskell %}
-- | Using 'Safe.headMay' and pattern matching on Maybe.
reportFirstInLine :: [Int] -> String
reportFirstInLine nums =
  case Safe.headMay nums of
    Just num -> "next up: customer " ++ show num
    Nothing -> "there are no customers in line"
{% endhighlight %}

## Pattern-matching directly on the data structure as an alternative

In practice, I don't use functions like `headMay` because I usually
just pattern-match on the list itself:

{% highlight haskell %}
-- | Using pattern matching on list.
reportFirstInLine2 :: [Int] -> String
reportFirstInLine2 [] = "there are no customers in line"
reportFirstInLine2 (num:_) = "next up: customer " ++ show num
{% endhighlight %}

When I think about it, though, there is something not quite right
about this solution. The wildcard `_` in the pattern gives away the
fact that we are getting back more information than we actually
*need*. In principle we should ask only for what we need, and
`headMay` does precisely that. I'm basically violating conceptual
encapsulation by getting back more than I need (the tail) and ignoring
it.

So I believe I should really start using
`headMay` in this kind of context, and thinking more deeply, I believe
that the single reason I haven't is that the standard Prelude didn't
provide it! It was easier to do the pattern-matching than to find
`safe` and add it as a dependency.

**How many of you think like me, and would happily use `headMay` if it
were part of the standard Prelude, but because it is not, you use a
wildcarded pattern match on a list instead?**

## Revisiting the `headMay` solution

Some of you might like using the `maybe` function that has type `b ->
(a -> b) -> Maybe a -> b`, to avoid pattern matching on `Maybe`:

{% highlight haskell %}
-- | No pattern matching.
reportFirstInLine3 :: [Int] -> String
reportFirstInLine3 =
  maybe "there are no customers in line" reportOne . Safe.headMay

reportOne :: Int -> String
reportOne num = "next up: customer " ++ show num
{% endhighlight %}

There's also a code golf version that I do *not* recommend:

{% highlight haskell %}
-- | Code golf.
reportFirstInLine4 :: [Int] -> String
reportFirstInLine4 =
  maybe "there are no customers in line"
        (("next up: customer " ++) . show)
        . Safe.headMay
{% endhighlight %}

## Other `safe` goodies

Each of the `...May` functions also has useful variants:

One allows specifying a default to return upon empty:

{% highlight haskell %}
headDef :: a -> [a] -> a
{% endhighlight %}

Another is unsafe, but at least generates a better exception. This is
useful if you *know* that a list is not empty, and do not want to
handle the case in which it is empty, but *just in case*, generate an
exception that if triggered, at least tells you where your "internal
fatal error" came from.

{% highlight haskell %}
headNote :: String -> [a] -> a
{% endhighlight %}

## Exactness is a safety issue

The
[`Safe.Exact`](http://hackage.haskell.org/package/safe-0.3.9/docs/Safe-Exact.html)
module provides a lot of useful functions that have to do with
indexing into a list or the checking the sizes of lists. Here,
"safety" no longer refers to an exception. It refers to something more
insidious: code you write that *typechecks and runs but does the
unintended thing*. For example, it is very easy to use `take` in a way
that you don't intend because it *silently* allows you to "take" more
elements from a list than it contains, but just assumes you know what
you are doing and don't "really" mean "take 1000 elements" but rather
"take 1000 elements or if there aren't 1000, take all the elements". I
have been bitten by `take` before, where I passed in an absurd number
that I did not *intend*. So the `Safe.Exact.takeExact...` family of
functions is quite useful. In the past, before I discovered `safe`, I
ended up basically writing my own wrappers, and now I won't do that
again.

## Foldable

Finally the
[`Safe.Foldable`](http://hackage.haskell.org/package/safe-0.3.9/docs/Safe-Foldable.html)
module is useful because `Foldable` is full of unsafe operations.

## When you know something about your data that the type doesn't know

A final note on exactness of list operations as a safety issue: the
principled solution to inexactness when it comes to indexing into or
size of lists is to *turn the potential bugs into type errors*, using
dependent types: a "list" type that is dependent on its size. A later
Day of Hackage will mention solutions.

## Conclusion

The `safe` package is a nice utility library that wraps "unsafe"
Prelude operations. There are technical and psychological reasons I
haven't used it, and I discussed them, but I will use it in the future
when it fits my needs.

## All the code

All my code for my article series are at
[this GitHub repo](https://github.com/FranklinChen/twenty-four-days2015-of-hackage).
