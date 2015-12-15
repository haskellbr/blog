---
layout: post
title: Sobre Text e OverloadedStrings
author: Pedro Tacla Yamada
author_url: "https://github.com/yamadapc"
excerpt_separator: "<!-- more -->"
---

Essa é uma nota rápida para completar o
[dia 4 da série 24 dias de Hackage](/2015/12/12/24-dias-de-hackage-2015-dia-4-wreq-programacao-de-clientes-web-com-notas-sobre-lens-e-a-sintaxe-de-operadores.html). Vou
comentar brevemente sobre a extensão `OverloadedStrings` e os tipos `String`,
`ByteString` e `Text`.

<!-- more -->

Você pode ter notado que Haskell tem mais de um tipo para representar texto.
Isso é, de fato, um problema do ecossistema da linguagem, mas não vale a pena
discutir as críticas a fundo. Como expliquei
[no primeiro post desse blog em português](http://localhost:4000/2015/12/04/implementando-fibonacci-em-haskell.html#por-que-digo-listas-e-no-arrays)
o literal `[1, 2, 3]` em Haskell é uma lista-ligada, não um Array. O tipo
`String` em Haskell também é uma lista:

{% highlight haskell %}
type String = [Char]
{% endhighlight %}

Você pode imaginar que isso causa problemas de performance, porque listas não
são a melhor estrutura de dados para muitas tarefas relacionadas a
processamento de texto.

Por isso, há dois pacotes que implementam tipos para strings com características
diferentes:

* [`text`](https://hackage.haskell.org/package/text)
* [`bytestring`](https://hackage.haskell.org/package/bytestring)

Ambos os pacotes seguem a convenção de exportar funções `pack` que recebem uma
`String` e retornam um `Text` ou uma `ByteString`. O que a extensão
`OverloadedStrings` faz é usar uma type-class definida em [`Data.String`](https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-String.html) chamada
`IsString`. A type-class define:

{% highlight haskell %}
class IsString s where
    fromString :: String -> s
{% endhighlight %}

Algumas implementações da type-class seriam:

{% highlight haskell %}
instance IsString String where
    fromString = id

instance IsString Text where
    fromString = pack
{% endhighlight %}

Assim, quando escrevemos o literal `"asdfasdf"` no código a extensão vai
reescrever isso como `(fromString "asdfasdf")`. Isso nos deixa ter
`"asdfasdf" :: String` e `"asdfasdf" :: Text`. Na maior parte dos casos o
compilador vai só inferir o tipo certo e as coisas vão funcionar fluidamente.

- - -

Outra extensão na mesma linha é [`OverloadedLists`](https://ghc.haskell.org/trac/ghc/wiki/OverloadedLists).
Ela usa type-class `IsList`, que é basicamente definida como:

{% highlight haskell %}
class IsList l where
    type Item l
    fromList :: [Item l] -> l
{% endhighlight %}

Isso quer dizer que uma instância para um "tipo alvo" tem que definir um tipo de
elemento e poderemos converter literais de listas desse tipo para o "tipo alvo".
Em outras palavras, se queremos escrever `Set`s do
[`Data.Set`](http://hackage.haskell.org/package/containers-0.5.3.1/docs/Data-Set.html)
como listas, adicionamos a instância:

{% highlight haskell %}
import qualified Data.Set

instance (Ord a) => IsList (Data.Set.Set a) where
    type Item (Data.Set.Set a) = a
    fromList = Data.Set.fromList
{% endhighlight %}

Ao ativar a extensão se pode escrever

{% highlight haskell %}
[1, 2, 3, 4] :: Data.Set.Set
{% endhighlight %}

Conhecendo essas extensões o problema não parece tão ruim assim.  Acabo de
mostrar uma forma com a qual Haskell nos deixa ter literais para tipos
definidos pelo usuário. Estou curioso sobre quais outras linguagens que te
permitem fazer isso.
