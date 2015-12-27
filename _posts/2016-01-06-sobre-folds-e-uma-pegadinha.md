---
layout: post
title: Sobre folds e uma pegadinha
author: Hercules Lemke Merscher
author_url: "https://github.com/hlmerscher"
---

Em Haskell uma maneira bem comum de iterar sobre uma lista e acumular valores é por meio de recursão. Geralmente usamos pattern matching para tratar o caso da lista vazia, seguido do caso da lista com um ou mais valores, aplicando a função e chamando a função novamente com o restante da lista.

{% highlight haskell %}
somaInteiros :: [Int] -> Int
somaInteiros []     = 0
somaInteiros (x:xs) = x + (somaInteiros xs)

somaInteiros [1,2,3,4,5] -- 15
{% endhighlight %}

Na medida que vamos nos habituando com recursão esse tipo de função se torna comum em código Haskell. Por se tratar de um padrão muito comum, algumas funções foram incluídas junto da linguagem para abstrair o uso de recursão, e essas funções são chamadas _folds_.

<!-- more -->

Uma função fold recebe 3 parâmetros, a função acumuladora, um valor inicial e a lista que será acumulada.

### foldr

Acumulando uma lista com a função __foldr__:

{% highlight haskell %}
foldr (+) 0 [1,2,3,4,5]

{- execução passo a passo

1 + (foldr (+) 0 [2,3,4,5]
1 + (2 + (foldr (+) 0 [3,4,5]))
1 + (2 + (3 + (foldr (+) 0 [4,5])))
1 + (2 + (3 + (4 + (foldr (+) 0 [5]))))
1 + (2 + (3 + (4 + (5 + (foldr (+) 0 [])))))

1 + (2 + (3 + (4 + (5 + 0))))
1 + (2 + (3 + (4 + 5)))
1 + (2 + (3 + 9))
1 + (2 + 12)
1 + 14
15
-}
{% endhighlight %}

Manipular pequenas listas não será um problema, agora imagine uma lista com 1000000000 itens. Para cada item a função _+_ adicionará um item a pilha de execução enquanto aguarda o resultado da execução seguinte do foldr, até que um estouro de pilha aconteça.

{% highlight haskell %}
foldr (+) 0 [1..1000000000]

*** Exception: stack overflow
{% endhighlight %}

O problema com a função _foldr_ é que os resultados intermediários poderiam ser acumulados antes da próxima chamada, o que não é feito, como deixa claro no exemplo acima.

### foldl

Para resolver este problema temos a função __foldl__, que vai funcionar de maneira análoga, mas com uma diferença sutil, vai acumular os itens de cada etapa até o final, o que evita milhares de itens sendo adicionados a pilha de execução. Seria algo assim:

{% highlight haskell %}
foldl f z []     = z
foldl f z (x:xs) = let y = f z x
                   in foldl f y xs
{% endhighlight %}

Executando o _foldl_ em uma lista com muitos valores:

{% highlight haskell %}
foldl (+) 0 [1..1000000000]

*** Exception: stack overflow
{% endhighlight %}

WTF! Se está acumulando os valores intermediários, porque diabos está acontecendo um estouro de pilha?

Rá! Pegadinha do malandro.

Devemos lembrar que Haskell é uma linguagem com avaliação preguiçosa, e só avalia uma expressão quando esta realmente é necessária para retornar um valor, por conta disso os valores não estavam sendo acumulados de fato. Vamos ao passo a passo de execução usando _foldl_:

{% highlight haskell %}
foldl (+) 0 [1,2,3,4,5]

{- execução passo a passo
let v1 = 0 + 1
in foldl (+) v1 [2,3,4,5]

let v1 = 0 + 1
    v2 = v1 + 2
in foldl (+) v2 [3,4,5]

let v1 = 0 + 1
    v2 = v1 + 2
    v3 = v2 + 3
in foldl (+) v3 [4,5]

let v1 = 0 + 1
    v2 = v1 + 2
    v3 = v2 + 3
    v4 = v3 + 4
in foldl (+) v4 [5]

let v1 = 0 + 1
    v2 = v1 + 2
    v3 = v2 + 3
    v4 = v3 + 4
    v5 = v4 + 5
in foldl (+) v5 []

((((0 + 1) + 2) + 3) + 4) + 5
(((1 + 2) + 3) + 4) + 5
((3 + 3) + 4) + 5
(6 + 4) + 5
10 + 5
15
{% endhighlight %}

Ao invés de acumular os valores, os mesmos agora estão sendo alocados no heap, que é limitado pela quantidade de memória e swap presente na máquina, o que acaba causando o um _stack overflow_ quando não há mais espaço para alocação. Triste, não? :(

É um comportamento inesperado que surpreende a primeira vez que nos deparamos com isso. Mas então, como resolver isso?

### foldl'

Temos de forçar a avaliação dos acumuladores para evitar este problema, e é exatamente o que a função __foldl'__ faz, utilizando por debaixo dos panos a função _seq_:

{% highlight haskell %}
foldl' f z []     = z
foldl' f z (x:xs) = let z' = f z x
                    in seq z' (foldl' f z' xs)
{% endhighlight %}

A função _seq_ é uma função que recebe 2 parâmetros, avalia o primeiro e retorna o segundo, perfeito para resolver o problema do _foldl_. Com o acumulador sendo avaliado forçadamente em cada passo, não temos alocações exageradas no heap, com isso evita-se o estouro de pilha.

{% highlight haskell %}
foldl' (+) 0 [1..100000000] -- 5000000050000000
{% endhighlight %}

### Conclusão

Para a maioria dos casos _foldr_ será suficiente, funcionando mesmo com listas infinitas. Para os outros casos quando a lista manipulada for bem grande e finita, foldl' será a opção. Dado os motivos já discutidos anteriormente neste post, é melhor evitar o foldl.

Para mais detalhes, consulte a wiki do site [haskell.org](https://www.haskell.org/) em inglês: [Foldr_Foldl_Foldl'](https://wiki.haskell.org/Foldr_Foldl_Foldl')
