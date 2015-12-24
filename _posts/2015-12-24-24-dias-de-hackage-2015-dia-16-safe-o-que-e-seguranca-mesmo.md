---
layout: post
title: "24 dias de Hackage, 2015 - dia 16 - safe; o que é segurança mesmo?"
author: Franklin Chen
author_url: "http://conscientiousprogrammer.com/"
translator: Pedro Yamada
translator_url: "https://github.com/yamadapc"
excerpt_separator: "<!-- more -->"
---
_Esse é um artigo escrito por Franklin Chen e traduzido para o português.
[Ler original](http://conscientiousprogrammer.com/blog/2015/11/30/haskell-tidbits-24-days-of-hackage-2015-day-1-introduction-and-stack/)_

## Encontro HaskellBR São Paulo
[Vamos nos encontrar em São Paulo em 25 de Janeiro de 2016. Marque sua presença
e comente se não puder vir.](http://www.meetup.com/haskellbr-sp/events/227526368/)

## Índice de toda a série
O índice de toda a série está no topo do artigo para o [dia 1](/2015/12/08/aperitivos-de-haskell-24-dias-de-hackage-2015-dia-1-introducao-e-stack.html).

## Dia 16

([Discussão no Reddit do original](https://www.reddit.com/r/haskell/comments/3x41la/24_days_of_hackage_2015_day_16_safe_what_is/))

Hoje farei algo estranho: até agora, discuti bibliotecas e ferramentas que eu
de fato uso. Hoje, vou discutir uma biblioteca que **não** uso e pensar um
pouco sobre porque não a uso e porque poderia querer a usar. Essa biblioteca é
o [`safe`](http://hackage.haskell.org/package/safe), que tenta nos proteger da
infeliz "insegurança" de funções comuns do `Prelude` padrão.

Isso é uma coisa boa, certo? Afinal, no
[dia 7](/2015/12/15/24-dias-de-hackage-2015-dia-7-semigroups-lista-nonempty-e-um-caso-de-estudo-de-tipos-e-testes.html),
eu promovi o uso da lista `NonEmpty` e de novo a usei no
[dia 14](/2015/12/21/24-dias-de-hackage-2015-dia-14-earley-uma-biblioteca-de-parsers-promisora-para-haskell.html).
Eu gosto de segurança, mas o que é "segurança mesmo"?

<!-- more -->

## Segurança é relativa

A noção de _segurança_ é sempre relativa a algum critério, alguma expectativa
e, de forma mais geral, no contexto de algum _estilo de vida_. E _estilos de
vida_ sempre podem ser protegidos por mecânismos diferentes, desde a proteção
estríta até _guias de estilo_ ou contratos sociais implícitos e ostracismo.

No contexto do pacote `safe`, o tipo de segurança sobre o qual estamos
preocupados é nunca ver um programa quebrar com uma _exceção_ vinda de código
puro como:

{% highlight console %}
*** Exception: Prelude.head: empty list
{% endhighlight %}

Isso acontece se você chama `head` em uma lista vazia:

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

A solução segura é **nunca chame `head` em uma lista que pode estar vazia**. Há
formas diferentes de garantir isso.

## Resolvendo um problema da psicologia humana?
Uma solução é mudar o tipo de `head`.

Podemos dizer que funções como `head` no Prelude do Haskell são um erro
histórico de 1990 que continou na linguagem, porque ela encoraja programadores
(principalmente recem-chegados) a chamar `head`. _Se for fácil fazer coisas
inseguras, as pessoas vão certamente as fazer e provavelmente as fazer
frequentemente._

Comunidades de linguagens mais novas atacam o problema psicológico _não
fornecendo_ uma função insegura `head`; por exemplo, o ecossistema padrão do
PureScript fornece um
[seguro `Data.List.head`](https://github.com/purescript/purescript-lists/blob/master/docs/Data/List.md#head)
com tipo `forall a. List a -> Maybe a`, mas também fornece um módulo
[`Data.List.Unsafe`](https://github.com/purescript/purescript-lists/blob/master/docs/Data/List/Unsafe.md)
que incluí
[`Data.List.Unsafe.head`](https://github.com/purescript/purescript-lists/blob/master/docs/Data/List/Unsafe.md#head)
com tipo `forall a. List a -> a` e mais.

E o
[módulo `List`](http://package.elm-lang.org/packages/elm-lang/core/3.0.0/List)
do Elm fornece
[`List.head`](http://package.elm-lang.org/packages/elm-lang/core/3.0.0/List#head)
com tipo `List a -> Maybe a`.

Marcando algo como inseguro pelo menos permite ao escritor e leitor do código a
tomarem nota de que algo pode dar errado, então eu acho que esse é um bom
começo para uma solução. Além disso, a pesquisa de psicologia tem mostrado
claramente que padrões importam: se o objetivo é promover segurança, é melhor
que o padrão seja seguro, e desabilitar ele explicitamente para ser inseguro,
ao invés do contrário.

Infelizmente, por razões históricas, se você está trabalhando com listas ou
algumas outras estruturas de dados em Haskell, você é forçado a ter que
escolher explicitamente ser seguro, já que o padrão é inseguro.

O pacote `safe` nos deixa ser seguros de forma mais fácil.

Você ganha funções como
[`Safe.headMay`](http://hackage.haskell.org/package/safe-0.3.9/docs/Safe.html#v:headMay)
com tipo `[a] -> Maybe a`.

{% highlight haskell %}
-- | Using 'Safe.headMay' and pattern matching on Maybe.
reportFirstInLine :: [Int] -> String
reportFirstInLine nums =
  case Safe.headMay nums of
    Just num -> "next up: customer " ++ show num
    Nothing -> "there are no customers in line"
{% endhighlight %}

## Uma alternativa: fazendo _pattern-matching_ diretamente na estrutura de dados

Na prática, eu não uso funções como `headMay`, porque eu só faço
_pattern-matching_ na própria lista:

{% highlight haskell %}
-- | Using pattern matching on list.
reportFirstInLine2 :: [Int] -> String
reportFirstInLine2 [] = "there are no customers in line"
reportFirstInLine2 (num:_) = "next up: customer " ++ show num
{% endhighlight %}

Quando eu penso a respeito, no entanto, há algo que cheira mal nessa solução. O
wildcard `_` no pattern mostra que nós estamos extraindo mais informação do que
_precisamos_ de fato. A princípio, nós poderíamos só extrair o que precisamos e
`headMay` faz exatamente isso. Eu estou basicamente violando o encapsulamento
conceitual extraindo e ignorando mais do que eu preciso (o resto da lista).

Então acredito que eu de fato deveria começar a usar o `headMay` nesse tipo de
contexto e pensando mais profundamente, acho que o único motivo que esse ainda
não é o caso é que o Prelude padrão não o incluí! Era mais fácil fazer
_pattern-matching_ do que encontrar o `safe` e o adicionar como uma
dependência.

**Quantos de vocês pensam como eu e usariam o `headMay` se ele fizesse parte do
Prelude, mas já que ele não faz, usam um pattern match na lista?**

## Revisitando a solução `headMay`
Alguns de vocês podem gostar de usar a função `maybe` que tem tipo `b -> (a ->
b) -> Maybe a -> b`, para evitar fazer _pattern-matching_ no `Maybe`:

{% highlight haskell %}
-- | No pattern matching.
reportFirstInLine3 :: [Int] -> String
reportFirstInLine3 =
  maybe "there are no customers in line" reportOne . Safe.headMay

reportOne :: Int -> String
reportOne num = "next up: customer " ++ show num
{% endhighlight %}

Também há uma versão _"code-golf"_ que eu *não* recomendo:

{% highlight haskell %}
-- | Code golf.
reportFirstInLine4 :: [Int] -> String
reportFirstInLine4 =
  maybe "there are no customers in line"
        (("next up: customer " ++) . show)
        . Safe.headMay
{% endhighlight %}

## Outras boas coisas no `safe`
Cada uma das funções `...May` também tem outras variações úteis.

Uma delas permite especificar um padrão para retornar no caso de que lista
esteja vazia:

{% highlight haskell %}
headDef :: a -> [a] -> a
{% endhighlight %}

Outra é insegura, mas pelo menos gera uma exceção melhor. Isso é útil se você
*sabe* que uma lista não está vazia, e você não quer ter que lidar com o caso
em que ela está vazia, mas *por via das dúvidas*, gerar uma exceção que se
atirada, pelo menos te diz de onde o seu "erro interno fatal" veio.

{% highlight haskell %}
headNote :: String -> [a] -> a
{% endhighlight %}

## Ser _exato_ é um problema de segurança

O módulo
[`Safe.Exact`](http://hackage.haskell.org/package/safe-0.3.9/docs/Safe-Exact.html)
fornece uma série de funções úteis que têm a ver com acessar elementos
arbitrários de uma lista e checar o tamanho de listas. Aqui, "segurança" não
mais se refere a uma exceção. Ela se refere a algo mais sútil: o código que
você escreve que *dá type-check e roda, mas faz algo inesperado*. Por exemplo,
é fácil usar o `take` de alguma forma que você não queira, porque ele permite
*silenciosamente* que você "pegue" mais elementos da lista do que ela contém,
mas só assume que sabe o que está fazendo e não quer dizer "pegue 1000
elementos" mas "pegue 1000 elementos e se não tiverem 1000 elementos, pegue
todos os elementos". Eu já fui mordido pelo `take` antes, em uma ocasião em que
passei um número absurdo que eu não *queria*. Então a família de funções
`Safe.Exact.takeExact...` é muito útil. No passado, antes de descobrir sobre o
`safe`, eu basicamente escrevia meus próprios wrappers e agora não vou ter mais
que fazer isso.

## Foldable
Finalmente o módulo
[`Safe.Foldable`](http://hackage.haskell.org/package/safe-0.3.9/docs/Safe-Foldable.html)
é útil porque o `Foldable` é cheio de operações inseguras.

## Quando você sabe alguma coisa sobre seus dados que o tipo não sabe
Uma nota final sobre tratar a ideia de _ser exato_ em operações sobre listas
como um problema de segurança: a solução embasada à _não ser exato_ quando se
trata de acessar um elemento ou o tamanho de listas é _transformar os bugs em
potencial em erros de tipo_, usando tipos dependentes; um tipo de lista que é
dependente no seu tamanho. Um dia de Hackage posterior vai mostrar soluções.

## Conclusão
O pacote `safe` é uma boa biblioteca utilitária que amarra as operações
inseguras do Prelude. Há razões técnicas e psicológias pelas quais eu ainda não
a usei, e eu discuti elas, mas vou a usar no futuro quando ela se encaixar nas
minhas necessidades.


## Todo o código
Todo o código para a série estará [nesse repositório do GitHub](https://github.com/FranklinChen/twenty-four-days2015-of-hackage).

- - -

### Nota do tradutor
Se você quer ajudar com esse tipo de coisa, agora é a hora. Entre no
[Slack](http://haskellbr.com/slack/) ou no
[IRC](http://irc.lc/freenode/haskell-br) da [HaskellBR](http://haskellbr.com/) e
contribua. Esse blog e outros projetos associados estão na
[organização `haskellbr` no GitHub](https://github.com/haskellbr) e em
[haskellbr.com/git](http://haskellbr.com/git).

[Há um milestone no GitHub com tarefas esperando por você.](https://github.com/haskellbr/blog/milestones/24%20dias%20de%20Hackage%202015).
