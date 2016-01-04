---
layout: post
title: "24 dias de Hackage, 2015 - dia 23 - Liquid Haskell: Refinamento de tipos no mundo real"
author: Franklin Chen
author_url: "http://conscientiousprogrammer.com/"
translator: Pedro Yamada
translator_url: "https://github.com/yamadapc"
excerpt_separator: "<!-- more -->"
---

_Esse é um artigo escrito por Franklin Chen e traduzido para o português.
[Ler original.](http://conscientiousprogrammer.com/blog/2015/12/23/24-days-of-hackage-2015-day-23-liquid-haskell-refinement-types-for-the-real-world/)_

## Índice de toda a série
O índice de toda a série está no topo do artigo para o [dia 1](/2015/12/08/aperitivos-de-haskell-24-dias-de-hackage-2015-dia-1-introducao-e-stack.html).

## Encontro HaskellBR São Paulo
[Vamos nos encontrar em São Paulo em Janeiro de 2016. Marque sua presença e comente se não puder vir.](http://www.meetup.com/haskellbr-sp/events/227526368/)

[Vote se o encontro terá workshops, um dojo ou palestras.](https://plus.google.com/+DanielYokomizo/posts/bM1C5HRLcR4)

# Dia 23
([Discussão do original no Reddit](https://www.reddit.com/r/haskell/comments/3y00mx/24_days_of_hackage_2015_day_23_liquid_haskell/))

Ao longo dessa série de posts "24 dias de Hackage, 2015", eu me esforcei para
dar um gostinho de usar o QuickCheck para especificar contratos que deveriam
ser satisfeitos pelo nosso código que não são checados a tempo de compilação
mas por testes aleatórios durante runtime. Mas nós sabemos que só porque um
pedaço de código tem centenas ou milhares de testes, isso não é uma *prova* de
que o código *está correto de fato para todos os casos possíveis*. Eu também
usei o `newtype` como uma forma de indicar *a intenção* de que eu queria
restringir os valores legais para um tipo, como
[no dia 15](/2015/12/23/24-dias-de-hackage-2015-dia-15-iospec-testando-io-e-algumas-dicas-para-o-quickcheck.html)
quando eu laboriosamente tentei *refinar* tipos existentes sem nenhuma prova
automaticamente checável de que estava os populando corretamente.

<!-- more -->

{% highlight haskell %}
-- | User input without a newline, and not equal to "secret".
newtype NotSecretString =
  NotSecretString { getNotSecretString :: NotNewlineString }

type NotNewlineString = [NotNewlineChar]

newtype NotNewlineChar =
  NotNewlineChar { getNotNewlineChar :: Char }
  deriving (Show)
{% endhighlight %}

Por isso estou muito animado quanto ao
[Liquid Haskell](http://goto.ucsd.edu/~rjhala/liquid/haskell/blog/about/),
um sistema de análise estática para Haskell que está sob desenvolvimento
ativo. Ele expõe um sistema de *refinamento de tipos* que permite que você
escreva, de forma clara e natural, algumas variedades de contratos que podem
ser verificadas *em tempo de compilação*. Acredito que esse tipo de sistema vai
ser um *game changer* no futuro das linguagens de programação.

## O que é um tipo refinado?
Um tipo refinado é um tipo com algum predicado que "refina" seus valores
permitidos possíveis. No Liquid Haskell, você pode adicionar anotações de
refinamento ao código Haskell, *embedados em comentários normais*, e também
escrever seus próprios predicados. É ótimo que o Liquid Haskell possa ser
introduzido ao código existente sem grandes mudanças e sem requerir que um
preprocessador seja rodado antes do código ser compilado.

Como um exemplo (tirado de um demo oficial), abaixo temos uma lista associativa
chave-valor ordinária e então temos valores de dois refinamentos do tipo que
gostariamos de checar *em tempo de compilação* que os inteiros para as chave
são restritos da forma desejada.

{% highlight haskell %}
data AssocP k v = KVP [(k, v)]

{-@ digitsP :: AssocP {v:Int | (Btwn 0 v 9)} String @-}
digitsP :: AssocP Int String
digitsP = KVP [ (1, "one")
              , (2, "two")
              , (3, "three") ]

{-@ sparseVecP :: AssocP {v:Int | (Btwn 0 v 1000)} Double @-}
sparseVecP :: AssocP Int Double
sparseVecP = KVP [ (12 ,  34.1 )
                 , (92 , 902.83)
                 , (451,   2.95)
                 , (877,   3.1 )]

{-@ predicate Btwn Lo V Hi = (Lo <= V && V <= Hi) @-}
{% endhighlight %}

Algo a notar sobre tipos refinados é que uma expressão pode ter mais de um
refinamento definido. Por exemplo, um `Int` refinado para estar entre 0 e 9 é,
claro, também um `Int` refinado para estar entre 0 e 20.

Você pode usar refinamentos para pre-condições e pós-condições e refinar os
tipos de funções.

## Ótimo demo interativo no site!

Visite o [site do demo interativo do Liquid Haskell](http://goto.ucsd.edu:8090/index.html)!

Porque um demo excelente já existe, não vou caminhar pelos exemplos de uso do
Liquid Haskell, apesar de que posso olhar para o código antigo e começar a
adicionar seu uso.

O site de demo tem dúzias de exemplos completos, embedados em um ambiente
interativo no qual você pode mudar o código e rodar o verificador estático para
ver o que acontece. Exemplos incluem definir um tipo refinado de uma lista
ordenada, de forma que você possa escrever uma função de ordenação com tipo
`(Ord a) => [a] -> IncrList a` onde o tipo requere que a função seja *provada
pelo verificador* e que sua função de sort de fato funcione.

## Como tipos refinados funcionam?
Liquid Haskell funciona partindo de refinamentos para uma coleção de restrições
lógicas cuja verificação é tercerizada para um *solver* tirado da estante
[SMT](https://en.wikipedia.org/wiki/Satisfiability_modulo_theories),
como o [Z3](https://github.com/Z3Prover/z3), que precisa estar instalado.
(No Mac OS X, instalo o Z3 com `brew install z3`.)

Tenho o Liquid Haskell instalado globalmente usando o Stack:

{% highlight console %}
$ stack install liquidhaskell intern liquid-fixpoint
{% endhighlight %}

Isso instala o executável `liquid` que você roda.

## Tutoriais, livros, blog, grupo de discussão

Um livro grátis e extensivo,
["Programming with Refinement Types: An Introduction to Liquid Haskell"](http://ucsd-progsys.github.io/liquidhaskell-tutorial/),
está disponível tanto como PDF e como uma versão HTML linda e legível.

O
[repositório do Liquid Haskell no GitHub](https://github.com/ucsd-progsys/liquidhaskell),
com um monte de atividade.

O
[blog do Liquid Haskell](http://goto.ucsd.edu/~rjhala/liquid/haskell/blog/).

A
[lista de e-mails do Google groups](https://groups.google.com/forum/#!forum/liquidhaskell).

## Videos

O time do Liquid Haskell tem sido muito ativo em dar palestras sobre seu
trabalho. Aqui estão algumas:

### [Ranjit Jhala](https://ranjitjhala.github.io/), "Refinement Types for Haskell"

<iframe width="420" height="315" src="https://www.youtube.com/embed/vYh27zz9530" frameborder="0" allowfullscreen></iframe>

### [Eric Seidel](http://gridaphobe.github.io/), "Refinement Types for the Real World"

<iframe width="560" height="315"
src="https://www.youtube.com/embed/vqvNQixKr6w" frameborder="0"
allowfullscreen></iframe>

### [Niki Vazou](http://goto.ucsd.edu/~nvazou/), "Bounded Refinement Types"

<iframe width="560" height="315" src="https://www.youtube.com/embed/nd3buP97Ryw" frameborder="0" allowfullscreen></iframe>

O paper recente sobre esse trabalho está
[aqui](http://goto.ucsd.edu/~nvazou/icfp15/main.pdf).

## Uma nota sobre tipos dependentes
Você pode se perguntar qual é a relação entre tipos refinados e um sistema de
tipos dependentes completo. Eu vejo ambos como técnicas úteis. Com tipos
dependentes você pode escrever tipos que codifiquem todo o tipo de informação
(e eu estou interessado em usar mais tipos dependentes em 2016) , mas a
flexibilidade e sensação de usar tipos refinados parece um ganho onde é natural
escrever predicados simples e trabalhar incrementalmente, refinando um tipo de
forma gradual.

## Conclusão
Estou animado quanto ao Liquid Haskell porque ele está trazendo refinamento de
tipos para o mundo real. Eu espero tentar os usar de fato em 2016.

## Todo o código
Todo o código para a série estará [nesse repositório do GitHub](https://github.com/FranklinChen/twenty-four-days2015-of-hackage).

- - -

### Nota do tradutor
Se você quer ajudar com esse tipo de coisa, agora é a hora. Entre no
[Slack](http://slack.haskellbr.com/) ou no
[IRC](http://irc.lc/freenode/haskell-br) da [HaskellBR](http://haskellbr.com/) e
contribua. Esse blog e outros projetos associados estão na
[organização `haskellbr` no GitHub](https://github.com/haskellbr) e em
[haskellbr.com/git](http://haskellbr.com/git).

[Há um milestone no GitHub com tarefas esperando por você.](https://github.com/haskellbr/blog/milestones/24%20dias%20de%20Hackage%202015).
