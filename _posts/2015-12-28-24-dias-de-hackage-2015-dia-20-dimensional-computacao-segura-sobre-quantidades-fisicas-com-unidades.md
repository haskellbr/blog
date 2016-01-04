---
layout: post
title: "24 dias de Hackage, 2015 - dia 20 - dimensional: Computação segura sobre quantidades físicas com unidades"
author: Franklin Chen
author_url: "http://conscientiousprogrammer.com/"
translator: Pedro Yamada
translator_url: "https://github.com/yamadapc"
excerpt_separator: "<!-- more -->"
---
_Esse é um artigo escrito por Franklin Chen e traduzido para o português.
[Ler original.](http://conscientiousprogrammer.com/blog/2015/12/20/24-days-of-hackage-2015-day-20-dimensional-type-checked-computation-on-physical-quantities-with-units/)_

## Índice de toda a série
O índice de toda a série está no topo do artigo para o [dia 1](/2015/12/08/aperitivos-de-haskell-24-dias-de-hackage-2015-dia-1-introducao-e-stack.html).

# Dia 20

([Discussão no Reddit](https://www.reddit.com/r/haskell/comments/3xoe58/24_days_of_hackage_2015_day_20_dimensional/))

Uma das coisas que os professores de ciência no ensino-médio sempre
enfatizavam, quando estávamos calculando algo, era *ser explícitos sobre as
unidades de medida*. Eles eram rigorosos em querer que nós mostrássemos as
unidades, as carregassemos e cancelássemos quando apropriado, para extrair uma
resposta final que fizesse sentido. E por bons motivos! Se você adicionar uma
distância (como um número de metros) e uma velocidade (como kilometros por
segundo), isso é um *erro de tipo*. Também é um erro de tipo se você adicionar
o valor numérico de uma distância em metros ao valor de uma distância em pés:
para fazer essa adição, você teria que as converter para uma unidade comum e,
então, adicionar os números.

A linguagem de programação F# vem com
[unidades de medida](https://msdn.microsoft.com/en-us/library/dd233243.aspx)
inclusas no seu sistema de tipos. Isso é uma funcionalidade muito legal. Como
podemos fazer isso em Haskell?

<!-- more -->

Acontece que com vários mecânismos de tipos sofisticados, fizeram esse tipo de
coisa em Haskell. A biblioteca que eu conferi para fazer checagem de unidades
por meio de tipos para quantidades físicas é a biblioteca
[`dimensional`](http://hackage.haskell.org/package/dimensional)
que está em desenvolvimento ativo.

Hoje vou mostrar um pouco de código que a usa para dar um gostinho do que pode
ser feito com ela.

## Instalação
Já que o `dimensional` está se movendo tão rápido, eu não uso a versão antiga
que está no Stackage LTS atual. Eu adicionei ao meu `stack.yaml`:

{% highlight yaml %}
- dimensional-1.0.1.1
- exact-pi-0.4.1.0
- numtype-dk-0.5
{% endhighlight %}

`dimensional` se move tão rápido que o _branch_ master
[no GitHub](https://github.com/bjornbm/dimensional) já passou mesmo dessa
versão.

## Tarefa de Exemplo
Como um corredor, as vezes quero realizar previsões e projeções das minhas
metas, para calcular várias quantidades como qual é o ritmo para terminar uma
corrida em um certo tempo, então decidi brincar com o `dimensional` para
expressar cálculos simples.

Dada uma meta de tempo para 5KM e um ritmo de corrida alvo (180 passos por
minuto), eu calculo qual é o comprimento do passo necessário.

## Um teste HSpec

Imports:

{% highlight haskell %}
module DimensionalExampleSpec where

import DimensionalExample (requiredStrideLength)

import Prelude hiding ((+))
import Numeric.Units.Dimensional.Prelude
       ( (*~), (/~)
       , (+)
       , Length, Time, kilo, meter, minute, second
       )
import Numeric.Units.Dimensional.NonSI (foot)

import Test.Hspec (Spec, hspec, describe, it, shouldSatisfy)
{% endhighlight %}

Note que eu provi imports explícitos para
`Numeric.Units.Dimensional.Prelude`. Na realidade, se eu estivesse usando o
`dimensional` para uma quantidade séria de código, eu cairia de cabeça e
adimitiria que estamos trabalhando com uma linguagem de domínio específico para
a aritmética que justifica esconder o `Prelude` e usar tudo que o `Prelude` do
`dimensional` expõe, unidades comuns, tipos para quantidades e operadores
aritméticos com _overloading_.

O teste só verifica que eu não tenho que ter um comprimento do passo maior que
4 pés mas que ele precisa ser maior que 3 pés:

{% highlight haskell %}
spec :: Spec
spec =
  describe "dimensional" $ do
    it "check required running stride length" $
      let fiveK :: Length Double
          fiveK = 5 *~ kilo meter

          goalTime :: Time Double
          goalTime = 24 *~ minute + 45 *~ second

          feetPerStep :: Double
          feetPerStep = requiredStrideLength fiveK goalTime /~ foot
      in feetPerStep `shouldSatisfy` (\x -> x > 3 && x < 4)
{% endhighlight %}

Note que todas as quantidades são parametrizadas pelo tipo numérico envolvido,
e.g., `Length a`. Os operadores com `~` combinam valores com uma unidade para
retornar uma quantidade completa. Você pode multiplicar por unidades, dividir
por elas (ao dividir por `foot` recebemos o `Double` a partir do `Length
Double` acima). Os operadores normais como `+` operam sobre as quantidades.

## Implementação

{% highlight haskell %}
module DimensionalExample where

import Prelude hiding ((/))
import Numeric.Units.Dimensional.Prelude
       ( (*~)
       , (/)
       , Quantity, Recip, DTime, Length, Time
       , one, minute
       )

-- | "Ideal" turnover for steps while running is 180 steps per minute.
turnover :: Quantity (Recip DTime) Double
turnover = (180 *~ one) / (1 *~ minute)

requiredStrideLength
  :: Length Double
  -> Time Double
  -> Length Double
requiredStrideLength distance goalTime =
  distance / goalTime / turnover
{% endhighlight %}

Aqui `one` é usado para criar uma quantidade sem dimenção. Por baixo dos panos,
a biblioteca usa _type families_ para representar a divisão por unidades e
assim por diante.

## Uma preocupação sobre usabilidade
Infelizmente, como você pode esperar de uma biblioteca que usa muita
infraestrutura e computação ao nível dos tipos, definitivamente há uma curva de
aprendizado para aprender a fundação conceitual dessa biblioteca, apesar de a
documentação ser muito boa. A pior coisa é que por causa de sinônimos de tipo
em combinação com todas as mágicas a nível de tipo, as mensagens de erro podem
ficar bem estranhas se você fizer algo que não faz sentido. Mesmo se você fizer
algo que faz sentido, pode ficar confuso. Por exemplo, imagine que nós não
soubessemos como escrever a assinatura de `turnover` acima. O tipo inferido é:

{% highlight haskell %}
    Top-level binding with no type signature:
      turnover :: dimensional-1.0.1.0:Numeric.Units.Dimensional.Internal.Dimensional
                    'Numeric.Units.Dimensional.Variants.DQuantity
                    ('Numeric.Units.Dimensional.Dimensions.TypeLevel.Dim
                       'numtype-dk-0.5:Numeric.NumType.DK.Integers.Zero
                       'numtype-dk-0.5:Numeric.NumType.DK.Integers.Zero
                       'numtype-dk-0.5:Numeric.NumType.DK.Integers.Neg1
                       'numtype-dk-0.5:Numeric.NumType.DK.Integers.Zero
                       'numtype-dk-0.5:Numeric.NumType.DK.Integers.Zero
                       'numtype-dk-0.5:Numeric.NumType.DK.Integers.Zero
                       'numtype-dk-0.5:Numeric.NumType.DK.Integers.Zero)
                    Double
{% endhighlight %}

Ouch! Isso é o que eu recebi pela primeira vez quando escrevi o código sem
anotações de tipo explícitas. Eu tive que mergulhar na biblioteca para entender
o que estava acontecendo para encontrar os sinônimos de tipo que expressávam
minha intenção.

Eu não sei como resolver o problema genérico de bibliotecas que usam muitos
tipos resultando em uma grande curva de aprendizado e _pegadinhas_, mas acho
que há mais e mais tipos complexos sendo usados em bibliotecas para Haskell, e
algo precisa ser feito. Acho difícil que um cientista que mal sabe Haskell e
cujo trabalho é escrever código seguro e correto usaria uma biblioteca como
`dimensional` hoje, independente do quão legal ela seja. Isso dito, eu estou
animado que o `dimensional` existe e que está em desenvolvimento ativo!

## Exemplos reais

Doug Burke usa o `dimensional` nos seus
[notebooks de astronomia do IHaskell](https://github.com/DougBurke/astro-haskell),
por exemplo [este](https://github.com/DougBurke/astro-haskell/blob/master/notebooks/angular%20diameter%20distance.ipynb).

## Conclusão
`dimensional` é uma biblioteca interessante para explicitar unidades e
verificar estáticamente cálculos envolvendo quantidades físicas e uma boa
mostra de como tipos podem ser usados para melhorar a forma de expressar e
verificar a intenção de um programa.

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
