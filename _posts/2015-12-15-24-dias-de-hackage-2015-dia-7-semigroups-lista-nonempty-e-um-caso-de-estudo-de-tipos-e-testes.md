---
layout: post
title: 24 dias de Hackage, 2015 - dia 7 - semigroups; lista NonEmpty e um caso de estudo de tipos e testes
author: Franklin Chen
author_url: "http://conscientiousprogrammer.com/"
translator: Pedro Yamada
translator_url: "https://github.com/yamadapc"
---

_Esse é um artigo escrito por Franklin Chen e traduzido para o português.
[Ler original](http://conscientiousprogrammer.com/blog/2015/12/07/24-days-of-hackage-2015-day-7-semigroups-nonempty-list-and-a-case-study-of-types-and-tests/)_

# Dia 7
O quão frequentemente o seguinte erro aconteceu para você, seja em Haskell ou
em alguma outra linguagem?

{% highlight console %}
*** Exception: Prelude.head: empty list
{% endhighlight %}

Basicamente, o código explodiu ao assumir que uma lista vazia tinha elementos.

De fato, uma pergunta
[foi postada no reddit recentemente](https://www.reddit.com/r/haskell/comments/3vlb8v/reading_data_problems/)
sobre código que falhava com:

{% highlight console %}
** Exception: Prelude.foldl1: empty list
{% endhighlight %}

E é o mesmo problema.

Hoje, apresento um estudo de caso: refatorar o código da pergunta para tirar
vantagem de um tipo de dados importante que fará parte da biblioteca padrão de
Haskell no futuro. O tipo `NonEmpty` para listas. Ele é parte do pacote
[`semigroups`](http://hackage.haskell.org/package/semigroups)
do Edward Kmett que
[vai fazer parte da biblioteca padrão](https://prime.haskell.org/wiki/Libraries/Proposals/SemigroupMonoid).

- - -

## De maneira geral, deveríamos adotar programação defensiva ou "confiante"?

É fácil dizer: "bom, nós deveríamos programar *defensivamente* e não chamar
funções inseguiras como `head` ou `foldl1` e sempre checar por uma lista
vazia". Mas e se *soubermos* que a lista deve ter pelo menos um elemento,
talvez porque validamos esse fato em outro ponto do nosso modelo de dados? Por
exemplo, digamos que você está comprando ingressos para um show. Você tem que
comprar pelo menos um, mas poderia comprar mais de um. As regras de negócio
para a retirada de ingressos podem assumir que há pelo menos um ingresso para
você. Elas não precisam ficar verificando se a lista está vazia, porque isso
deveria ter sido pego de cara quando você fez o pedido.

A situação é análoga ao problema do `NULL` em muitas linguagens: linguagens que
usam `NULL` não diferenciam ao nível dos *tipos* entre algo *possivelmente
inexistente* (0 ou 1 elementos) e algo que *sempre* está lá (1 elemento). É
infeliz quando nós sabemos algo sobre nossos dados mas não o dizemos nos
tipos. No caso das listas, a situação é que uma lista *possivelmente vazia*
pode ter 0 ou mais elementos, enquando uma lista *não vazia* pode ter 1 ou mais
elementos. As duas situações são extremamente comuns e deveriam ser modeladas
por tipos diferentes (pense em como regexes distinguem entre `x*` e `x+`)!

Eu acredito que a solução real para esse tipo de problema é não ser *defensivo*
e sujar todo o código com validações durante o runtime de `NULL` ou
`isEmpty`. A solução também não é só bancar o "hacker cowboy" e convidar
possíveis e reais erros pulando toda a validação.

Ao invés disso, sempre que prática, é *usar os tipos certos* para que dentro de
um certo escopo apropriado do código, erros de runtime *não possam acontecer* e
com isso dentro desse escopo nós possamos programar *confiantemente* e não
*defensivamente*. Se não usarmos o _type system_ da nossa linguagem para nosso
próprio bem, só estamos fazendo programação dinamicamente tipada tradicional em
uma linguagem tipada e perdendo todos os benefícios dos tipos. Já que fizemos
sacrifícios adotando uma linguagem com tipos estáticos sobre uma linguagem com
tipos dinâmicos, deveríamos tomar vantagem do contexto em que nos metemos.

Há uma assimetria interessante no munda da programação: é *impossível* escrever
código com tipos estáticos em uma linguagem com tipos dinâmicos, mas é fácil
escrever código com tipos dinâmicos em uma linguagem com tipos estáticos!

## O tipo de listas não vazias
Vamos olhar para a lista `NonEmpty`. Você pode a usar hoje antes que ela vire
parte da biblioteca padrão, basta adicionar `semigroups` às suas dependências.

### Quais são os requisítos para uma lista não vazia?
A grosso modo, uma lista `NonEmpty` deve:

- Suportar operações de conversão com uma lista normal *possivelmente vazia*
- Suportar operações análogas às operações padrões sobre listas (como `map` e
  `filter`) que levam em consideração se a saída é possivelmente vazia

### QuickCheck para especificar leis (propriedades)
No
[dia 3](/2015/12/10/24-dias-de-hackage-2015-dia-3-hspec-a-importancia-de-testes.html),
mencionei que o QuickCheck é muito útil para especificar requisitos quando
desenhando um módulo novo. Idealmente, quando estamos desenhando uma
API ao redor de um tipo, nós o tratamos como um tipo abstrato e verificamos o
comportamento de operações sobre ele. A lista `NonEmpty` é simples o suficiente
que você pode pensar que é overkill fazer isso, mas eu gostaria de apresentar
um gostinho do que é possível ser feito com desenvolvimento guiado a testes e
propriedades.

Vamos imaginar que estamos criando um módulo `Data.List.NonEmpty`:

{% highlight haskell %}
{-# LANGUAGE ScopedTypeVariables #-}

-- Parte de um módulo de testes hipotético para o pacote semigroups
module HypotheticalSemigroupsSpec where

import qualified Data.List.NonEmpty as NonEmpty

import Test.Hspec (Spec, hspec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import qualified Test.QuickCheck as QuickCheck
import qualified Data.Maybe as Maybe

spec :: Spec
spec =
  describe "semigroups" $ do
    describe "Data.List.NonEmpty" $ do
      describe "o construtor NonEmpty.nonEmpty" $ do
        it "falha quando tentamos o contruir a partir de uma lista vazia" $ do
          NonEmpty.nonEmpty ([] :: [Int]) `shouldBe` Nothing
        prop "sucede com qualquer lista não vazia" $ do
          \(QuickCheck.NonEmpty (xs :: [Int])) ->
            NonEmpty.nonEmpty xs `shouldSatisfy` Maybe.isJust
      describe "a conversão para uma lista regular" $ do
        prop "converte de volta para a lista regular original" $ do
          \(QuickCheck.NonEmpty (xs :: [Int])) ->
            let Just nonEmptyXs = NonEmpty.nonEmpty xs
            in NonEmpty.toList nonEmptyXs `shouldBe` xs
{% endhighlight %}

Basicamente, `nonEmpty` é um construtor seguro:

{% highlight haskell %}
nonEmpty :: [a] -> Maybe (NonEmpty a)
{% endhighlight %}

Também há um construtor não seguro chamado `fromList`. Não o usaria exceto se
já souber que uma lista não está vazia, porque ela foi retornada por uma API
que garantiu isso mas não de forma tipada. Por exemplo, eu dei de cara com esse
problema escrevendo parsers com bibliotecas como o
[`parsec`](https://hackage.haskell.org/package/parsec), porque
combinadores como o `many1` tem tipo:

{% highlight haskell %}
many1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m [a]
-- N.T. Há uma breve nota sobre esse tipo abaixo
{% endhighlight %}

Apesar de que a biblioteca *garante* que se um parse for bem sucedido, a lista
resultante terá pelo menos um elemento!

Em uma situação como essa, teria uma justificativa para usar o
`NonEmpty.fromList` inseguro para *imediatamente* botar minha lista, que sei
ser _não vazia_, em um tipo mais refinado.

- - -

#### Nota do tradutor extendida
Não cabe discutir parser-combinators nesse artigo, mas o comentário do autor
quer dizer que dentro do contexto do `ParsecT`, o `many1` recebe um valor que
representa um "parser" para algo com tipo `a` e retorna um parser de uma lista
de `a` que deve ter pelo menos um elemento.

É o equivalente de uma regex `(algumGrupo)+` que tenta dar match de
`algumGrupo` uma ou mais vezes. Seu uso simplificado com
pseudo-código poderia ser:

{% highlight haskell %}
parseFunctionName :: Parser [Char]
parseFunctionName = many1 parseCharacter
  where
    parseCharacter :: Parser Char
    parseCharacter = undefined
{% endhighlight %}

- - -

_[N.T. De volta ao texto]_

Uma nota sobre o funcionamento interno do QuickCheck: `QuickCheck.NonEmpty`
é só um
[newtype no QuickCheck](https://hackage.haskell.org/package/QuickCheck-2.8.1/docs/Test-QuickCheck.html#t:NonEmptyList)
para gerar listas normais `[a]` que não estão vazias durante o runtime (todas
tem forma `(x:xs)`). Ignore a coincidência do token `NonEmpty` em comum com o
módulo e tipo do `semigroups`. Imagine se o QuickCheck tivesse sido escrito
quando o tipo `NonEmpty` que já existisse. Nesse caso ele poderia só gerar um
`NonEmpty` real ao invés de usar um `newtype` feito só para a geração!

Se você não usa o QuickCheck ou bibliotecas geradoras de testes similares,
aprenda mais sobre elas! Esses métodos de testes são muito mais úteis do que os
testes baseados em exemplos que eu mostrei até agora nesses artigos com
propósitos ilustrativos. Quando possível, testes auto-gerados devem ser
escritos ao invés de testes manuais baseados em exemplos.

(O uso de `[Int]` e anotações de tipos é porque o QuickCheck requere um tipo
monomórfico para gerar dados de teste concretos. `ScopedTypeVariables` é uma
extensão do GHC que deveria ser parte linguagem Haskell padrão, para mim; ela
foi coberta
[em um Dia de Extensões do GHC de 2014](https://ocharles.org.uk/blog/guest-posts/2014-12-20-scoped-type-variables.html).)

### (Update de 2015-12-14) Usando `NonEmpty` para um parser
No **dia 14** eu usei o `NonEmpty` para representar de forma fundamentada o
resultado de um parser.

_[N.T. Post ainda não traduzido, o link real será adicionado com o tempo; veja [o artigo original para o dia 14](http://conscientiousprogrammer.com/blog/2015/12/14/24-days-of-hackage-2015-day-14-earley-a-promising-newer-parser-library-for-haskell/)]_,

## Algumas notas na API completa do `NonEmpty`
O `map` se comporta como esperado, porque ele não precisa mudar o número de
elementos e portanto iterar sobre um `NonEmpty` resulta claramente em outro
`NonEmpty`:

{% highlight haskell %}
map :: (a -> b) -> NonEmpty a -> NonEmpty b
{% endhighlight %}

Entretanto, um `filter` sobre um `NonEmpty` retorna uma lista normal
possívelmente vazia, como deveria, porque o predicado pode falhar em todos os
elementos:

{% highlight haskell %}
filter :: (a -> Bool) -> NonEmpty a -> [a]
{% endhighlight %}

E `foldl1`, diferente do `Prelude.foldl1` para listas regulares, é
*seguro*. Ele não pode falhar, porque ele sempre pode começar a dobra com
usando o primeiro elemento como o valor inicial.

Há um monte de outras funções úteis para listas.

Ah, e a
[implementação interna](http://hackage.haskell.org/package/semigroups-0.18.0.1/docs/src/Data-List-NonEmpty.html#NonEmpty)
é o que você deve suspeitar: é só uma tupla do primeiro elemento com o resto
(tail) possivemente vazio e um operador "cons" especial `:|` que tenta parecer
com o operador normal `:` sobre listas.

{% highlight haskell %}
data NonEmpty a = a :| [a]
{% endhighlight %}

## Um caso de estudos em refatoração
Vamos tentar refatorar um pouco
[de código postado no Reddit](https://www.reddit.com/r/haskell/comments/3vlb8v/reading_data_problems/)
que estava atirando uma exceção inesperada durante o runtime.

Não vamos entrar em como o código pode ser escrito de forma completamente
diferente, mas só nos concentrar em identificar e remover o código inseguro que
atira exceções.

O código usa o pacote excelente
[`split`](http://hackage.haskell.org/package/split)
do qual eu sou um usuário feliz. Eu tomei a liberdade de adicionar comentários
ao código e imports explícitos.

### Código original inseguro
{% highlight haskell %}
import qualified Data.List.Split as Split

totalArea :: [(Int, Int, Int)] -> Int
totalArea xs = foldl (\acc x -> (acc + partialArea x)) 0 xs

partialArea :: (Int, Int, Int) -> Int
partialArea (l, w, h) = 2 * (l*w + w*h + h*l) + slack
  where areas       = [l, w, h]

        -- 'maximum' é inseguro
        smallSides  = filter (< maximum areas) areas

        -- 'foldl1' é inseguro
        slack       = foldl1 (*) smallSides

parseFile :: String -> [(Int, Int, Int)]
parseFile xs = map (splitDimensions) (breakLines xs)

breakLines :: String -> [String]
breakLines = Split.splitOn "\n"

-- | 'read' é inseguro. '(!!)' é inseguro.
splitDimensions :: String -> (Int, Int, Int)
splitDimensions xs = (item 0, item 1, item 2)
                   where item n = read ((Split.splitOn "x" xs)!!n)
{% endhighlight %}

Isso quebra no `foldl1`:
{% highlight haskell %}
spec :: Spec
spec =
  describe "UnsafeListExample" $ do
    it "totalArea quebra para um certo input" $ do
      let contents = "1 x 1 x 1"
      evaluate (totalArea (parseFile contents)) `shouldThrow` anyException
{% endhighlight %}

Já identifiquei os problemas acima.

Vamos ignorar o fato que `read` e acessar elementos de uma lista `(!!)` são
inseguros, porque isso não é interessante para hoje.

Mas `maximum` e `foldl1` são ambos inseguros sobre listas, porque quebram com
listas vazias.

Acaba que *a partir da inspeção manual, agindo como uma ferramenta de análise
estática humana* o uso de `maximum` é OK aqui, porque ele está operando sobre
uma lista de 3 elementos que foi criada logo antes de ser passada para o
`maximum`. Mas essa segurança não é refletida nos tipos.

E quanto ao `foldl1`? Se você pensar cuidadosamente no que o `filter` retorna,
pode deduzir que ele pode retornar uma lista vazia e se você não puder garantir
que que ela não é vazia, o `foldl1` vai quebrar feio. E ele quebra.

Isso foi um monte de raciocínio desperdiçado. Felizmente, alguns testes rodados
descobriram o bug, mas ainda assim, o código acabou sendo postado no Reddit,
perguntando o qual era o bug e não é fácil encontrar esse tipo de problema. Há
uma forma melhor?

### Refatorando com listas `NonEmpty`
Vamos usar `NonEmpty`. Antes de mais nada, nós mudamos `totalArea` para receber
um `NonEmpty` já que é isso que nós queremos de fato.

{% highlight haskell %}
totalArea :: NonEmpty (Int, Int, Int) -> Int
totalArea xs = foldl (\acc x -> (acc + partialArea x)) 0 xs
{% endhighlight %}

Note que nós só tivemos que mudar o tipo do parâmetro e mais nenhum outro
código, porque `foldl` foi implementado (genericamente) para `NonEmpty` e
listas normais.

Agora, mudamos a definição de `partialArea`:

{% highlight haskell %}
partialArea :: (Int, Int, Int) -> Int
partialArea (l, w, h) = 2 * (l*w + w*h + h*l) + slack
  where
    areas :: NonEmpty Int
    areas = NonEmpty.fromList [l, w, h]

    -- 'maximum' é seguro sobre um 'NonEmpty'
    -- Mas 'smallSides' pode ser vazio por causa do 'NonEmpty.filter',
    -- e 'NonEmpty.fromList' é inseguro!
    smallSides :: [Int]
    smallSides = NonEmpty.filter (< maximum areas) areas

    -- inseguro!
    smallSides1 :: NonEmpty Int
    smallSides1 = NonEmpty.fromList smallSides

    -- 'foldl1' é seguro sobre um 'NonEmpty'
    slack = foldl1 (*) smallSides1
{% endhighlight %}

Nós fizemos várias mudanças:

- Já que estamos criando uma lista não-vazia, nós transformamos ela
  imediatamente em uma lista `NonEmpty`
- `maximum` é definido (genericamente) para `NonEmpty` então nós não precisamos
  mudar o código, mas nós sabemos que o `maximum` não pode falhar para o
  `NonEmpty`
- `NonEmpty.filter`, como discutido, retorna uma lista normal, *não* um
  `NonEmpty`
- No final, nós queremos computar `slack` de forma segura, mas nós não podemos
  fazer isso a não ser que usemos um `foldl1` seguro
- De trás para frente, nós precisamos converter uma lista normal em um
  `NonEmpty`, mas só tem um jeito de fazer isso e ele é inseguro!

Então, refinando os tipos usados no programa, nós identificamos o ponto exato
onde *nós não poderíamos escrever o código necessário* sem recorrer a
`NonEmpty.fromList`. Estritamente falando, nós não fizemos um erro de runtime
virar um erro de compilação, mas nós restringimos o que estava acontecendo de
forma mecânica só seguindo os tipos.

O problema real nesse código parece ser que nós queremos que `smallSides` seja
não vazio. A tentativa de usar só código seguro automaticamente resolveu uma
causa de insegurança em potencial (`maximum`) e apontou para uma situação mais
complicada que requere uma asserção contra o resultado do
`filter`. `partialArea` poderia ser escrito para evitar a asserção, se não
usasse listas normais em nenhum lugar para `areas`.

### Talvez o problema seja que nós não devessemos usar listas normais aqui
Você pode ter protestado sobre esse exercício todo esse tempo, porque
`NonEmpty` simplesmente é o tipo errado para todo esse trabalho!  **Não há
nenhuma razão para usar um tipo sofisticado que não se encaixa no problema.**
Não havia nenhum motivo para que a lista `[l, w, h]` fosse criada em primeiro
lugar. Se você está usando um tipo e você ainda acaba tendo que realizar
asserções sobre ele, frequentemente isso significa que o tipo não é o certo.

Em `partialArea`, se expressarmos a lógica do que queríamos de fato (os dois
menores valores de três), podemos só escrever o que queremos dizer:

{% highlight haskell %}
-- | Pulando as listas completamente!
bestPartialArea :: (Int, Int, Int) -> Int
bestPartialArea (l, w, h) = 2 * (l*w + w*h + h*l) + slack
  where
    (side0, side1, _) = sort3 (l, w, h)
    slack = side0 * side1
{% endhighlight %}

Onde criamos um módulo utilitário `Sort3`:

{% highlight haskell %}
module Sort3 (sort3) where

-- | Ordene exatamente 3 valores
sort3 :: Ord a => (a, a, a) -> (a, a, a)
sort3 (a0, a1, a2) =
  if a0 > a1
  then if a0 > a2
       then if a2 < a1
               then (a2, a1, a0)
               else (a1, a2, a0)
       else (a1, a0, a2)
  else if a1 > a2
       then if a0 > a2
            then (a2, a0, a1)
            else (a0, a2, a1)
       else (a0, a1, a2)
{% endhighlight %}

Sim, eu pesquisei no Hoogle (como recomendado no meu artigo ontem sobre
pesquisar por módulos utilitários) e apesar de não achar essa função exata, eu
achei sua lógica no contexto de ordenar vetores de forma óptima no
[módulo `Data.Vector.Algorithms.Optimal`](https://hackage.haskell.org/package/vector-algorithms-0.7.0.1/docs/Data-Vector-Algorithms-Optimal.html)
do excelente
[pacote `vector-algorithms`](https://hackage.haskell.org/package/vector-algorithms)
que eu recomendo fortemente quando trabalhando com vetores. Eu copiei e colei a
lógica para trabalhar sobre uma tripla simples.

### O valor de testes
QuickCheck é uma ferramenta ótima. Suponha que nós não quisessemos ou
pudessemos refinar os tipos no nosso código, por algum motivo. Nós sempre
podemos usar testes para encontrar bugs fáceis de descobrir. Por exemplo, nós
poderíamos ter escrito um teste de sanidade sobre `totalArea` gerando um monte
de input randômico e verificando que o resultado é o que nós esperamos ou pelo
menos algo razoável:

{% highlight haskell %}
import UnsafeListExample (totalArea, parseFile)

import Test.Hspec (Spec, hspec, describe, it, shouldSatisfy, shouldThrow, anyException)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Positive(..))
import Control.Exception (evaluate)
import Text.Printf (printf)

spec :: Spec
spec =
  describe "UnsafeListExample" $ do
    it "totalArea quebra para algum input" $ do
      let contents = "1 x 1 x 1"
      evaluate (totalArea (parseFile contents)) `shouldThrow` anyException
    prop "totalArea retorna algo razoável para qualquer tripla de inteiros" $ do
      \(Positive (l :: Int)) (Positive (w :: Int)) (Positive (h :: Int)) ->
        let contents = printf "%d x %d x %d" l w h
        in totalArea (parseFile contents) `shouldSatisfy` (> 0)
{% endhighlight %}

Rapidamente nós temos um contra-exemplo reportado pelo QuickCheck:

{% highlight console %}
  1) UnsafeListExample totalArea does not crash on any triple of ints
       uncaught exception: ErrorCall (Prelude.foldl1: empty list) (after 4 tests)
       Positive {getPositive = 2}
       Positive {getPositive = 2}
       Positive {getPositive = 2}
{% endhighlight %}

Além disso, você pode ter se perguntado sobre aquela ordenação óptima
complicada de três valores. Será que eu copiei e colei a lógica corretamente?
Para ficar tranquilo eu escrevi um teste do QuickCheck para ela:

{% highlight haskell %}
spec :: Spec
spec =
  describe "Sort3" $ do
    prop "sort3 ordena corretamente" $ do
      \(triple :: (Int, Int, Int)) ->
        let (a0', a1', a2') = Sort3.sort3 triple
        in a0' <= a1' && a1' <= a2'
{% endhighlight %}

Eu amo o QuickCheck. O que seria da vida sem ele?

### Algumas refatorações bonus
Algumas outras refatorações orientadas a tipos convertendo para `NonEmpty` que
eu não vou discutir em detalhes porque elas foram irrelevantes aqui:

{% highlight haskell %}
parseFile :: NonEmpty Char -> NonEmpty (Int, Int, Int)
parseFile xs = NonEmpty.map (splitDimensions) (breakLines xs)

-- | We ended up not needing the fact that the input is nonempty, and
-- converted it to a regular list.
-- | Nós terminamos não precisando do fato de o input ser não vazio e
-- convertemos ele para uma lista normal
breakLines :: NonEmpty Char -> NonEmpty String
breakLines string1 = ourSplitOn "\n" (NonEmpty.toList string1)

-- | 'read' é inseguro. '(!!)' é inseguro.
splitDimensions :: String -> (Int, Int, Int)
splitDimensions xs = (item 0, item 1, item 2)
                   where item n = read ((Split.splitOn "x" xs)!!n)

-- | Usar a função insegura 'NonEmpty.fromList' é seguro porque sabemos que o
-- resultado de 'Split.splitOn' é não vazio. Note que os elementos do resultado
-- podem ser vazios
ourSplitOn :: Eq a => [a] -> [a] -> NonEmpty [a]
ourSplitOn subList list = NonEmpty.fromList (Split.splitOn subList list)
{% endhighlight %}

A coisa mais interessante foi a observação de que `Split.splitOn` sempre
retorna uma lista não vazia de listas. A princípio, poderiamos botar o
resultado em um `NonEmpty`. Eu até escrevi um pequeno teste do QuickCheck que
passa:

{% highlight haskell %}
spec :: Spec
spec =
  describe "split" $ do
    prop "splitOn sempre resulta em uma lista não vazia" $ do
      \subList (list :: String) ->
        Split.splitOn subList list `shouldSatisfy` not . null
{% endhighlight %}

Nota: o `split` já tem uma
[quantidade enorme de propriedades testadas](http://hub.darcs.net/byorgey/split/browse/test/). Eu
adoro a biblioteca `split`. Acho que é muito bem testada. Dê uma olhada.

## Uma breve nota sobre [`Semigroup`](http://hackage.haskell.org/package/semigroups-0.18.0.1/docs/Data-Semigroup.html) do semigroups

O `Semigroup` é uma _type-class_ representando uma estrutura algébrica que
requere que uma única operação associativa seja definida para ela: "append",
que a biblioteca expõe como um operador `<>`.

{% highlight haskell %}
import Data.Semigroup ((<>))
{% endhighlight %}

Um teste do QuickCheck verificando o que já sabemos: que `String` tem uma
instância de `Semigroup`, a concateção de strings, e que é associativa como
necessário:

{% highlight haskell %}
    describe "Data.Semigroup.Semigroup" $ do
      prop "<> é associativo para String" $ do
        \(x :: String) y z -> (x <> y) <> z `shouldBe` x <> (y <> z)
{% endhighlight %}

Você pode já usar esse operador `<>` com
[`Monoid`, que já faz parte da biblioteca padrão](https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Monoid.html),
mas a _type-class_ `Monoid` deveria ser uma sub-classe de `Semigroup` e isso é
o que vai acontecer em uma versão futura de Haskell (conceitualmente, isso
deveria estar presente desde o começo, mas Haskell foi inventado 25 anos atrás
em 1990 e `Semigroup` aparentemente não foi considerado importante o suficiente
para ser incluso na hierarquia de _type-classes_). A diferença é que um
`Monoid` também requere um elemento identidade `mempty`.

Não há espaço aqui para dizer qualquer coisa sobre porque semigroups ou monoids
são úteis para a computação. Monoids em particular tem virado uma palavra
diária em círculos de Big Data por causa do MapReduce, que se baseia em monoids
para performance.

_[N.T. É útil para se poder dividir as fases de um map ou reduce em muitos workers e combinar os resultados em uma segunda etapa, idependente da ordem na qual as etapas terminam; o mesmo se aplica para tentar paralelizar uma dobra ou map]_

Alguns recursos orientados a Haskell sobre monoids para conferir:

- [No Wikibooks](https://en.wikibooks.org/wiki/Haskell/Monoids)
  (O [Wikibook de Haskell](https://en.wikibooks.org/wiki/Haskell) é um bom recurso para Haskell em geral)
- Um artigo muito bom com código
  ["Gaussian distributions are monoids"](https://izbicki.me/blog/gausian-distributions-are-monoids)

## Conclusão
As coisas mais importantes de hoje: considere usar o `NonEmpty` quando você tem
uma lista que sabe ser não vazia, para que possa efetuar operações sobre ela
sem se preocupar com atirar um exceção. Só está há uma dependência do Cabal de
distância! Além disso, tenha certeza de que você queria uma lista em primeiro
lugar, e não uma tupla ou um vetor de tamanho fixo ou algo desse tipo. E use o
QuickCheck.

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
