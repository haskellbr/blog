---
layout: post
title: "24 dias de Hackage, 2015 - dia 11 - monad-loops: Refatorando para evitar escrever funções recursivas"
author: Franklin Chen
author_url: "http://conscientiousprogrammer.com/"
translator: Lucas Tadeu Teixeira
translator_url: "https://github.com/lucastadeu"
excerpt_separator: "<!-- more -->"
---
Esse é um artigo escrito por Franklin Chen e traduzido para o português.
[Ler original](http://conscientiousprogrammer.com/blog/2015/12/11/24-days-of-hackage-2015-day-11-monad-loops-avoiding-writing-recursive-functions-by-refactoring/)_

## Índice de toda a série
O índice de toda a série está no topo do artigo para o [dia 1](/2015/12/08/aperitivos-de-haskell-24-dias-de-hackage-2015-dia-1-introducao-e-stack.html).

- - -

## Dia 11

Desgosto, frequentemente, escrever funções recursivas. Recursão é uma forma
de `goto` e, portanto, recursão é (conceitualmente) a infraestrutura
baixo-nível de linguagem de montagem da programação funcional: um bloco
fundamental para construção mas que, não necessariamente, quero ver todos os
dias.

<!-- more -->

Uma analogia: programação imperativa nos anos 1960 foi alterada
significativamente quando novas estruturas de controle como laços `for` e
`while` modificaram sua aparência ao remover o _boilerplate_ de utilizar-se
a instrução de baixo-nível `goto` em todos os lugares; `for` e `while` são
as estruturas de controle de alta ordem da programação imperativa.

Em programação funcional, o _boilerplate_ da recursão é evitado por meio
da escrita e utilização de funções (de alta ordem) como `map`, `filter`,
`foldr` e `traverse`. A parte legal é que, em programação funcional,
essas são funções ordinárias disponíveis em espaço de usuário; já em
programação imperativa, estruturas de controle são embutidas na linguagem
e, geralmente, não é tão simples criar suas próprias estruturas.

Por que você gostaria de definir suas próprias estruturas de controle?
Vou mostrar alguns exemplos e uma pequena biblioteca muito útil, `monad-loops`,
que você pode usar para evitar reinventar a roda.

## Um exemplo de tarefa

Vamos escrever uma função que simula o processo de _login_ de um usuário, no qual

- é requisitada senha do usuário
- um laço é iniciado, no qual
    - a tentativa de senha é lida
    - se não estiver correta, a senha é requisitada novamente e reiniciamos o laço
    - se estiver correta, saímos do laço
    - imprimimos parabéns

Abaixo está a tradução literal do pseudocódigo para Haskell, usando recursão para
o laço:

{% highlight haskell %}
logIn :: IO ()
logIn = do
  putStrLn "% Digite a senha:"
  go
  putStrLn "$ Parabéns!"

  where
    -- Utilização de recursão para o laço
    go = do
      guess <- getLine
      if guess /= "segredo"
        then do
          putStrLn "% Senha incorreta!"
          putStrLn "% Tente novamente:"
          go
        else
          return ()
{% endhighlight %}

Você gosta deste código? Eu me irrito com o fato de precisar escrever uma
função recursiva apenas para fazer um laço de repetição.


### Comparação com típico estilo imperativo


Compare com um código Python típico:

{% highlight Python %}
def log_in():
  print "% Digite a senha"
  while raw_input() != 'segredo:
    print "% Senha incorreta!"
    print "% Tente novamente:"
  print "% Parabéns!"
{% endhighlight %}

Há um motivo pelo qual [laços de repetição
`while`](https://en.wikipedia.org/wiki/While_loop) foram criados nos anos 1960
para programação imperativa. Era um modo de melhorar a expressão de
_padrões de fluxo de controle_ desse tipo. Seria um retrocesso voltar a
programar apenas no "estilo" `goto`. (Porém, isso não siginifca que `goto`
e recursão são coisas ruins. Knuth escreveu um
[famoso artigo em 1974](https://en.wikipedia.org/wiki/While_loop) defendendo
o uso cuidadoso de `goto` contra o que ele, corretamente, apontou como
dogma que foi longe demais contra o `goto`. O mesmo pode ser dito contra
qualquer dogma anti-recursão)

Quando eu ensino Haskell, me sinto envergonhado quando pessoas imaginam
o que há de tão "errado" com Haskell que você não consegue escrever código tão
intuitivo como o código Python acima - é necessário usar a tal da recursão para
fazer algo tão simples. O dilema que enfrento ao ensinar é o pensamento na
minha mente "Mas, mas, você também pode ter laços de repetição, eles só não
foram embutidos na linguagem e pode até escrever seus próprios usando funções
de alta ordem!", enquanto eu sei que muitos estão pensando "Mas que porcaria de
linguagem acadêmica, torre de marfim, imprática, ela nem ao menos tem laços" e,
enquanto isso, sei que não posso me precipitar para funções de alta ordem na
primeira hora.


### Fazendo seu próprio laço

A ideia central de programação funcional é que se você vê código usado
repetidamente, um padrão, então pode ser interessante refatorá-lo em
dois componentes:

- a estrutura abstrata do padrão
- a instância concreta do problema


`monad-loops` provê diversos combinadores úteis para capturar padrões de fluxo
de controle comuns. Por exemplo, podemos usar a função `whileM_` em nosso problema:

{% highlight haskell %}
import Control.Monad.Loops (whileM_)
{% endhighlight %}

O tipo de `whileM_` é:

{% highlight haskell %}
whileM_ :: Monad m => m Bool -> m a -> m ()
{% endhighlight %}

É necessária uma "condição" que avalia um `Bool` (em um contexto monádico `m`),
em conjunto com um "corpo" monádico arbitrário contendo uma ação a ser executada,
para retornar um "laço" (ação monádica que retorna uma unidade) que continua
testando a "condição" e executando o "corpo".

Nosso código reescrito:

{% highlight haskell %}
-- | Sem recursão explícita
logIn2 :: IO ()
logIn2 = do
  putStrLn "% Digite a senha:"
  whileM_ (do
             guess <- getLine
             return (guess /= "segredo")
          ) (do
               putStrLn "% Senha incorreta!"
               putStrLn "% Tente novamente:"
            )
  putStrLn "$ Parabéns!"
{% endhighlight %}

Isso tem uma aparência muito melhor que a recursão explícita, exceto pela
sintaxe estranha porque a "condição" e o "corpo" de nosso laço são apenas
expressões comuns e, portanto, devem ser envolvidas em parênteses para que
possam ser passadas para a função `whileM_`.

Note que [a implementação da `whileM_`](https://hackage.haskell.org/package/monad-loops-0.4.3/docs/src/Control-Monad-Loops.html#whileM_)
é código Haskell comum que abstrai exatamente a estrutura de nosso código
original, extraindo a "condição" e o "corpo" de dentro da função. Programação
funcional tem tudo a ver com ["extract method"](http://refactoring.com/catalog/extractMethod.html)!

{% highlight haskell %}
whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ p f = go
    where go = do
            x <- p
            if x
                then f >> go
                else return ()
{% endhighlight %}

#### Melhorando a sintaxe

Alertei, em posts anteriores, sobre abusar excessivo da sintaxe; então,
deliberadamente, apresentei uma sintaxe "normal" para chamar a função
`whileM_`, com o objetivo de mostrar que ela é realmente código comum,
nada de especial. Mas, para aqueles que ainda não têm tanta familiaridade
com os atalhos idiomáticos de Haskell, abaixo encontram-se alguns exemplos de
reescrita com sintax mais "sofisticada" (mas que fazem exatamente a mesma coisa).

O primeiro passo para evitar o uso excessivo de parênteses para envolver as
expressões é utilizar o operador de aplicação de função `$`, um truque que permite
remover os parênteses da expressão final (o que funciona como o "corpo" do laço):

{% highlight haskell %}
-- | With $ syntax.
logIn3 :: IO ()
logIn3 = do
  putStrLn "% Digite a senha:"
  whileM_ (do
             guess <- getLine
             return (guess /= "segredo")
          ) $ do
    putStrLn "% Senha incorreta!"
    putStrLn "% Tente novamente:"
  putStrLn "$ Parabéns!"
{% endhighlight %}


Outro passo é elevar (_lift_) as operações para dentro da mônada utilizando
`liftM`:

{% highlight haskell %}
import Control.Monad (liftM)

-- | With lifting.
logIn4 :: IO ()
logIn4 = do
  putStrLn "% Digite a senha:"
  whileM_ (liftM (\guess -> guess /= "segredo") getLine) $ do
    putStrLn "% Senha incorreta!"
    putStrLn "% Tente novamente:"
  putStrLn "$ Parabéns!"
{% endhighlight %}

O preço é que você precisa entender a elevação (_lifiting_) e o operador `$`.

Se você estiver disposto a pagar outro custo, o de utilizar secionamento de
operadores e o operador `fmap` simbólico `<$>`, aqui está a versão final:

{% highlight haskell %}
-- | With operator sectioning and <$>.
logIn5 :: IO ()
logIn5 = do
  putStrLn "% Digite a senha:"
  whileM_ ((/= "segredo") <$> getLine) $ do
    putStrLn "% Senha incorreta!"
    putStrLn "% Tente novamente:"
  putStrLn "$ Parabéns!"
{% endhighlight %}

Isso se parece muito com o código Python, exceto pelo fato de estar
cheio de sintaxe que parece bastante misteriosa para um novato em
Haskell.

## Outra tarefa: lendo e coletando linhas

Um último exemplo de refatoração para remover recursão:

Suponha que você queira escrever uma ação de `IO` para uma aplicação
de linha de comando, que lê linhas de entrada fornecidas pelo usuário até
encontrar `quit`; e você quer coletar  todas essas linhas (mas não o
`quit`) em uma lista:

{% highlight haskell %}
readLinesUntilQuit :: IO [String]
{% endhighlight %}

Um exemplo de uma sessão interativa:

{% highlight console %}
> readLinesUntilQuit
hello
lovely
world!
quit
{% endhighlight %}

Resultado:

{% highlight haskell %}
["hello","lovely","world!"]
{% endhighlight %}

Aqui está uma implementação intuitiva, utilizando recursão:

{% highlight haskell %}
readLinesUntilQuit :: IO [String]
readLinesUntilQuit = do
  line <- getLine
  if line /= "quit"
    then do
      -- recursive call, to loop
      restOfLines <- readLinesUntilQuit
      return (line : restOfLines)
    else return []
{% endhighlight %}

O código anterior não é ilegível mas, definitivamente, há bastante
_boilerplate_ presente:

- a condição
- recursão
- inserção de coisas em uma lista

### Removendo a recursão

Vamos usar `unfoldM`:

{% highlight haskell %}
unfoldM :: Monad m => m (Maybe a) -> m [a]
{% endhighlight %}

Todo o trabalho é realizado no argumento, o qual nós extraímos
em sua própria definição:

{% highlight haskell %}
-- | Sem recursão explícita
readLinesUntilQuit2 :: IO [String]
readLinesUntilQuit2 = unfoldM maybeReadLine

-- | Lê uma única linha e verifica se é igual a "quit"
maybeReadLine :: IO (Maybe String)
maybeReadLine = do
  line <- getLine
  return (if line /= "quit"
          then Just line
          else Nothing)
{% endhighlight %}

Podemos avançar ainda mais nesta refatoração, porque `maybeReadLine`
mistura ambos `IO` e uma verificação de condição pura com a
linha de entrada. **Expressões envolvidas em parênteses,
frequentemente, são sinais que indicam uma oportunidade de
refatoração.**

{% highlight haskell %}
readLinesUntilQuit3 :: IO [String]
readLinesUntilQuit3 = unfoldM (notQuit <$> getLine)

notQuit :: String -> Maybe String
notQuit line =
  if line /= "quit"
    then Just line
    else Nothing
{% endhighlight %}

Gosto desta última versão porque ela desacopla o laço, a condição
e a ação fundametal `IO` trazendo mais informação a ser utilizada.

## Uma nota sobre testes

Se você tem acompanhado toda esta série de artigos Dias de Hackage,
pode estar se perguntando por que eu não provi testes de HSpec e segui
o estilo de desenvolvimento orientado a testes (_test-driven development_).
O motivo é que eu não queria entrar no assunto de como simular IO de modo
a escrever testes que simulam entrada de um usuário.


## Conclusão

Demonstrei dois exemplos de refatoração de código para evitar recursões
explícitas, favorecendo o uso de combinadores da `monad-loops`. Eu espero
que a exploração da refatoração, assim como da sintaxe, seja útil para aqueles
que ainda não tinham familiaridades com estas técnicas e expressões
idiomáticas. Dê uma conferida em toda a biblioteca para encontrar outros
combinadores que você pode usar.


## Todo o código

Todo o código para minha série de artigos estão disponíveis
[neste repositório do GitHub](https://github.com/FranklinChen/twenty-four-days2015-of-hackage).
