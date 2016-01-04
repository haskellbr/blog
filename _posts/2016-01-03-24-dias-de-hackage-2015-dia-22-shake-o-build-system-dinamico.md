---
layout: post
title: "24 dias de Hackage, 2015 - dia 22 - Shake: O build system dinâmico"
author: Franklin Chen
author_url: "http://conscientiousprogrammer.com/"
translator: Pedro Yamada
translator_url: "https://github.com/yamadapc"
excerpt_separator: "<!-- more -->"
---
_Esse é um artigo escrito por Franklin Chen e traduzido para o português.
[Ler original.](http://conscientiousprogrammer.com/blog/2015/12/22/24-days-of-hackage-2015-day-22-shake-the-dynamic-build-system/)_

## Índice de toda a série
O índice de toda a série está no topo do artigo para o [dia 1](/2015/12/08/aperitivos-de-haskell-24-dias-de-hackage-2015-dia-1-introducao-e-stack.html).

## Encontro HaskellBR São Paulo
[Vamos nos encontrar em São Paulo em Janeiro de 2016. Marque sua presença e comente se não puder vir.](http://www.meetup.com/haskellbr-sp/events/227526368/)

[Vote se o encontro terá workshops, um dojo ou palestras.](https://plus.google.com/+DanielYokomizo/posts/bM1C5HRLcR4)

# Dia 22
([Discussão do original no Reddit](https://www.reddit.com/r/haskell/comments/3xu80o/24_days_of_hackage_2015_day_22_shake_the_dynamic/))

`make`, o venerável build system, é falho em muitos sentidos. Para mim,
os dois maiores problemas eram:

- Lidar com builds *dinâmicas*, onde algo durante o build atira o build de
  dependencias
- Querer escrever código em uma linguagem de verdade, não
  `make`, "a linguagem _turing complete_"

Muitos build systems melhorados surgiram desde o `make`. Por exemplo, alguns
anos atrás, descobri o [SCons](http://www.scons.org/), o build system
*dinâmico* baseado em Python e ele melhorou muito minha vida, porque podia o
tratar como uma biblioteca embedável e fazer as tarefas chamarem meu código
Python. Eu ainda tenho programas usando SCons em uso.

<!-- more -->

No entanto, [Shake](http://shakebuild.com/) apareceu e eu não estou olhando
para trás. Ele tem as virtudes do SCons e mais. EU já o uso em novos setups de
build e planejo migrar builds antigas usando SCons para Shake na próxima vez
que elas precisarem de qualquer retrabalho significativo.

Note que o
[build do GHC está migrando para o Shake](https://ghc.haskell.org/trac/ghc/wiki/Building/Shake),
então há uma quantidade séria de areia sendo levantada.

## Para informações sobre o Shake

Ao invés de repetir uma fração da documentação extensiva e excelente do Shake,
os direciono para o
[Web site](http://shakebuild.com/), que incluí tutoriais e material de
referência. Também há uma
[lista de e-mails no Google](https://groups.google.com/forum/?fromgroups#!forum/shake-build-system).
O [repositório no GitHub](https://github.com/ndmitchell/shake)
é sempre extremamente ativo.

Também dê olhada
[no blog do criador, Neil Mitchell](http://neilmitchell.blogspot.com/).

## Um pequeno exemplo do Shake
Eu decidi investigar o Shake um pouco para esse post. Acontece que eu deveria
ter feito isso mais cedo. Eu vinha fazendo a mesma tarefa repetitiva todos os
dias, copiando informação para um índice crescente de toda a série "Dias de
Hackage", enquanto deveria só ter escrito um programa para fazer isso
automaticamente.

Aqui está um programa usando Shake de exemplo que eu finalmente escrevi para
realizar essa tarefa. É uma build dinâmica porque o índice da série depende de
um processo estar rodando (o gerador de sites estáticos Hugo) e extrair
informações dele, assim como de arquivos Markdown. Eu extraí o dia e título dos
arquivos Markdown e combino o dia com o URL para o HTML gerado para criar um
índice.

### Compilação
Eu compilo meus programas usando Shake, porque isso faz as coisas rodarem muito
mais rápido, mas você também pode só rodar o comando `shake`.

### Dependencias dinâmicas
Note que eu também poderia ter usado um
[oracle](https://hackage.haskell.org/package/shake-0.15.5/docs/Development-Shake.html#g:11)
para salvar dados estruturados, mas por propósitos ilustrativos eu os salvei em
arquivos, arcando com o custo de pressupostos "tipados a strings" sobre o
conteúdo dos arquivos.

Em suma, Shake é sobre `want` (alvo) e `need` (o que depender sobre). Algumas
funções implícitamente adicionam um `need`, como `getDirectoryFiles` e
`readFileLines`. Eu uso o `writeFileChanged` e deixo o Shake decidir se o
conteúdo de um arquivo mudou.

Peço desculpas pelos imports não explícitos. Como o Shake é uma linguagem
extensível de domínio específico, eu decidi só absorver seu vocabulário, mas
entendo que isso dificulta a leitura do código.

{% highlight haskell %}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import MultilineRe (multilineRe)

import Development.Shake
import Development.Shake.FilePath

import Text.Regex.PCRE.Heavy (Regex, scan)
import Data.Maybe (listToMaybe)
import Text.Printf (printf)
import Control.Monad (zipWithM)

shakeDir :: FilePath
shakeDir = "_build"

-- | File, sorted by day, containing a line for each Markdown source of a post.
daysSources :: FilePath
daysSources = shakeDir </> "days-sources"

-- | File, sorted by day, containing a line for each generated Day of Hackage post.
daysUrls :: FilePath
daysUrls = shakeDir </> "days-urls"

tocFile :: FilePath
tocFile = shakeDir </> "TOC.md"

-- | Base directory of blog source.
blogDir :: FilePath
blogDir = "/Users/chen/Sync/ConscientiousProgrammer"

-- | Base directory of generated blog.
publicDir :: FilePath
publicDir = "/Users/chen/ConscientiousProgrammer-public"

-- | Generated HTML directory for each post.
urlsGlob :: FilePattern
urlsGlob = "blog/2015/1*/*/*hackage-2015-day-*"

-- | Location of Markdown blog posts.
postDir :: FilePath
postDir = blogDir </> "content/post"

-- | Rely on naming convention here.
postGlob :: FilePattern
postGlob = "*hackage-2015-day-*"

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=shakeDir} $ do
  want [tocFile]

  tocFile %> \out -> do
    sourcePaths <- readFileLines daysSources
    urls <- readFileLines daysUrls

    toc <- liftIO $ zipWithM extractTOCEntry sourcePaths urls
    writeFileChanged out $ unlines $ map formatTOCEntry toc

  -- Run Hugo to generate a directory for each post.
  daysUrls %> \out -> do
    need [daysSources]
    unit $ cmd (Cwd blogDir) "hugo"
    Stdout stdout <- cmd Shell (Cwd publicDir) "echo" urlsGlob
    writeFileChanged out $ unlines $ words stdout

  daysSources %> \out -> do
    daysFiles <- getDirectoryFiles postDir [postGlob]
    writeFileChanged out $ unlines $ map (postDir </>) daysFiles

-- | A day's entry in the TOC.
data TOCEntry =
  TOCEntry { _day :: Int
           , _title :: String
           , _url :: String
           }
  deriving (Eq, Ord)

dayTitleRegex :: Regex
dayTitleRegex = [multilineRe|^title:.*day\s+(\d+):\s*([^"]+)|]

extractTOCEntry :: FilePath -> String -> Action TOCEntry
extractTOCEntry sourcePath url = do
  text <- readFile' sourcePath
  case listToMaybe (scan dayTitleRegex text) of
    Just (_, [dayString, title]) ->
      return $ TOCEntry (read dayString) title url
    _ ->
      error $ printf "failed to extract day and title from %s" sourcePath

formatTOCEntry :: TOCEntry -> String
formatTOCEntry entry =
  printf "- Day %d: [%s](/%s/)" (_day entry) (_title entry) (_url entry)
{% endhighlight %}

## Mais recursos
Uma boa talk introdutória pelo Neil tentando vender o Shake como um
_killer-app_ para uma audiência de _não-Haskellers_ é:


<iframe width="560" height="315" src="https://www.youtube.com/embed/iFZQyLMrkn4" frameborder="0" allowfullscreen></iframe>

Na sua talk no Haskell eXchange 2015,
["Defining your own build system with Shake"](http://neilmitchell.blogspot.com/2015/10/defining-your-own-shake-build-system.html).

## Conclusão
Se você tem que compilar coisas, use o Shake. Eu estou muito animado pelo seu
desenvolvimento ativo.

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
