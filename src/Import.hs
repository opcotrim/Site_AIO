{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod
import Yesod.Static
-- statac para imagens get static eé um campo que recebe uma funçao cria uma funçao para cada imagem carro.png carro_png
-- abre a tag <img src = @{staticR carro_png}>

pRoutes = [parseRoutes|
   /user UsuarioR GET POST
   /listar ListUserR GET
   /static StaticR Static getStatic
   /ima ImgR GET
   /login LoginR GET POST
   / WelcomeR GET
   /bye ByeR GET
   /admin AdminR GET
   /cad CadastroR GET POST
   /menu MenuR GET
   /autor AutorR GET
   /resultado ResultadoR GET
   /listprod ListarProdutoR GET
   /listsuper ListarSuperR GET

   
|]

pRoutes1 = [parseRoutes|
  / ListarOrdemR GET
  /ordem OrdemR GET POST
  /prod ProdutoR GET POST
  /supermercado SuperR GET POST
  -- ROTAS não estao linkadas
|]

