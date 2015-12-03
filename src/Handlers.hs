{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}

module Handlers where

import Import
import Yesod
import Yesod.Static -- novo para imagens.
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Text.Lucius -- serve para separar o front do codigo uma para cada text.julius, text.cassius, hamlet vem de graça
import Database.Persist.Postgresql
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Text (Text)
import Data.Time -- dia hora
import qualified Data.Text as T
import Text.Hamlet
import Database.Persist.Postgresql

mkYesodDispatch "Sitio" pRoutes

-- form esta defindo com 05 parametros - x = rota,  y=Text val = Text e retorna um widget (whamletFile é uma pagina) 
-- gerador de formulario ctrl c e ctrl v

getMenuR :: Handler Html
getMenuR = defaultLayout [whamlet| <h1>
                   <body style="background-color:LavenderBlush">
                    <div class="divNmPagina"><center><b>Menu - Pesquisa de Preços
                      <h4 style="color: DeepSkyBlue;"> Opções:<h5> <hr> 
                        <a  href="ordem"> <br>
                           Cadastro de Pesquisa 
                        <a href="prod"> <br>
                          Cadastro de Produtos
                        <a href="supermercado"> <br>
                           Cadastro de Supermercado
                         <a href="listprod"> <br>
                           Lista de Produtos
                        <a href="listsuper"> <br>
                           Lista de Supermercados
                        <a href="resultado"> <br>
                           Resultado da Pesquisa 
                        <a href="autor"> <br>
                            Autores
                        <a href="bye"> <br>
                                  Sair 
|]

getAutorR :: Handler Html
getAutorR = defaultLayout [whamlet|
              <body style="background-color:LavenderBlush">
                <h1 style="color: DeepSkyBlue;"> Autores: 
                <li><h3> Alef Rodrigues Franco <br>
                <li><h3> Isabela Baraldi Gandelman <br>
                <li><h3> Osvaldo Pereira Cotrim <br>

|]

widgetForm :: Route Sitio -> Enctype -> Widget -> Text -> Text -> Widget
widgetForm x enctype widget y val = do
     msg <- getMessage
     $(whamletFile "form.hamlet")
     toWidget $(luciusFile "teste.lucius")

-- tabelas sem validaçao, e a relaçao de 1 para 1
formUsu :: Form Usuario
formUsu = renderDivs $ Usuario <$>
    areq textField "Username" Nothing <*>
    areq textField "Pass" Nothing


-- cadastro de formulario botao cadastrar
getUsuarioR :: Handler Html
getUsuarioR = do
    (wid,enc) <- generateFormPost formUsu
    defaultLayout $ widgetForm UsuarioR enc wid "Cadastro de Usuarios" "Cadastrar"

-- pegar imagem - interpola na pasta empolgou
getImgR :: Handler Html
getImgR = defaultLayout [whamlet| 
    <img src=@{StaticR empolgou_jpg}> 
|]

-- procurado a session  com  e retorna uma maybe text para tirar o just(so entra pessoas autenticadas autorizaçao minima)
getWelcomeR :: Handler Html
getWelcomeR = do
     usr <- lookupSession "_ID"
     defaultLayout [whamlet|
        $maybe m <- usr
            <h1>
                <body style="background-color:LavenderBlush">
                     <div class="divNmPagina"><center><b>Menu - Pesquisa de Preços
                          <h4 style="color: DeepSkyBlue;"> Logado: #{m}
     |]

-- inicio
getLoginR :: Handler Html
getLoginR = do 

    (wid,enc) <- generateFormPost formUsu
    defaultLayout $ widgetForm LoginR enc wid "Entrar" "Login"

getCadastroR :: Handler Html
getCadastroR = do
    (wid,enc) <- generateFormPost formUsu
    defaultLayout $ widgetForm CadastroR enc wid "Cadastrar" "Cadastro"

getResultadoR :: Handler Html
getResultadoR = do
                 ordens <- runDB $ (rawSql "SELECT ??, ??, ?? \
                                           \FROM ordem \
                                           \INNER JOIN produto       ON ordem.peca_id=produto.id \
                                           \INNER JOIN supermercado ON ordem.peca_id=supermercado.id "
                                           [])::Handler [(Entity Ordem, Entity Produto, Entity Supermercado)]
                 defaultLayout [whamlet|
                      <body style="background-color:LavenderBlush">
                        <h1 style="color: DeepSkyBlue;"> Produtos Pesquisados:
                          <style>
                              table, th, td {
                                border: 1px solid black;
                              }
                        <table>
                         <tr>
                             <td>Data....:<td>Nro.Item:<td>Supermercado:<td>Quantidade:<td>Produtos:<td>Preço Anterior:<td>Preço Atual:
                                $forall (Entity oq ordem, Entity _ prd, Entity _ nf) <- ordens
                                 <tr>
                                 <td style="color: green; "> #{show $ utctDay $ ordemData ordem}
                                 <td style="color: orange;">#{fromSqlKey oq}
                                 <td style="color: blue;  ">#{supermercadoNome nf}
                                 <td style="color: purple;">#{show $ ordemQtde  ordem}
                                 <td style="color: black; "><b>#{produtoNome prd}
                                 <td style="color: DarkRed; ">#{produtoPrecoAnter prd}
                                 <td style="color: red;   ">#{produtoPrecoAtual prd}
                 |]


getListarSuperR :: Handler Html
getListarSuperR = do
                 superm <- runDB $ selectList [] [Asc SupermercadoNome]
                 defaultLayout [whamlet|
                     <body style="background-color:LavenderBlush">
                      <h1 style="color: DeepSkyBlue;"> Supermermercados Cadastrados:
                        <style>
                          table, th, td { border: 1px solid black;}
                      <table>
                        <tr>
                            Supermercados:
                       $forall Entity fid super <- superm
                         <td> #{supermercadoNome super}
                 |]

getListarProdutoR :: Handler Html
getListarProdutoR = do
                 produtos <- runDB $ selectList [] [Asc ProdutoNome]
                 defaultLayout [whamlet|
                     <body style="background-color:LavenderBlush">
                      <h1 style="color: DeepSkyBlue;"> Produtos Cadastrados:
                        <style>
                          table, th, td { border: 1px solid black;}
                      <table>
                        <td>
                            Lista de Produtos
                          $forall Entity pid prod <- produtos
                            <tr><td style="color: green;">#{produtoNome prod}
                            <tr><td>Prc.Anterior:#{produtoPrecoAnter prod}
                            <tr><td>Prc.Atual...:#{produtoPrecoAtual prod}
                 |]

--inicio

formOrdem :: Form Ordem
formOrdem = renderDivs $ Ordem <$>
             areq (selectField supers) "Supermercado:" Nothing <*>
             areq (selectField prods)  "Escolha Prod.:" Nothing <*>
             areq intField             "Qtde Produto:" Nothing <*>
             lift (liftIO getCurrentTime) <*> -- dia atual do cadastro liftIO funçao monad valor fixo no formulario
             lift (liftIO $ return False)

prods = do
       entidades <- runDB $ selectList [] [Asc ProdutoNome] 
       optionsPairs $ fmap (\ent -> (produtoNome $ entityVal ent, entityKey ent)) entidades

supers = do
       entidades <- runDB $ selectList [] [Asc SupermercadoNome] 
       optionsPairs $ fmap (\ent -> (supermercadoNome $ entityVal ent, entityKey ent)) entidades

getOrdemR :: Handler Html
getOrdemR = do
           (widget, enctype) <- generateFormPost formOrdem
           defaultLayout $ widgetForm1 OrdemR enctype widget "Pesquisa de preços."

postOrdemR :: Handler Html
postOrdemR = do
            ((result,_),_) <- runFormPost formOrdem
            case result of
                FormSuccess x -> (runDB $ insert x) >> defaultLayout [whamlet|<h1> Pesquisa de preços inserida com sucessos!|]
                _ -> redirect OrdemR
--
formProduto :: Form Produto
formProduto = renderDivs $ Produto <$>
             areq textField   "Nome do Produt" Nothing <*>
             areq textField   "Descrição Prod." Nothing <*>
             areq intField    "Quantidad Prod." Nothing <*>
             areq doubleField "Preço Anterior." Nothing <*>
             areq doubleField "Prco Pesquisado" Nothing

formSuper :: Form Supermercado
formSuper = renderDivs $ Supermercado <$>
             areq textField "Nome do Supermercado..:" Nothing

widgetForm1 :: Route Sitio -> Enctype -> Widget -> Text -> Widget
widgetForm1 x enctype widget y = [whamlet|
            <body style="background-color:LavenderBlush">
            <h1 style="color: DeepSkyBlue;"> Cadastro: #{y}
            
            <form method=post action=@{x} enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Enter">
|]
getProdutoR :: Handler Html
getProdutoR = do
           (widget, enctype) <- generateFormPost formProduto
           defaultLayout $ widgetForm1 ProdutoR enctype widget "Produto"

postProdutoR :: Handler Html
postProdutoR = do
            ((result,_),_) <- runFormPost formProduto
            case result of
                FormSuccess prod -> (runDB $ insert prod) >> defaultLayout [whamlet|
                <body style="background-color:LavenderBlush">
                  <h1 style="color: DeepSkyBlue;"> Produto inserido|]
                _ -> redirect ProdutoR

getSuperR :: Handler Html
getSuperR = do
           (widget, enctype) <- generateFormPost formSuper
           defaultLayout $ widgetForm1 SuperR enctype widget "Supermercado"

postSuperR :: Handler Html
postSuperR = do
            ((result,_),_) <- runFormPost formSuper
            case result of
                FormSuccess super -> (runDB $ insert super) >> defaultLayout [whamlet|
                <body style="background-color:LavenderBlush">
                  <h1 style="color: DeepSkyBlue;"> Supermercado inserido|]
                _ -> redirect SuperR



--fim

postLoginR :: Handler Html
postLoginR = do
    ((result,_),_) <- runFormPost formUsu
    case result of
        FormSuccess usr -> do
            usuario <- runDB $ selectFirst [UsuarioNome ==. usuarioNome usr, UsuarioPass ==. usuarioPass usr ] []
            case usuario of
                Just (Entity uid usr) -> do
                    setSession "_ID" (usuarioNome usr)
                    redirect WelcomeR
                Nothing -> do
                    setMessage $ [shamlet| Invalid user |]
                    redirect LoginR 
        _ -> redirect LoginR

                             
-- linha 54 executa o formulario para pegar o post se bem sucedido retorna usuario. 
-- pega só o primeiro select frist select from todos usuarios where nome o cara do form usr ==. igualdade do yesod perssist
-- case usuario, entity pode ter ou não, coloca o nome e senha na session root, autenticado vem do banco de dados e redireciona para welcomeR
-- nothing caso náo esteja no banco - shamlet para mensg html setmesseger. depois redireciona para login 

postCadastroR :: Handler Html
postCadastroR = do
    ((result,_),_) <- runFormPost formUsu
    case result of
        FormSuccess usr -> do
            runDB $ insert usr
            setMessage $ [shamlet| <p> Cadastro inserido com sucesso! |]
            redirect CadastroR
        _ -> redirect CadastroR

-- set messager é novo.
postUsuarioR :: Handler Html
postUsuarioR = do
    ((result,_),_) <- runFormPost formUsu
    case result of
        FormSuccess usr -> do
            runDB $ insert usr
            setMessage $ [shamlet| <p> Usuario inserido com sucesso! |]
            redirect UsuarioR
        _ -> redirect UsuarioR


-- listar
getListUserR :: Handler Html
getListUserR = do
    listaU <- runDB $ selectList [] [Asc UsuarioNome]
    defaultLayout $(whamletFile "list.hamlet")

--getby deleta a sessao
getByeR :: Handler Html
getByeR = do
    deleteSession "_ID"
    defaultLayout [whamlet| <h1>
                            <body style="background-color:LavenderBlush">
                                <div class="divNmPagina"><center><b>Menu - Pesquisa de Preços
                                    <h3 style="color: DeepSkyBlue;"> Finalizado! |]
-- finaliza


getAdminR :: Handler Html
getAdminR = defaultLayout [whamlet| <h1>
                                    <body style="background-color:LavenderBlush">
                                      <div class="divNmPagina"><center><b>Menu - Pesquisa de Preços
                                        <h3 style="color: DeepSkyBlue;"> Bem-vindo! |]


connStr = "dbname=d266oucgqg58dl host=ec2-107-21-224-11.compute-1.amazonaws.com user=fmtteyovbsxaip password=2henlXgzObUw18mREZh_TjF4rN port=5432"
-- "dbname=dd9en8l5q4hh2a host=ec2-107-21-219-201.compute-1.amazonaws.com user=kpuwtbqndoeyqb password=aCROh525uugAWF1l7kahlNN3E0 port=5432"
-- "dbname=d266oucgqg58dl host=ec2-107-21-224-11.compute-1.amazonaws.com user=fmtteyovbsxaip password=2henlXgzObUw18mREZh_TjF4rN port=5432"

-- static e novo sitio tem dois parametros pool e s 
main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       s <- static "."
       warpEnv (Sitio pool s)