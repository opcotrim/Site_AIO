{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Pesquisa where

import Database.Persist.Postgresql
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Text (Text)
import Data.Time -- dia hora
import qualified Data.Text as T
import Control.Applicative
import Yesod
--import qualified Database.Esqueleto as E
--import Database.Esqueleto ((^.))
data Pesquisar = Pesquisar{connPool :: ConnectionPool}

instance Yesod Pesquisar

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Fornecedor
   nome Text
   deriving Show
Peca
   nome Text sqltype=varchar(20)
   descricao Text
   estoque Int
   precoAnter Double
   precoAtual Double 2
Ordem
   fornId FornecedorId
   pecaId PecaId -- chave extrangeira  
   qtde Int
   data UTCTime default=now() -- data hora dia pega a hora do sistema (importa datatime)
   processado Bool
   UniqueSP fornId pecaId -- id 1,2,3, superecId 1,3,4 prod 1,2,3 join
|]

mkYesod "Pesquisar" [parseRoutes|
  /prod PecaR GET POST
  /fornecedor SuperR GET POST
  /listprod ListarPecaR GET
  /listsuper ListarSuperR GET
  /ordem OrdemR GET POST
  / ListarOrdemR GET

|]

instance YesodPersist Pesquisar where
   type YesodPersistBackend Pesquisar = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Pesquisar FormMessage where
    renderMessage _ _ = defaultFormMessage

formOrdem :: Form Ordem
formOrdem = renderDivs $ Ordem <$>
             areq (selectField supers) "Supermercado:" Nothing <*>
             areq (selectField prods)  "Escolha Prod.:" Nothing <*>
             areq intField             "Qtde Produto:" Nothing <*>
             lift (liftIO getCurrentTime) <*> -- dia atual do cadastro liftIO funçao monad valor fixo no formulario
             lift (liftIO $ return False)

prods = do
       entidades <- runDB $ selectList [] [Asc PecaNome] 
       optionsPairs $ fmap (\ent -> (pecaNome $ entityVal ent, entityKey ent)) entidades

supers = do
       entidades <- runDB $ selectList [] [Asc FornecedorNome] 
       optionsPairs $ fmap (\ent -> (fornecedorNome $ entityVal ent, entityKey ent)) entidades

formPeca :: Form Peca
formPeca = renderDivs $ Peca <$>
             areq textField   "Nome do Produt" Nothing <*>
             areq textField   "Descrição Prod." Nothing <*>
             areq intField    "Quantidad Prod." Nothing <*>
             areq doubleField "Preço Anterior." Nothing <*>
             areq doubleField "Prco Pesquisado" Nothing

formSuper :: Form Fornecedor
formSuper = renderDivs $ Fornecedor <$>
             areq textField "Nome do Supermercado..:" Nothing

widgetForm :: Route Pesquisar -> Enctype -> Widget -> Text -> Widget
widgetForm x enctype widget y = [whamlet|
            <body style="background-color:LavenderBlush">
            <h1 style="color: DeepSkyBlue;"> Cadastro de: #{y}
            
            <form method=post action=@{x} enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Enter">
|]

getPecaR :: Handler Html
getPecaR = do
           (widget, enctype) <- generateFormPost formPeca
           defaultLayout $ widgetForm PecaR enctype widget "Peca"

postPecaR :: Handler Html
postPecaR = do
            ((result,_),_) <- runFormPost formPeca
            case result of
                FormSuccess prod -> (runDB $ insert prod) >> defaultLayout [whamlet|<h1> Produto inserido|]
                _ -> redirect PecaR


getSuperR :: Handler Html
getSuperR = do
           (widget, enctype) <- generateFormPost formSuper
           defaultLayout $ widgetForm SuperR enctype widget "Supermercado"

postSuperR :: Handler Html
postSuperR = do
            ((result,_),_) <- runFormPost formSuper
            case result of
                FormSuccess super -> (runDB $ insert super) >> defaultLayout [whamlet|<h1> Supermercado inserido|]
                _ -> redirect SuperR

getListarPecaR :: Handler Html
getListarPecaR = do
                 produtos <- runDB $ selectList [] [Asc PecaNome]
                 defaultLayout [whamlet|
                     <body style="background-color:LavenderBlush">
                      <h1 style="color: DeepSkyBlue;"> Produtos Cadastrados:
                        <style>
                          table, th, td { border: 1px solid black;}
                      <table>
                        <td>
                            Lista de Produtos
                          $forall Entity pid prod <- produtos
                            <tr><td style="color: green;">#{pecaNome prod}
                            <tr><td>Prc.Anterior:#{pecaPrecoAnter prod}
                            <tr><td>Prc.Atual...:#{pecaPrecoAtual prod}
                 |]
-- <h5> #{pecaDescricao prod}

getListarSuperR :: Handler Html
getListarSuperR = do
                 superm <- runDB $ selectList [] [Asc FornecedorNome]
                 defaultLayout [whamlet|
                     <body style="background-color:LavenderBlush">
                      <h1 style="color: DeepSkyBlue;"> Supermermercados Cadastrados:
                        <style>
                          table, th, td { border: 1px solid black;}
                      <table>
                        <tr>
                            Nomes dos Supermercados
                       $forall Entity fid super <- superm
                         <td> #{fornecedorNome super}
                 |]

getOrdemR :: Handler Html
getOrdemR = do
           (widget, enctype) <- generateFormPost formOrdem
           defaultLayout $ widgetForm OrdemR enctype widget "Pesquisa de preços:"

postOrdemR :: Handler Html
postOrdemR = do
            ((result,_),_) <- runFormPost formOrdem
            case result of
                FormSuccess x -> (runDB $ insert x) >> defaultLayout [whamlet|<h1> Pesquisa de preços inserida|]
                _ -> redirect OrdemR

getListarOrdemR :: Handler Html
getListarOrdemR = do
                 ordens <- runDB $ (rawSql "SELECT ??, ??, ?? \
                                           \FROM ordem \
                                           \INNER JOIN peca       ON ordem.peca_id=peca.id \
                                           \INNER JOIN fornecedor ON ordem.peca_id=fornecedor.id "
                                           [])::Handler [(Entity Ordem, Entity Peca, Entity Fornecedor)]
                 defaultLayout [whamlet|
                      <body style="background-color:LavenderBlush">
                        <h1 style="color: DeepSkyBlue;"> Produtos Pesquisados:
                          <style>
                              table, th, td {
                                border: 1px solid black;
                              }
                        <table>
                         <tr>
                             <td>Data....:<td>Nro.Item:<td>Supermer:<td>Quantida:<td>Produtos:<td>Prc.Anter:<td>Prc.Atual:
                                $forall (Entity oq ordem, Entity _ prd, Entity _ nf) <- ordens
                                 <tr>
                                 <td style="color: green; "> #{show $ utctDay $ ordemData ordem}
                                 <td style="color: orange;">#{fromSqlKey oq}
                                 <td style="color: blue;  ">#{fornecedorNome nf}
                                 <td style="color: purple;">#{show $ ordemQtde ordem}
                                 <td style="color: black; "><b>#{pecaNome prd}
                                 <td style="color: DarkRed; ">#{pecaPrecoAnter prd}
                                 <td style="color: red;   ">#{pecaPrecoAtual prd}
                 |]

connStr = "dbname=d266oucgqg58dl host=ec2-107-21-224-11.compute-1.amazonaws.com user=fmtteyovbsxaip password=2henlXgzObUw18mREZh_TjF4rN port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
       runSqlPersistMPool (runMigration migrateAll) pool
       warpEnv (Pesquisar pool)