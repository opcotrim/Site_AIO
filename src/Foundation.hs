{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}

module Foundation where
import Import
import Yesod
import Yesod.Static
import Data.Text
import Database.Persist.Postgresql
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Text (Text)
import Data.Time -- dia hora
import qualified Data.Text as T
import Control.Applicative
import Text.Hamlet
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )

-- a mais yesod static 
data Sitio = Sitio { connPool :: ConnectionPool,
                     getStatic :: Static }
-- static "." linux é a propria pasta
staticFiles "."

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Usuario
   nome Text
   pass Text
   deriving Show
   
Supermercado
   nome Text

Produto
   nome Text sqltype=varchar(20)
   descricao Text
   estoque Int
   precoAnter Double
   precoAtual Double 2
   
Ordem
   fornId SupermercadoId
   pecaId ProdutoId -- chave extrangeira  
   qtde Int
   data UTCTime default=now() -- data hora dia pega a hora do sistema (importa datatime)
   processado Bool
   UniqueSP fornId pecaId -- id 1,2,3, superecId 1,3,4 prod 1,2,3 join

|]

mkYesodData "Sitio" pRoutes

instance YesodPersist Sitio where
   type YesodPersistBackend Sitio = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

instance Yesod Sitio where
    authRoute _ = Just $ LoginR
    isAuthorized LoginR _ = return Authorized
    isAuthorized AdminR _ = isAdmin
    isAuthorized _ _ = isUser
-- permissoes é no foundation e quem vai ser a rota de logim, 3 tipos
-- qualquer rota é usuario
isAdmin = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just "admin" -> Authorized
        Just _ -> Unauthorized "Soh o admin acessa aqui!"

isUser = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized
-- just admin autoriza 
-- 

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Sitio FormMessage where
    renderMessage _ _ = defaultFormMessage


