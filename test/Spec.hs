import Test.Hspec
import Test.QuickCheck
import qualified Logic.Pure.Params as Params
import Lib
import           Network.HTTP.Types.URI     as HTTP
import Interface.Cache
import Data.Either
import Interface.Error as Error
--import App.Emulate  --перенести потом тестовые случаи куда-нибудь в отдельный модуль папки test
--и модуль Emulate тоже в test

main :: IO ()
main = hspec testParseParams

-----------------------------parseParams--------------------------------------------

parseParams :: Query -> API -> Either E ParamsMap
parseParams = Params.parseParams

testParseParams :: Spec
testParseParams = do
    --undefined
    describe "Logic.parseParams" $ do 
      it "throws some error" $ do
        parseParams [] (API Insert [Author]) `shouldSatisfy` isLeft

    -- zip pathInfos queries where
    -- pathInfos = repeat $ API Insert [Author] 
-- insertAuthorCases :: [(Either E ParamsMap -> Bool, API, Query)]
-- insertAuthorCases = 
--     [
--             --некорректные запросы
--             --Ошибка веб-запроса: Не указан обязательный параметр "description"
--             (,,) Nothing (API Insert [Author]) 
--                 [],

--             --Ошибка веб-запроса: Недопустимый параметр запроса: "foo"
--             (,,) Nothing (API Insert [Author])
--                 [("user_id", Just "1"),
--                 ("foo", Just "bar")],

--             --Ошибка веб-запроса: Не указано значение обязательного параметра "description"
--             (,,) Nothing (API Insert [Author])
--                 [("user_id", Just "1"),
--                 ("description", Nothing)],

--             --Ошибка веб-запроса: Параметр запроса "user_id" должен быть целым числом
--             (,,) Nothing (API Insert [Author])
--                 [("user_id", Just "bar"),
--                 ("description", Just "bar")],

--             --Ошибка веб-запроса: Недопустимый параметр запроса: "foo"
--             (,,) Nothing (API Insert [Author])
--                 [("user_id", Just "1"),
--                 ("description", Just "1"),
--                 ("foo", Just "bar")],

--             --КОРРЕКТНЫЕ ПАРАМЕТРЫ
--             (,,) Nothing (API Insert [Author])
--             [("user_id", Just "666"),
--             ("description", Just "description for user_id=666")],

--             --КОРРЕКТНЫЕ ПАРАМЕТРЫ
--             (,,) Nothing (API Insert [Author])
--             [("user_id", Just "2"),
--             ("description", Just "description for user_id=1")]
--         ]