{-# LANGUAGE OverloadedStrings #-}
import Core.AST.Parser
import Core.AST.Serializer
import Core.AST.Editor
import Core.Types
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath ((</>))

main :: IO ()
main = do
    content <- TIO.readFile "test/Golden/fixtures/complex.cabal"
    let ast0 = parseAST content
        dep = Dependency (trustedMkPackageName "containers") (Just (UnparsedVersion ">=0.1")) BuildDepends
    
    -- Add
    case addDependencyToAST "library" Nothing dep ast0 of
        Failure err -> print err
        Success ast1 -> do
            -- Remove
            case removeDependencyFromAST "library" Nothing "containers" ast1 of
                Failure err -> print err
                Success ast2 -> do
                    let final = serializeAST ast2
                    if final == content
                        then putStrLn "MATCH"
                        else do
                            putStrLn "MISMATCH"
                            TIO.writeFile "temp_debug/final.cabal" final
                            TIO.writeFile "temp_debug/orig.cabal" content
