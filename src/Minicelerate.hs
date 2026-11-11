module Minicelerate where

import Control.Monad.IO.Class (liftIO)
import Data.Kind              (Type)
import Data.Word              (Word64)

import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH

-------------------------------------------------------------------------------
-- Terms (expressions)
-------------------------------------------------------------------------------

data Tm where
    Var :: Tm
    Lit :: Word64 -> Tm
    Add :: Tm -> Tm -> Tm

deriving instance Show Tm

-------------------------------------------------------------------------------
-- Interpreter
-------------------------------------------------------------------------------

interpret :: Tm -> Word64 -> Word64
interpret Var       x = x
interpret (Lit x)   _ = x
interpret (Add t s) x = interpret t x + interpret s x

-------------------------------------------------------------------------------
-- Template Haskell
-------------------------------------------------------------------------------

splice' :: Tm -> TH.CodeQ Word64 -> TH.CodeQ Word64
splice' Var       x = x
splice' (Lit x)   _ = [|| $$(TH.liftTyped x) :: Word64 ||]
splice' (Add t s) x = [|| $$(splice' t x) + $$(splice' s x) ||]

splice :: Tm -> TH.CodeQ (Word64 -> Word64)
splice t = [|| \x -> $$(splice' t [|| x ||]) ||]

-------------------------------------------------------------------------------
-- C
-------------------------------------------------------------------------------

cProgram :: String -> Tm -> String
cProgram name t = unlines
    [ "#include <stdint.h>"
    , "#include <stdio.h>"
    , "uint64_t " ++ name ++ "(uint64_t x) {"
    , "    // printf(\"debug: \\n\");"
    , "    return " ++ cTerm 0 t "" ++ ";"
    , "}"
    ]

cTerm :: Int -> Tm -> ShowS
cTerm _ Var       = showString "x"
cTerm _ (Lit n)   = shows n
cTerm d (Add t s) = showParen (d > 5) $
    cTerm 5 t . showString " + " . cTerm 4 s

-------------------------------------------------------------------------------
-- Compile time "native" via C, make GHC compile the C code.
-------------------------------------------------------------------------------

nativeCode :: Tm -> TH.CodeQ (Word64 -> Word64)
nativeCode t = TH.joinCode $ do
    -- an option would be to use TH.newName to generate fresh name for Haskell name
    --
    -- hsName <- TH.newName "_native"
    --
    -- However, this doesn't work https://gitlab.haskell.org/ghc/ghc/-/work_items/13587
    -- But this is questionable: we need to generate an unique C name anyway,
    -- and we can reuse the similar scheme for the Haskell name.

    -- TODO: actually generate unique names
    -- let hsName = TH.Name (TH.OccName "foobar") TH.NameS
    --
    -- but GHC has a bug: https://gitlab.haskell.org/ghc/ghc/-/work_items/28113
    -- we need to use newName anyway :(
    hsName <- TH.newName "foobar"
    let cName = "foobarbazquu"

    let code = cProgram cName t
    liftIO $ putStrLn code

    -- add C program
    TH.addForeignSource TH.LangC code

    -- type of FFI import
    ty <- [t| Word64 -> Word64 |]

    -- add top level FFI import
    TH.addTopDecls $ singleton $
        TH.ForeignD $ TH.ImportF TH.CCall TH.Safe cName hsName ty

    -- in this simple example we don't need to create wrapper around FFI import
    return $ TH.unsafeCodeCoerce $ TH.varE hsName

-------------------------------------------------------------------------------
-- Compile time by manually invoking GCC.
-------------------------------------------------------------------------------

-- this is like what accelerate-llvm-native does
-- though a-l-m generates llvm IR, what underlying compiler (and language)
-- is used is not relevant as long as the interface is C calls.

-- TODO

-------------------------------------------------------------------------------
-- Example
-------------------------------------------------------------------------------

example :: Tm
example = Add Var (Lit 5)

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

singleton :: x -> [x]
singleton x = [x]
