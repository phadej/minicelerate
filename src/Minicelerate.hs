module Minicelerate where

import Control.Monad.IO.Class     (liftIO)
import Foreign.Ptr                (FunPtr)
import System.FilePath            ((</>))
import System.Posix.DynamicLinker (RTLDFlags (RTLD_LOCAL, RTLD_NOW), dlopen, dlsym)

import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified System.Directory           as Dir
import qualified System.Process             as Proc

-------------------------------------------------------------------------------
-- Terms (expressions)
-------------------------------------------------------------------------------

-- The term language in this example is very simple on purpose
-- The pipeline how we intepret or compile the term language doesn't change
-- with the complexity of the term language; here we are not interested in
-- "code generation" step.
data Tm where
    Var :: Tm
    Lit :: Double -> Tm
    Add :: Tm -> Tm -> Tm
    Sin :: Tm -> Tm

deriving instance Show Tm

-------------------------------------------------------------------------------
-- Interpreter
-------------------------------------------------------------------------------

interpret :: Tm -> Double -> Double
interpret Var       x = x
interpret (Lit x)   _ = x
interpret (Add t s) x = interpret t x + interpret s x
interpret (Sin t)   x = sin (interpret t x)

-------------------------------------------------------------------------------
-- Template Haskell
-------------------------------------------------------------------------------

-- this would be like using runQ on Intepreter backend of accelerate.
splice :: Tm -> TH.CodeQ (Double -> Double)
splice t = [|| \x -> $$(splice' t [|| x ||]) ||]

splice' :: Tm -> TH.CodeQ Double -> TH.CodeQ Double
splice' Var       x = x
splice' (Lit x)   _ = [|| $$(TH.liftTyped x) :: Double ||]
splice' (Add t s) x = [|| $$(splice' t x) + $$(splice' s x) ||]
splice' (Sin t)   x = [|| sin @Double $$(splice' t x) ||]

-------------------------------------------------------------------------------
-- C
-------------------------------------------------------------------------------

cProgram :: String -> Tm -> String
cProgram name t = unlines
    [ "#include <math.h>"
    , "#include <stdio.h>"
    , "double " ++ name ++ "(double x) {"
    , "    // printf(\"debug: \\n\");"
    , "    return " ++ cTerm 0 t "" ++ ";"
    , "}"
    ]

cTerm :: Int -> Tm -> ShowS
cTerm _ Var       = showString "x"
cTerm _ (Lit n)   = shows n
cTerm d (Add t s) = showParen (d > 5) $
    cTerm 5 t . showString " + " . cTerm 4 s
cTerm _ (Sin t)   =
    showString "sin(" . cTerm 0 t . showChar ')'

-------------------------------------------------------------------------------
-- Compile time "native" via C, make GHC compile the C code.
-------------------------------------------------------------------------------

-- | accelerate doesn't have "raw" C backend.
-- accelerate-llvm generates LLVM IR, which cannot be compiled by GHC.
--
-- If there were a accelerate backend generating C, its runQ
-- variant would be the cleanest:
-- The code below doesn't use file system.
--
nativeCode :: Tm -> TH.CodeQ (Double -> Double)
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
    let cName = "foobar"

    let code = cProgram cName t
    -- liftIO $ putStrLn code

    -- add C program
    TH.addForeignSource TH.LangC code

    -- type of FFI import
    ty <- [t| Double -> Double |]

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

-- | runQ with Native backend.
nativeCodeManual :: Tm -> TH.CodeQ (Double -> Double)
nativeCodeManual t = TH.joinCode $ do
    root <- TH.getPackageRoot >>= liftIO . Dir.makeAbsolute

    hsName <- TH.newName "foobar2"
    let cName = "foobar2"

    let code = cProgram cName t
    objectPath <- liftIO $ compileCObject root code

    -- add C program
    TH.addForeignFilePath TH.RawObject objectPath

    -- type of FFI import
    ty <- [t| Double -> Double |]

    -- add top level FFI import
    TH.addTopDecls $ singleton $
        TH.ForeignD $ TH.ImportF TH.CCall TH.Safe cName hsName ty

    -- in this simple example we don't need to create wrapper around FFI import
    return $ TH.unsafeCodeCoerce $ TH.varE hsName

compileCObject :: FilePath -> String -> IO FilePath
compileCObject root code = do
    let cfile = root </> "code.c"
    let ofile = root </> "code.o"

    writeFile cfile code
    Proc.callProcess "gcc" ["-c", "-o", ofile, cfile]

    return ofile

-------------------------------------------------------------------------------
-- Run-time by manually invoking GCC to create shared library
-------------------------------------------------------------------------------

-- 1. Compile with PIC
-- $ gcc -c -Wall -Werror -fpic foo.c
--
-- 2. Create a shared lib from an object file
-- $ gcc -shared -o libfoo.so foo.o
--
-- 3. Load with dlopen/dlsym

-- Note: accelerate use unsafePerformIO so the interface of
-- e.g. accelerate-llvm-native looks pure
-- In this example library we rather be honest about where IO occurs.

-- | run Native backend.
nativeCodeRuntime :: Tm -> IO (Double -> Double)
nativeCodeRuntime tm = do
    let cName = "foobar3"
    root <- Dir.getCurrentDirectory
    so <- compileSharedObject root $ cProgram cName tm

    -- open shared library and load symbol.
    -- we do "leak" the reference, library stays open until program is terminated.
    dl  <- dlopen so [RTLD_NOW, RTLD_LOCAL]
    sym <- dlsym dl cName

    return (dynamicCall sym)

-- dynamic stub to call our foreign type
--
-- I'm not sure how accelarete-llvm-native eventually calls into C,
-- it could be by passing function pointers to some manually written C code
-- instead of asking GHC to create a stub.
--
foreign import ccall "dynamic"
    dynamicCall :: FunPtr (Double -> Double) -> Double -> Double

compileSharedObject :: FilePath -> String -> IO FilePath
compileSharedObject root code = do
    let cfile  = root </> "shared.c"
    let ofile  = root </> "shared.o"
    let sofile = root </> "shared.so"

    writeFile cfile code
    Proc.callProcess "gcc" ["-c", "-fpic", "-o", ofile, cfile]
    -- whether -lm is needed or not ~depends
    -- There is a comment about that in accelerate-llvm-native
    Proc.callProcess "gcc" ["-shared", "-o", sofile, ofile, "-lm"]

    return sofile

{-
  -- TODO: Unclear if -lm is necessary on Darwin too; let's add it
  -- just in case. (The -lm on Linux was added to properly declare
  -- dependency on libm, so that it gets pulled in even if the main
  -- executable is statically-linked and thus does not have a dynamic
  -- libm in its address space.)

  -- macos and linux add -lm, windows doesn't with a comment:
  -- no -lm necessary on windows
-}

-------------------------------------------------------------------------------
-- Example
-------------------------------------------------------------------------------

example :: Tm
example = Sin (Add Var (Lit 5))

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

singleton :: x -> [x]
singleton x = [x]
