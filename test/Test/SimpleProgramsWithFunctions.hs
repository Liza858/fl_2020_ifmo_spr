{-# LANGUAGE QuasiQuotes #-}

module Test.SimpleProgramsWithFunctions where

import AST
import Combinators
import Test.Helper
import LLang
import Test.Tasty.HUnit (Assertion, assertBool, (@?=))
import Text.RawString.QQ


fromResult pr =
    let ~(Success _ _ result) = runParser parseProg pr in result

psome = tail [r|
def main(arg) {
    x = some(42);
    write(x);
};
def some(n) {
    return n;
};
|]

unit_some :: Assertion
unit_some = do
    -- wtf haskell identation lol xdd
    let mainContents = Seq [
                Assign "x" (FunctionCall "some" [Num 42]),
                Write (Ident "x")
            ]
    fromResult psome @?= Program {
        functions = [
            Function "main" ["arg"] mainContents,
            Function "some" ["n"] (Seq [
                Return (Ident "n")
            ])
        ],
        main = mainContents
    }


pfac = tail [r|
def main(arg) {
    read(n);
    write(fac(n));
};
def fac(n) {
    if (n<=1) {
        return 1;
    }
    else {
        return fac(n-1)*fac(n-2);
    };
};
|]

unit_fac :: Assertion
unit_fac = do
    let mainContents = Seq [
                Read "n",
                Write (FunctionCall "fac" [Ident "n"])
            ]
    fromResult pfac @?= Program {
        functions = [
            Function "main" ["arg"] mainContents,
            Function "fac" ["n"] (Seq [
                If (BinOp Le (Ident "n") (Num 1))
                    (Seq [Return (Num 1)])
                    (Seq [Return
                        (BinOp Mult
                            (FunctionCall "fac" [BinOp Minus (Ident "n") (Num 1)])
                            (FunctionCall "fac" [BinOp Minus (Ident "n") (Num 2)]))])
            ])
        ],
        main = mainContents
    }

psum = tail [r|
def main(arg) {
    read(n);
    acc = 0;
    while (n>=0) {
        read(value)
        acc = sum(acc, value);
        n = n-1;
    };
    write(acc);
};
def sum(left, right) {
    return left+right;
};
|]

unit_sum :: Assertion
unit_sum = do
    let mainContents = Seq [
                Read "n",
                Assign "acc" (Num 0),
                While (BinOp Ge (Ident "n") (Num 0)) (Seq [
                    Read "value",
                    Assign "acc" (FunctionCall "sum" [Ident "acc", Ident "value"]),
                    Assign "n" (BinOp Minus (Ident "n") (Num 1))
                ]),
                Write (Ident "acc")
            ]
    fromResult psum @?= Program {
        functions = [
            Function "main" ["arg"] mainContents,
            Function "sum" ["left", "right"] (Seq [
                Return (BinOp Plus (Ident "left") (Ident "right"))
            ])
        ],
        main = mainContents
    }
