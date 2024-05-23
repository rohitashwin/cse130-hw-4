{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Language.Nano.Parser (
    parseExpr
  , parseTokens
  ) where

import Language.Nano.Lexer
import Language.Nano.Types hiding (Nano (..))
import Control.Monad.Except
import Control.Exception

}

-- Entry point
%name top

-- Lexer structure
%tokentype { Token }

-- Parser monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Token Names
%token
    let   { LET _    }
    true  { TRUE _   }
    false { FALSE _  }
    in    { IN _     }
    if    { IF _     }
    then  { THEN _   }
    else  { ELSE _   }
    TNUM  { NUM _ $$ }
    ID    { ID _ $$  }
    '\\'  { LAM _    }
    '->'  { ARROW _  }
    '='   { EQL _    }
    '+'   { PLUS _   }
    '-'   { MINUS _  }
    '*'   { MUL _    }
    '&&'  { AND _    }
    '||'  { OR  _    }
    '=='  { EQB _    }
    '/='  { NEQ _    }
    '<'   { LESS _   }
    '<='  { LEQ _    }
    ':'   { COLON _  }
    '('   { LPAREN _ }
    ')'   { RPAREN _ }
    '['   { LBRAC _  }
    ']'   { RBRAC _  }
    ','   { COMMA _  }

-- Operators
%right in
%nonassoc '=' if then else
%right '->'
%left '||'
%left '&&'
%nonassoc '==' '/=' '<' '<='
%right ':'
%left '+' '-'
%left '*'
%%

Top  : ID '=' Expr                            { $3 }
     | Expr                                   { $1 }

Expr : let ID  '=' Expr in Expr               { ELet $2 $4 $6 }
     | let ID IDS '=' Expr in Expr            { ELet $2 (mkLam $3 $5) $7 }
     | '\\' ID '->' Expr                      { ELam $2 $4 }
     | if Expr then Expr else Expr            { EIf $2 $4 $6 }
     | Expr '||' Expr                         { EBin Or $1 $3 }
     | Expr '&&' Expr                         { EBin And $1 $3 }
     | Expr '==' Expr                         { EBin Eq $1 $3 }
     | Expr '/=' Expr                         { EBin Ne $1 $3 }
     | Expr '<' Expr                          { EBin Lt $1 $3 }
     | Expr '<=' Expr                         { EBin Le $1 $3 }
     | ListItems                              { $1 }

ListItems: '['']'                             { ENil }
      | ExprCont ':' ListItems                { EBin Cons $1 $3 }
      | '[' ListItems                         { $2 }
      | ListItems ']'                         { EBin Cons $1 ENil }
      | ExprCont ',' ListItems                { EBin Cons $1 $3 }
      | ExprCont                              { $1 }

ExprCont: ExprCont '+' ExprCont               { EBin Plus $1 $3 }
     | ExprCont '-' ExprCont                  { EBin Minus $1 $3 }
     | Mult                                   { $1 }

Mult : Mult '*' Value                         { EBin Mul $1 $3 }
     | Function                               { $1 }

Function : Function Value                     { EApp $1 $2 }
         | Value                              { $1 }

Value : '(' Expr ')'                          { $2 }
     | TNUM                                   { EInt $1 }
     | true                                   { EBool True }
     | false                                  { EBool False }
     | ID                                     { EVar $1 }

IDS : ID                                      { [$1] }
    | ID IDS                                  { $1:$2 } 

{
mkLam :: [Id] -> Expr -> Expr
mkLam []     e = e
mkLam (x:xs) e = ELam x (mkLam xs e)

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError []     = throwError "Unexpected end of Input"

parseExpr :: String -> Expr
parseExpr s = case parseExpr' s of
                Left msg -> throw (Error ("parse error:" ++ msg))
                Right e  -> e

parseExpr' input = runExcept $ do
   tokenStream <- scanTokens input
   top tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens


}
