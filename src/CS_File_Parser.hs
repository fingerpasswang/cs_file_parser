{-# LANGUAGE BangPatterns #-}

import Control.Applicative hiding ((<|>), many, optional)
import Control.Monad (MonadPlus(..), ap)
import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Char
import Numeric (readHex, readDec, readSigned, readFloat, readInt)
import Control.Monad (liftM2, liftM3, liftM4)
import System.IO (Handle, hSetEncoding, stdout, utf8)
import Data.Char(isLower, isUpper, isAlpha, isAlphaNum)
import qualified Data.ByteString.Char8 as C
import System.Directory (doesFileExist)
import System.Environment (getArgs, getProgName)

data CSExp = 
	UsingExp String
	| NamespaceExp String [CSExp]
	| ClassExp String [String] [CSExp]
	| FuncExp String [String] String CSExp [CSExp]
	| FormalArgsExp [(String,String,String)]
	| CommentExp String
	| StatementExp CSStatement
		deriving (Eq, Ord, Show)
		
data CSStatement = 
	NormalStatement String
	| Clause String [CSStatement]
		deriving (Eq, Ord, Show)
		
--newtype Parser = CharParser ()

listplus :: [Parser a] -> Parser a
listplus lst = foldr (<|>) mzero (map try lst)

star   :: Parser a -> Parser [a]
star p = star_p
	where 
		star_p = try plus_p <|> (return []) <?> "star_p"
		plus_p = (:) <$> p <*> star_p <?> "plus_p"
		
plus   :: Parser a -> Parser [a]
plus p = plus_p
    where
        star_p = plus_p <|> (return [])
        plus_p = (:) <$> p <*> star_p
		
-- help combinators begin
p_junk :: Parser ()
p_junk =  spaces 

p_parse   :: Parser a -> Parser a
p_parse p =  p_junk *> p

p_token   :: Parser a -> Parser a
p_token p =  p <* p_junk
			 <?> "Token"
			 
-- help combinators end

p_word :: Parser String
p_word =  p_token varname 
	where varname = (:) <$> satisfy isAlpha
						<*> many (listplus [(satisfy isAlphaNum), (char '_'), (char '.')])
						<?> "p_word"

p_exp :: Parser CSExp	
p_exp =  listplus [p_using_exp, p_namespace_exp, p_class_exp, p_func_exp, p_comment_exp, p_statement_exp]
		 <?> "p_exp"

p_using_exp :: Parser CSExp
p_using_exp =  UsingExp 
			<$  p_token (string "using")
			<*> p_token p_word
			<*  char ';'
			<?> "p_using_exp"
			
p_namespace_exp :: Parser CSExp
p_namespace_exp =  NamespaceExp
				<$  p_parse (string "namespace")
				<*> p_parse p_word
				<*  p_parse (char '{')
				<*> star (p_parse p_exp)
				<*  p_parse (char '}')
				<?> "p_namespace_exp" 
				
p_attr :: Parser String
p_attr =  listplus (map (p_token.string) ["public", "private", "protected", "static", "internal"])
		  <?> "p_attr"

p_between 		:: Char -> Char -> Parser a -> Parser a
p_between l r p =   char l 
				 *>	(spaces *> p <* spaces)
				<*  char r
				<?> "p_between"
 
p_between_brace :: Parser a -> Parser a
p_between_brace p = p_between '{' '}' p

p_between_bracket :: Parser a -> Parser a
p_between_bracket p = p_between '(' ')' p <?> "p_between_bracket"

p_class_exp :: Parser CSExp
p_class_exp =  (flip ClassExp) 
			<$> many p_attr
			<*  p_parse (string "class")
			<*> p_parse p_word
			<*> p_between_brace (star (p_parse p_exp))
			<?> "p_class_exp" 
			
p_seperate      :: Parser a -> Char -> Parser [a]
p_seperate p ch =  (:) <$> p
					   <*> star (p_parse p_rest)
	where p_rest =   char ch *> p
				
p_formal_args_exp :: Parser CSExp
p_formal_args_exp =  FormalArgsExp
					 <$> listplus[(p_seperate p_formal_arg_exp ',') , return []] 
					 <?> "p_formal_args_exp"
 
p_formal_arg_exp :: Parser (String, String, String)
p_formal_arg_exp =  (,,) 
					<$> p_parse p_word 
					<*> p_parse p_word  
					<*> listplus [assign, (return [])] 
					<?> "p_formal_arg_exp"
	where assign =  (p_parse (char '=')) *> (p_parse p_literal)
	
p_literal :: Parser String
p_literal =  listplus [(plus (satisfy isAlphaNum)), p_word]

--p_assign_exp :: Parser (String,String)
--p_assign_exp =  (,)
--			<$>	(p_token p_word)
--			<*	(p_token (char '='))
--			<*> (p_token p_literal)
--			<?> "p_assign_exp"
	 
p_not_sat_string    :: Char -> Parser String
p_not_sat_string ch =  plus (satisfy (\c -> c /= ch))

p_normal_statement :: Parser CSStatement
p_normal_statement = NormalStatement 
			<$> p_statement_test
			<?> "p_statement"
	
p_statement_test :: Parser String
p_statement_test =  concat <$> (star substatement)
				 <*  char ';'
	where substatement = 
		listplus [(plus (noneOf "{};")), with_brace]
		where with_brace = (++)
						   <$> (return "{")
						   <*> with_brace_2 (p_between_brace (star (noneOf "{}")))
			where with_brace_2 p = (++)
								   <$> p 
								   <*> (return "}")
			
p_clause_statement :: Parser CSStatement
p_clause_statement =  Clause 
			<$> plus (noneOf "{}")
			<*  char '{'
			<*> star (p_parse p_statement)
			<*  p_parse (char '}')
			<?> "p_clause_statement"

p_statement_exp :: Parser CSExp
p_statement_exp =  StatementExp 
				<$> p_statement
				<?> "p_statement_exp"

p_statement :: Parser CSStatement
p_statement =  listplus [p_normal_statement, p_clause_statement]
				<?> "p_statement"
			
p_func_exp :: Parser CSExp	
p_func_exp =  (\a b c d e -> FuncExp c a b d e)
		   <$> star p_attr
		   <*> p_parse p_word
		   <*> p_parse p_word
		   <*> p_parse (p_between_bracket p_formal_args_exp)
		   <*> p_parse (p_between_brace (star (p_parse p_exp)))
		   <?> "p_func_exp" 
			
p_eol :: Parser String
p_eol =   try (string "\r\n")
		   <|> try (string "\n\r")
		   <|> string "\n"
		   <|> string "\r"
		   <?> "p_eol"
			
p_comment_exp :: Parser CSExp
p_comment_exp =   try (oneline) 
			  <|> multiline 
			  <?> "p_comment_exp"
	where 
		oneline 	= CommentExp
					<$  p_parse (string "//") 
					<*> star (noneOf "\n\r") 
					<*  p_eol 
					<?> "oneline"
		multiline   = CommentExp
					<$  p_parse (string "/*")
					<*> star (noneOf "/*") 
					<*  string "*/"
					<?> "multiline"

test :: Either ParseError [CSStatement]
test =  parse (p_between_brace (star (p_parse p_statement))) "" "{123;123;}" 			
			
test2 :: Either ParseError CSExp
test2 =  parse p_func_exp "" "public static void Show(uint id = 4444) {123;123;}"
			
test3 :: Either ParseError CSExp
test3 =  parse p_exp "" "public static void Show(uint id = 4444, int a, string ttt) {123;123;if (n > 00) 123;if (n == 0){}}"			
		
p_main :: Parser [CSExp]
p_main = plus (p_token (p_parse p_exp))

handle_exp :: CSExp -> String
handle_exp (UsingExp _) = ""
handle_exp (NamespaceExp _ exps) = handle_exps exps
handle_exp (ClassExp _ _ exps) = handle_exps exps
handle_exp (FuncExp name _ _ formal_args _) = name ++ (handle_exp formal_args) ++ ";\n"
handle_exp (FormalArgsExp (first_arg:rest_args)) = "(" ++ (handle_arg first_arg) ++ (handle_rest_args rest_args) ++ ")"
	where 
		handle_arg (_,arg_name,_) = arg_name
		handle_rest_args ((_,first_name,_):r) = "," ++ first_name ++ (handle_rest_args r)
		handle_rest_args [] = ""
handle_exp (FormalArgsExp []) = "()"
handle_exp (CommentExp _) = ""
handle_exp (StatementExp _) = ""

handle_exps :: [CSExp] -> String
handle_exps exps =  (concat (map handler exps))
	where handler = handle_exp
	
handle_CS 				:: (Either ParseError [CSExp]) -> String
handle_CS (Right exps)  =  handle_exps exps

parse_CS :: FilePath -> IO ()
parse_CS file = do
    context <- C.readFile file
    if C.null context
    then putStrLn $ file ++ " is not exist"
    else putStrLn $ handle_CS (parse p_main "" (C.unpack context))
	
main :: IO ()
main = hSetEncoding stdout utf8 >> getArgs >>= \args -> parse_CS $ head args
