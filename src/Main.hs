{-# LANGUAGE TypeApplications, ApplicativeDo, GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Main where

import Control.Lens
import Data.Char
import Data.Coerce
import Data.Validation
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)

newtype Password = Password Text 
  deriving (Show, Eq)

newtype Username = Username Text 
  deriving (Show, Eq)

newtype Error = Error [Text]
  deriving (Show, Eq, Semigroup)

data User = User Username Password
  deriving (Show, Eq)

type Rule a = (a -> Validation Error a)

main' :: IO ()
main' = 
  makeUser @Either
    <$> (prompt "username" *> (Username <$> T.getLine)) 
    <*> (prompt "password" *> (Password <$> T.getLine)) 
  >>= print
  where prompt n = putStrLn ("Please enter a " <> n) >> putStr "> " 

main :: IO ()
main = do
  putStrLn "Please enter a username" >> putStr "> "
  username <- Username <$> T.getLine
  putStrLn "Please enter a password" >> putStr "> "
  password <- Password <$> T.getLine
  display username password

display:: Username -> Password -> IO ()
display name pwd = 
  case makeUser name pwd of 
    Failure err  -> T.putStr . T.unlines $ coerce err
    Success (User name password) ->
      T.putStrLn $ "Welcome, " <> coerce @Username name

makeUser :: Validate v => Username -> Password -> v Error User
makeUser username password = review _Validation $
  User <$> usernameErrors username
       <*> passwordErrors password

makeUser' :: Username -> Password -> Validation Error User
makeUser' username password = do  
  username2 <- usernameErrors username
  password2 <- passwordErrors password
  pure $ User username2 password2

validatePassword :: Rule Password
validatePassword (Password pwd) =
  case cleanWhitespace pwd of
    Failure err -> Failure err
    Success pwd2 -> requireAlphaNum pwd2 *> validatePasswordLength (Password pwd2)

passwordErrors :: Rule Password
passwordErrors pwd = 
  case validatePassword pwd of 
    Failure err -> Failure $ Error ["Invalid password: "] <> err
    success -> success

usernameErrors :: Rule Username
usernameErrors username = 
  case validateUsername username of
    Failure err -> Failure $ Error ["Invalid username: "] <> err
    success -> success
  
validateUsername :: Rule Username
validateUsername (Username username) =
  case cleanWhitespace username of
    Failure err -> Failure err
    Success username2 -> requireAlphaNum username2 *> checkUsernameLength username2

requireAlphaNum :: Rule Text
requireAlphaNum xs = 
  case T.all isAlphaNum xs of 
    False -> Failure $ Error ["Must be alphanumeric only"]
    True -> Success xs

cleanWhitespace :: Rule Text
cleanWhitespace xs = 
  case T.strip xs of 
    "" -> Failure $ Error ["Whitespace not allowed"]
    x -> Success x 
-- cleanWhitespace "" = Failure $ Error ["Whitespace not allowed"]
-- cleanWhitespace (x:xs) = 
--   case isSpace x of
--     True -> cleanWhitespace xs
--     False -> Success $ x:xs

validatePasswordLength :: Validate v => Password -> v Error Password
validatePasswordLength pwd = 
  validate (Error ["Password length invalid"]) checkPasswordLength (coerce @Password pwd)

checkPasswordLength :: Password -> Maybe Password
checkPasswordLength password@(Password pwd) = 
  case T.length pwd > 20 || T.length pwd < 3 of
    True -> Nothing
    False -> Just $ password

checkUsernameLength :: Text -> Validation Error Username
checkUsernameLength username = 
  case T.length username > 20 || T.length username < 3 of
    True -> Failure $ Error ["Username length invalid"]
    False -> Success $ Username username

checkLength :: (Show a) => (Text -> a) -> Int -> Text -> Validation Error a
checkLength tycon len xs = 
  case T.length xs > len || T.length xs < 3 of
    True -> Failure . Error $ ["Length must be < " <> (T.pack $ show len) <> " and > 3"]
    False -> Success $ tycon xs

-- START Tests Cases --

test :: IO ()
test = printTestResult $ 
  testCheckPasswordLength
  *> testCleanWhiteSpace
  *> testValidatePassword

testCheckPasswordLength = 
  assertEq 1 (validatePasswordLength (Password "")) (Failure $ Error ["Password length invalid"])
  *> assertEq 2 (validatePasswordLength (Password "1234")) 
                (Success $ Password "1234")
  *> assertEq 3 (validatePasswordLength (Password "aaaaaaaaaaaaaaaaaaaaaaaaaa")) 
                (Failure $ Error ["Password length invalid"])

testCleanWhiteSpace = 
  assertEq 4 (cleanWhitespace "   a") (Success "a")
  *> assertEq 5 (cleanWhitespace "     ") (Failure $ Error ["Whitespace not allowed"])

testValidatePassword = do
  assertEq 6 (validatePassword $ Password "mypass") (Success $ Password "mypass")

-- END Tests Cases --

-- START Test Framework -- 
printTestResult :: Validation Error () -> IO ()
printTestResult (Failure error) = putStrLn $ show error
printTestResult (Success ())   = putStrLn "All tests passed!"

assertEq :: (Eq a, Show a) => Int -> a -> a -> Validation Error ()
assertEq n actual expected =
  case actual == expected of 
    True -> Success ()
    False -> Failure . Error $ [T.unlines 
      [ "Test " <> (T.pack $ show n)
      , "  Expected:  " <> (T.pack $ show expected)
      , "  But got:   " <> (T.pack $ show actual)]]

-- END Test Framework -- 

reverseLine :: IO ()
reverseLine = do
  line <- getLine
  print $ reverse line

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing f = Nothing
bindMaybe (Just x) f = f x 
