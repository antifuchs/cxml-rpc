Feature: XML-RPC client

As a user of the cxml-rpc library, I want to call methods on the
remote endpoint.

Scenario: Call a method without arguments

  Given an external XML-RPC server is running
  Given the external server has a method "hello" defined following this scheme:
  | returns       |
  | "Hello World" |
  
  When I call the method "hello" with no arguments
  Then the return value should be the string "Hello World"

Scenario: Call a method with two integer arguments

  Given an external XML-RPC server is running
  Given the external server has a method "add" defined following this scheme:
  | a |   b | returns |
  | 1 |   2 |       3 |
  | 5 | 402 |     407 |
  
  When I call the method "add" with arguments (:integer 1 :integer 2)
  Then the return value should be the integer 3

  When I call the method "add" with arguments (:integer 5 :integer 402)
  Then the return value should be the integer 407
