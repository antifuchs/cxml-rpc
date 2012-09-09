Feature: Lazy XML-RPC encoders

  As a client of a shoddy xml-rpc encoder,
  I want to be able to read their requests and responses correctly,
  so that I can communicate with them.


Scenario: Implicit <string> types in <values>

  Given the following method response:
  """
<methodResponse><params><param><value>987lkjasd</value></param></params></methodResponse>
  """
  When I decode the method response
  Then the first value should be "987lkjasd"


Scenario: Implicit <string> types in <values> starting with whitespace

  Given the following method response:
"""
<methodResponse><params><param><value>
   987lkjasd
</value></param></params></methodResponse>
"""
  When I decode the method response
  Then the first value should be:
"""

   987lkjasd

"""


Scenario: Implicit <string> type starting with an XML entity

  Given the following method response:
  """
<methodResponse><params><param><value>&#32;987lkjasd</value></param></params></methodResponse>
  """
  When I decode the method response
  Then the first value should be " 987lkjasd"


Scenario: Indented, explicit type tags

  Given the following method response:
  """
<methodResponse><params><param><value>
   <string>987lkjasd</string>
</value></param></params></methodResponse>
  """
  When I decode the method response
  Then the first value should be "987lkjasd"
