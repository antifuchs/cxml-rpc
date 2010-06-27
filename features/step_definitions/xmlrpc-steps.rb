Given /^debugging the external server is activated$/ do
  @debugging = true
end

Given /^an external XML\-RPC server is running$/ do
  @server = XMLRPC::Server.new(8080)
  @service_thread = Thread.new {
    puts @server.serve()
  }
end

Given /^the external server has a method "([^\"]*)" defined following this scheme:$/ do |name, table|
  headers = table.headers
  n_arguments = headers.length - 1
  interface_table = {}
  table.rows.each do |row|
    args = row[0...n_arguments].map { |cell| process_arg(cell) }
    interface_table[args] = process_arg(row[n_arguments])
  end
  
  puts interface_table.inspect
  interface = Class.new do
    define_method(name) do |*args|
      interface_table[args]
    end
  end
  @server.add_handler("test", interface.new)
end
