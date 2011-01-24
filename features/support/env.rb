require 'cucumber'
require 'spec'

require 'xmlrpc/client'
require 'xmlrpc/server'
require 'clucumber'

# Constants:
PORT = 11317
BEANSTALK_CONNSPEC = "localhost:#{PORT}"

# Programs we need:

require 'clucumber'
begin
  ClucumberSubprocess.launch(File.expand_path("../", File.dirname(__FILE__)),
                             :port => 42428).listen <<-LISP
    (push #p"#{File.expand_path("../../", File.dirname(__FILE__))}/" asdf:*central-registry*)
  LISP
rescue PTY::ChildExited
  STDERR.puts(@main_clucumber && @main_clucumber.output)
end

After do
  if @server
    @server.shutdown
    @service_thread.exit

    @service_thread = @server = nil
  end
end

module CXMLRpcWorld
  def process_arg(val)
    case
    when val =~ /^"(.*)"$/
      $1
    when val =~ /^[0-9]+$/
      val.to_i
    else
      val
    end
  end
end

World(CXMLRpcWorld)
