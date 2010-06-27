require 'cucumber'
require 'spec'

require 'xmlrpc/client'
require 'xmlrpc/server'
require 'clucumber'

# Constants:
PORT = 11317
BEANSTALK_CONNSPEC = "localhost:#{PORT}"

# Programs we need:

unless File.exist?(File.expand_path("../step_definitions/clucumber_override.wire", File.dirname(__FILE__)))
  begin
    @main_clucumber = ClucumberSubprocess.new(File.expand_path("../", File.dirname(__FILE__)),
                                              :port => 42428)
    at_exit do
      @main_clucumber.kill
    end

    @main_clucumber.start <<-LISP
      (load #p"#{File.expand_path("../../cxml-rpc.asd", File.dirname(__FILE__))}")
      (asdf:oos 'asdf:load-op :cxml-rpc)
    LISP
  rescue PTY::ChildExited
    puts(@main_clucumber && @main_clucumber.output)
  end
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
