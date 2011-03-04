require 'rubygems'
require 'rake'

begin
  require 'jeweler'
  Jeweler::Tasks.new do |gem|
    gem.name = "erlectricity"
    gem.rubyforge_project = "erlectricity"
    gem.summary = "A library to interface erlang and ruby through the erlang port system"
    gem.email = "tom@mojombo.com"
    gem.homepage = "http://github.com/mojombo/erlectricity"
    gem.authors = ["Scott Fleckenstein", "Tom Preston-Werner"]
    gem.require_paths = ["lib", "ext"]
    gem.files.include("ext")
    gem.extensions << 'ext/extconf.rb'

    # gem is a Gem::Specification... see http://www.rubygems.org/read/chapter/20 for additional settings
  end
rescue LoadError
  puts "Jeweler not available. Install it with: sudo gem install technicalpickles-jeweler -s http://gems.github.com"
end

task :test do
  require 'open3'
  require 'fileutils'

  puts "\nCleaning extension build files and running all specs in native ruby mode..."
  `rm -f ext/*.bundle` && puts("rm -f ext/*.bundle")
  `rm -f ext/*.o` && puts("rm -f ext/*.o")
  Open3.popen3("ruby test/spec_suite.rb") do |stdin, stdout, stderr|
    while !stdout.eof?
      print stdout.read(1)
    end
  end

  puts "\nRunning `make` to build extensions and rerunning decoder specs..."
  Dir.chdir('ext') { `make` }
  Open3.popen3("ruby test/decode_spec.rb") do |stdin, stdout, stderr|
    while !stdout.eof?
      print stdout.read(1)
    end
  end
end

begin
  require 'rcov/rcovtask'
  Rcov::RcovTask.new do |test|
    test.libs << 'test'
    test.pattern = 'test/**/*_test.rb'
    test.verbose = true
  end
rescue LoadError
  task :rcov do
    abort "RCov is not available. In order to run rcov, you must: sudo gem install spicycode-rcov"
  end
end


task :default => :test

require 'rake/rdoctask'
Rake::RDocTask.new do |rdoc|
  if File.exist?('VERSION.yml')
    config = YAML.load(File.read('VERSION.yml'))
    version = "#{config[:major]}.#{config[:minor]}.#{config[:patch]}"
  else
    version = ""
  end

  rdoc.rdoc_dir = 'rdoc'
  rdoc.title = "erlectricity #{version}"
  rdoc.rdoc_files.include('README*')
  rdoc.rdoc_files.include('lib/**/*.rb')
end