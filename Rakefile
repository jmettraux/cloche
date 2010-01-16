
task :default => [ :compile ]

task :compile do

  FileUtils.mkdir('ebin') unless File.exist?('ebin')

  sh 'erlc -o ebin src/*.erl'
  #sh 'erlc -o ebin src/cloche_utils.erl'

  sh 'erlc -o test test/*.erl'
end

task :clean do

  FileUtils.rm_rf('ebin')
  FileUtils.rm_rf('work_test')
  Dir['test/*.beam'].each { |beam| FileUtils.rm(beam) }
end

task :test => [ :clean, :compile ] do

  sh "erl -noshell -pa ebin -pa test -s cloche_utils_test test -s init stop"
  sh "erl -noshell -pa ebin -pa test -s cloche_test test -s init stop"
end

task :rtest => [ :clean, :compile ] do

  require 'test/test.rb'
end

task :tserve => [ :clean, :compile ] do

  sh "./start.erl 9000 htest"
end

