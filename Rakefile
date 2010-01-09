
task :default => [ :compile ]

task :compile do

  sh "erlc -o lib src/*.erl"
  sh "erlc -o test test/*.erl"
end

task :clean do

  sh "rm lib/*.beam"
  sh "rm test/*.beam"
end

task :test => [ :clean, :compile ] do

  sh "erl -noshell -pa lib -pa test -s cloche_test test -s init stop"
end

