
#
# testing cloche via http
#
# Tue Jan 12 13:17:16 JST 2010
#

require 'test/unit'
require 'fileutils'

require 'rubygems'

require 'patron'
require 'yajl'
require 'rufus-jig'


class GetTest < Test::Unit::TestCase

  def setup
    @h = Rufus::Jig::Http.new('127.0.0.1', 9000)
    FileUtils.mkdir_p('htest/person/ff')
    File.open('htest/person/ff/jeff.json', 'wb') do |f|
      f.write('{"_id":"jeff","type":"person","eyes":"green","_rev":2}')
    end
  end
  def teardown
    @h.close
  end

  def test_get

    assert_equal(
      { '_id' => 'jeff', 'type' => 'person', '_rev' => 2, 'eyes' => 'green' },
      @h.get('/person/jeff'))
  end
end

