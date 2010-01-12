
#
# testing cloche via http
#
# Tue Jan 12 13:17:16 JST 2010
#

require 'test/unit'

require 'rubygems'

require 'patron'
require 'yajl'
require 'rufus-jig'


class GetTest < Test::Unit::TestCase

  def setup
    @h = Rufus::Jig::Http.new('127.0.0.1', 9000)
  end
  def teardown
    @h.close
  end

  def test_get

    assert_equal(
      {"_id"=>"toto", "type"=>"json", "_rev"=>2},
      @h.get('/person/toto'))
  end
end

