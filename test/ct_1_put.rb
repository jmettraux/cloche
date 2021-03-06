
#
# testing cloche via http
#
# Tue Jan 12 21:26:00 JST 2010
#

require 'test/unit'
require 'fileutils'

require 'rubygems'

require 'patron'
require 'yajl'
require 'rufus-jig'


class PutTest < Test::Unit::TestCase

  def setup
    @h = Rufus::Jig::Http.new('127.0.0.1', 9000)
  end
  def teardown
    @h.delete('/person')
    @h.close
  end

  def test_put_new

    r = @h.put(
      '/person/jami',
      { '_id' => 'jami', 'type' => 'person', 'eyes' => 'blue' },
      { :raw => true, :content_type => :json })

    assert_equal 200, r.status

    assert_equal(
      { "_id" => "jami", "type" => "person", "eyes" => "blue", "_rev" => 0 },
      Rufus::Json.decode(File.read('htest/person/mi/jami.json')))
  end

  def test_put_wrong_rev
    flunk
  end

  def test_put
    flunk
  end
end

