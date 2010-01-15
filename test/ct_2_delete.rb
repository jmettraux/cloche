
#
# testing cloche via http
#
# Fri Jan 15 19:26:40 JST 2010
#

require 'test/unit'
require 'fileutils'

require 'rubygems'

require 'patron'
require 'yajl'
require 'rufus-jig'


class DeleteTest < Test::Unit::TestCase

  def setup
    @h = Rufus::Jig::Http.new('127.0.0.1', 9000)
  end
  def teardown
    @h.close
  end

  def test_delete_type

    @h.put(
      '/person/jami',
      { '_id' => 'jami', 'type' => 'person', 'eyes' => 'blue' },
      { :content_type => :json })

    @h.delete('/person')

    assert_equal(false, File.exist?('htest/person/mi/jami.json'))
  end
end

