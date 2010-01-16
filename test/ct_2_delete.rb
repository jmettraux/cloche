
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
    @h.put(
      '/person/jami',
      { '_id' => 'jami', 'type' => 'person', 'eyes' => 'blue' },
      { :content_type => :json })
    @d = @h.get('/person/jami')
  end
  def teardown
    @h.delete('/person')
    @h.close
  end

  def test_delete_type

    @h.delete('/person')

    assert_equal(false, File.exist?('htest/person/mi/jami.json'))
  end

  def test_delete_missing_rev

    assert_raise Rufus::Jig::HttpError do
      @h.delete('/person/jami')
    end

    assert_equal(true, File.exist?('htest/person/mi/jami.json'))
  end

  def test_delete

    r = @h.delete('/person/jami?rev=0')

    assert_equal true, r['ok']
    assert_equal false, File.exist?('htest/person/mi/jami.json')
  end

  def test_delete_in_path

    r = @h.delete('/person/jami/0')

    assert_equal true, r['ok']
    assert_equal false, File.exist?('htest/person/mi/jami.json')
  end

  def test_delete_missing

    r = @h.delete('/person/john?rev=0')

    assert_equal true, r['ok']
  end

  def test_delete_wrong_rev

    r = @h.delete('/person/jami?rev=7')

    assert_equal @d, r
  end
end

