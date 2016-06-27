package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)

    val leaf1 = Leaf('a', 2)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("weight of a single leaf") {
    new TestTrees {
      assert(weight(leaf1) === 2)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("chars of a single leaf") {
    new TestTrees {
      assert(chars(leaf1) === List('a'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times returns an empty list when no characters are provided") {
    assert(times(List()) === List())
  }

  test("times returns a list of one character when only one is provided") {
    assert(times(List('a')) === List(('a', 1)))
  }

  test("times returns a list of two characters when two is provided") {
    assert(times(List('a', 'b')) === List(('a', 1), ('b', 1)))
  }

  test("times returns a list of one character with correct count") {
    assert(times(List('a', 'a', 'a')) === List(('a', 3)))
  }

  test("times returns a list of multiple characters with correct count") {
    assert(times(List('a', 'b', 'a', 'b', 'b', 'c')) === List(('a', 2), ('b', 3), ('c', 1)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("encode a very short text should be identity") {
    new TestTrees {
      assert(encode(t1)("ab".toList) === List(0, 1))
    }
  }

  test("decode should decode a single char") {
    val codeTree = createCodeTree(List('a', 'b'))
    assert(decode(codeTree, List(0)) === List('a'))
  }

  test("decode should decode right character") {
    val codeTree = createCodeTree(List('a', 'b'))
    assert(decode(codeTree, List(1)) === List('b'))
  }

  test("decode should decode first character twice") {
    val codeTree = createCodeTree(List('a', 'b'))
    assert(decode(codeTree, List(0, 0)) === List('a', 'a'))
  }

  test("decode should decode second character twice") {
    val codeTree = createCodeTree(List('a', 'b'))
    assert(decode(codeTree, List(1, 1)) === List('b', 'b'))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

}
