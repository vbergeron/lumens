package lumens.fields

import org.apache.lucene.search.Query
import org.apache.lucene.document.IntField
import org.apache.lucene.index.IndexableField
import org.apache.lucene.search.SortedNumericSelector
import org.apache.lucene.search.SortField

trait IntFieldDesc extends FieldDesc[Int]:
    def field(value: Int): IndexableField =
        IntField(name, value, store)

    def exact(value: Int): Query =
        IntField.newExactQuery(name, value)

    def range(min: Int, max: Int): Query =
        IntField.newRangeQuery(name, min, max)

    def ascending: SortField =
        IntField.newSortField(name, false, SortedNumericSelector.Type.MIN)

    def descending: SortField =
        IntField.newSortField(name, true, SortedNumericSelector.Type.MAX)

object IntFieldDesc:
    case class Transient(name: String) extends IntFieldDesc:
        def stored: Stored = Stored(name)

    case class Stored(name: String) extends IntFieldDesc, ReadableFieldDesc[Int]:
        def reader: StoredFieldReader[Int] = StoredFieldReader[Int]
