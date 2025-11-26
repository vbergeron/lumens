package lumens
package fields

import org.apache.lucene.search.Query
import org.apache.lucene.document.DoubleField
import org.apache.lucene.index.IndexableField
import org.apache.lucene.document.DoublePoint
import org.apache.lucene.search.SortedNumericSelector
import org.apache.lucene.search.SortField

trait DoubleFieldDesc extends FieldDesc[Double]:
    def field(value: Double): IndexableField =
        DoubleField(name, value, store)

    def exact(value: Double): Query =
        DoubleField.newExactQuery(name, value)

    def range(min: Double, max: Double): Query =
        DoubleField.newRangeQuery(name, min, max)

    def ascending: SortField =
        DoubleField.newSortField(name, false, SortedNumericSelector.Type.MIN)

    def descending: SortField =
        DoubleField.newSortField(name, true, SortedNumericSelector.Type.MAX)

object DoubleFieldDesc:
    case class Transient(name: String) extends DoubleFieldDesc:
        def stored: Stored = Stored(name)

    case class Stored(name: String) extends DoubleFieldDesc, ReadableFieldDesc[Double]:
        def reader: StoredFieldReader[Double] = StoredFieldReader[Double]
