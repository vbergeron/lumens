package lumens
package fields

import org.apache.lucene.search.Query
import org.apache.lucene.document.DoubleField
import org.apache.lucene.index.IndexableField
import org.apache.lucene.document.DoublePoint

trait DoubleFieldDesc extends FieldDesc[Double]:
    def field(value: Double): IndexableField =
        DoubleField(name, value, store)

    def exact(value: Double): Query =
        DoubleField.newExactQuery(name, value)

    def range(min: Double, max: Double): Query =
        DoubleField.newRangeQuery(name, min, max)

object DoubleFieldDesc:
    case class Transient(name: String) extends DoubleFieldDesc:
        def stored: Stored = Stored(name)

    case class Stored(name: String) extends DoubleFieldDesc, ReadableFieldDesc[Double]:
        def reader: StoredFieldReader[Double] = StoredFieldReader[Double]
