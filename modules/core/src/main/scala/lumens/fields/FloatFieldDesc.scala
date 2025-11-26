package lumens.fields

import org.apache.lucene.search.Query
import org.apache.lucene.document.FloatField
import org.apache.lucene.index.IndexableField
import org.apache.lucene.document.FloatPoint

trait FloatFieldDesc extends FieldDesc[Float]:
    def field(value: Float): IndexableField =
        FloatField(name, value, store)

    def exact(value: Float): Query =
        FloatField.newExactQuery(name, value)

    def range(min: Float, max: Float): Query =
        FloatField.newRangeQuery(name, min, max)

object FloatFieldDesc:
    case class Transient(name: String) extends FloatFieldDesc:
        def stored: Stored = Stored(name)

    case class Stored(name: String) extends FloatFieldDesc, ReadableFieldDesc[Float]:
        def reader: StoredFieldReader[Float] = StoredFieldReader[Float]
