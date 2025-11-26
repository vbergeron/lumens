package lumens.fields

import org.apache.lucene.search.Query
import org.apache.lucene.document.LongField
import org.apache.lucene.index.IndexableField
import org.apache.lucene.document.LongPoint
import lumens.fields.{FieldDesc, ReadableFieldDesc}
import lumens.fields.StoredFieldReader

trait LongFieldDesc extends FieldDesc[Long]:
    def field(value: Long): IndexableField =
        LongField(name, value, store)

    def exact(value: Long): Query =
        LongField.newExactQuery(name, value)

    def range(min: Long, max: Long): Query =
        LongField.newRangeQuery(name, min, max)

object LongFieldDesc:
    case class Transient(name: String) extends LongFieldDesc:
        def stored: Stored = Stored(name)

    case class Stored(name: String) extends LongFieldDesc, ReadableFieldDesc[Long]:
        def reader: StoredFieldReader[Long] = StoredFieldReader[Long]
