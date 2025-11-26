package lumens.fields

import org.apache.lucene.search.Query
import org.apache.lucene.search.TermQuery
import org.apache.lucene.index.Term
import org.apache.lucene.document.KeywordField
import org.apache.lucene.index.IndexableField
import lumens.fields.{FieldDesc, ReadableFieldDesc}
import lumens.fields.StoredFieldReader

trait KeywordFieldDesc extends FieldDesc[String]:
    def field(value: String): IndexableField =
        KeywordField(name, value, store)

    def exact(value: String): Query =
        TermQuery(Term(name, value))

object KeywordFieldDesc:
    case class Transient(name: String) extends KeywordFieldDesc:
        def stored: Stored = Stored(name)

    case class Stored(name: String) extends KeywordFieldDesc, ReadableFieldDesc[String]:
        def reader: StoredFieldReader[String] = StoredFieldReader[String]
