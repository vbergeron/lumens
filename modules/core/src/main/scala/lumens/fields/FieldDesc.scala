package lumens
package fields

import org.apache.lucene.document.Field
import org.apache.lucene.index.IndexableField
import org.apache.lucene.document.Document

trait FieldDesc[A]:
    def name: String

    def store: Field.Store = Field.Store.NO

    def field(value: A): IndexableField

trait ReadableFieldDesc[A] extends FieldDesc[A]:
    def reader: StoredFieldReader[A]

    override def store: Field.Store = Field.Store.YES

    def read(doc: Document): Either[ReadError, A] =
        reader.read(doc, name)

object FieldDesc:
    def keyword(name: String): KeywordFieldDesc.Transient =
        KeywordFieldDesc.Transient(name)

    def int(name: String): IntFieldDesc.Transient =
        IntFieldDesc.Transient(name)

    def long(name: String): LongFieldDesc.Transient =
        LongFieldDesc.Transient(name)

    def float(name: String): FloatFieldDesc.Transient =
        FloatFieldDesc.Transient(name)

    def double(name: String): DoubleFieldDesc.Transient =
        DoubleFieldDesc.Transient(name)
