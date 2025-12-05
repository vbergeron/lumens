package lumens
package fields

import org.apache.lucene.document.Field
import org.apache.lucene.index.IndexableField
import org.apache.lucene.document.Document
import org.apache.lucene.search.TermQuery
import org.apache.lucene.search.Query
import org.apache.lucene.index.Term
import org.apache.lucene.document.KeywordField
import org.apache.lucene.search.SortField
import org.apache.lucene.search.SortedSetSelector

class ReadError(msg: String) extends Throwable(msg)

trait FieldDesc[Name <: String, A, Base]:
    def name: String
    def forward: A => Base
    def fieldBase(value: Base): IndexableField

    def store: Field.Store = Field.Store.NO

    def field(value: A): IndexableField =
        fieldBase(forward(value))

trait PersistentFieldDesc[Name <: String, A, Base] extends FieldDesc[Name, A, Base]:
    def backward: Base => Either[ReadError, A]
    def readBase(doc: Document): Either[Throwable, Base]

    override def store: Field.Store = Field.Store.YES

    def read(doc: Document): Either[Throwable, A] =
        readBase(doc).flatMap(backward)

object FieldDesc:
    // Keyword field constructors
    def keyword(fieldName: String): TransientKeywordFieldDesc[fieldName.type, String] =
        new TransientKeywordFieldDesc[fieldName.type, String]:
            def name: String              = fieldName
            def forward: String => String = identity

    def persistentKeyword(fieldName: String): PersistentKeywordFieldDesc[fieldName.type, String] =
        new PersistentKeywordFieldDesc[fieldName.type, String]:
            def name: String                                  = fieldName
            def forward: String => String                     = identity
            def backward: String => Either[ReadError, String] = Right(_)

    // Int field constructors
    def int(fieldName: String): TransientIntFieldDesc[fieldName.type, Int] =
        new TransientIntFieldDesc[fieldName.type, Int]:
            def name: String        = fieldName
            def forward: Int => Int = identity

    def persistentInt(fieldName: String): PersistentIntFieldDesc[fieldName.type, Int] =
        new PersistentIntFieldDesc[fieldName.type, Int]:
            def name: String                            = fieldName
            def forward: Int => Int                     = identity
            def backward: Int => Either[ReadError, Int] = Right(_)

    // Long field constructors
    def long(fieldName: String): TransientLongFieldDesc[fieldName.type, Long] =
        new TransientLongFieldDesc[fieldName.type, Long]:
            def name: String          = fieldName
            def forward: Long => Long = identity

    def persistentLong(fieldName: String): PersistentLongFieldDesc[fieldName.type, Long] =
        new PersistentLongFieldDesc[fieldName.type, Long]:
            def name: String                              = fieldName
            def forward: Long => Long                     = identity
            def backward: Long => Either[ReadError, Long] = Right(_)

    // Double field constructors
    def double(fieldName: String): TransientDoubleFieldDesc[fieldName.type, Double] =
        new TransientDoubleFieldDesc[fieldName.type, Double]:
            def name: String              = fieldName
            def forward: Double => Double = identity

    def persistentDouble(fieldName: String): PersistentDoubleFieldDesc[fieldName.type, Double] =
        new PersistentDoubleFieldDesc[fieldName.type, Double]:
            def name: String                                  = fieldName
            def forward: Double => Double                     = identity
            def backward: Double => Either[ReadError, Double] = Right(_)

    // Float field constructors
    def float(fieldName: String): TransientFloatFieldDesc[fieldName.type, Float] =
        new TransientFloatFieldDesc[fieldName.type, Float]:
            def name: String            = fieldName
            def forward: Float => Float = identity

    def persistentFloat(fieldName: String): PersistentFloatFieldDesc[fieldName.type, Float] =
        new PersistentFloatFieldDesc[fieldName.type, Float]:
            def name: String                                = fieldName
            def forward: Float => Float                     = identity
            def backward: Float => Either[ReadError, Float] = Right(_)

    // Text field constructors
    def text(fieldName: String): TransientTextFieldDesc[fieldName.type, String] =
        new TransientTextFieldDesc[fieldName.type, String]:
            def name: String              = fieldName
            def forward: String => String = identity

    def persistentText(fieldName: String): PersistentTextFieldDesc[fieldName.type, String] =
        new PersistentTextFieldDesc[fieldName.type, String]:
            def name: String                                  = fieldName
            def forward: String => String                     = identity
            def backward: String => Either[ReadError, String] = Right(_)
