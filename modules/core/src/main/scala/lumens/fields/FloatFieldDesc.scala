package lumens.fields

import org.apache.lucene.index.IndexableField
import org.apache.lucene.document.FloatField
import org.apache.lucene.search.Query
import org.apache.lucene.search.SortField
import org.apache.lucene.document.Document
import org.apache.lucene.search.SortedNumericSelector

trait FloatFieldDesc[Name <: String, A] extends FieldDesc[Name, A, Float]:
    def fieldBase(value: Float): IndexableField =
        FloatField(name, value, store)

    def exact(value: A): Query =
        FloatField.newExactQuery(name, forward(value))

    def range(min: A, max: A): Query =
        FloatField.newRangeQuery(name, forward(min), forward(max))

    def ascending: SortField =
        FloatField.newSortField(name, false, SortedNumericSelector.Type.MIN)

    def descending: SortField =
        FloatField.newSortField(name, true, SortedNumericSelector.Type.MAX)

trait TransientFloatFieldDesc[Name <: String, A] extends FloatFieldDesc[Name, A]:
    def contramap[B](f: B => A): TransientFloatFieldDesc[Name, B] =
        new TransientFloatFieldDesc[Name, B]:
            def name: String        = TransientFloatFieldDesc.this.name
            def forward: B => Float = b => TransientFloatFieldDesc.this.forward(f(b))

trait PersistentFloatFieldDesc[Name <: String, A] extends FloatFieldDesc[Name, A], PersistentFieldDesc[Name, A, Float]:

    def readBase(doc: Document): Either[Throwable, Float] =
        Right(doc.getField(name).storedValue().getFloatValue())

    def iemap[B](f: A => Either[ReadError, B], g: B => A): PersistentFloatFieldDesc[Name, B] =
        new PersistentFloatFieldDesc[Name, B]:
            def name: String = PersistentFloatFieldDesc.this.name

            def forward: B => Float =
                b => PersistentFloatFieldDesc.this.forward(g(b))

            def backward: Float => Either[ReadError, B] =
                fl => PersistentFloatFieldDesc.this.backward(fl).flatMap(f)
