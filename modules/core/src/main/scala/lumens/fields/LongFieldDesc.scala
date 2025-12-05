package lumens.fields

import org.apache.lucene.index.IndexableField
import org.apache.lucene.document.LongField
import org.apache.lucene.search.Query
import org.apache.lucene.search.SortField
import org.apache.lucene.document.Document
import org.apache.lucene.search.SortedNumericSelector

trait LongFieldDesc[Name <: String, A] extends FieldDesc[Name, A, Long]:
    def fieldBase(value: Long): IndexableField =
        LongField(name, value, store)

    def exact(value: A): Query =
        LongField.newExactQuery(name, forward(value))

    def range(min: A, max: A): Query =
        LongField.newRangeQuery(name, forward(min), forward(max))

    def ascending: SortField =
        LongField.newSortField(name, false, SortedNumericSelector.Type.MIN)

    def descending: SortField =
        LongField.newSortField(name, true, SortedNumericSelector.Type.MAX)

trait TransientLongFieldDesc[Name <: String, A] extends LongFieldDesc[Name, A]:
    def contramap[B](f: B => A): TransientLongFieldDesc[Name, B] =
        new TransientLongFieldDesc[Name, B]:
            def name: String       = TransientLongFieldDesc.this.name
            def forward: B => Long = b => TransientLongFieldDesc.this.forward(f(b))

trait PersistentLongFieldDesc[Name <: String, A] extends LongFieldDesc[Name, A], PersistentFieldDesc[Name, A, Long]:

    def readBase(doc: Document): Either[Throwable, Long] =
        Right(doc.getField(name).storedValue().getLongValue())

    def iemap[B](f: A => Either[ReadError, B], g: B => A): PersistentLongFieldDesc[Name, B] =
        new PersistentLongFieldDesc[Name, B]:
            def name: String = PersistentLongFieldDesc.this.name

            def forward: B => Long =
                b => PersistentLongFieldDesc.this.forward(g(b))

            def backward: Long => Either[ReadError, B] =
                l => PersistentLongFieldDesc.this.backward(l).flatMap(f)
