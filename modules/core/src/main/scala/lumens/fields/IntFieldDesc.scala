package lumens.fields

import org.apache.lucene.index.IndexableField
import org.apache.lucene.document.IntField
import org.apache.lucene.search.Query
import org.apache.lucene.search.SortField
import org.apache.lucene.document.Document
import org.apache.lucene.search.SortedNumericSelector

trait IntFieldDesc[Name <: String, A] extends FieldDesc[Name, A, Int]:
    def fieldBase(value: Int): IndexableField =
        IntField(name, value, store)

    def exact(value: A): Query =
        IntField.newExactQuery(name, forward(value))

    def range(min: A, max: A): Query =
        IntField.newRangeQuery(name, forward(min), forward(max))

    def ascending: SortField =
        IntField.newSortField(name, false, SortedNumericSelector.Type.MIN)

    def descending: SortField =
        IntField.newSortField(name, true, SortedNumericSelector.Type.MAX)

trait TransientIntFieldDesc[Name <: String, A] extends IntFieldDesc[Name, A]:
    def contramap[B](f: B => A): TransientIntFieldDesc[Name, B] =
        new TransientIntFieldDesc[Name, B]:
            def name: String      = TransientIntFieldDesc.this.name
            def forward: B => Int = b => TransientIntFieldDesc.this.forward(f(b))

trait PersistentIntFieldDesc[Name <: String, A] extends IntFieldDesc[Name, A], PersistentFieldDesc[Name, A, Int]:

    def readBase(doc: Document): Either[Throwable, Int] =
        Right(doc.getField(name).storedValue().getIntValue())

    def iemap[B](f: A => Either[ReadError, B], g: B => A): PersistentIntFieldDesc[Name, B] =
        new PersistentIntFieldDesc[Name, B]:
            def name: String = PersistentIntFieldDesc.this.name

            def forward: B => Int =
                b => PersistentIntFieldDesc.this.forward(g(b))

            def backward: Int => Either[ReadError, B] =
                i => PersistentIntFieldDesc.this.backward(i).flatMap(f)
