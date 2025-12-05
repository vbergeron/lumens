package lumens.fields

import org.apache.lucene.index.IndexableField
import org.apache.lucene.document.DoubleField
import org.apache.lucene.search.Query
import org.apache.lucene.search.SortField
import org.apache.lucene.document.Document
import org.apache.lucene.search.SortedNumericSelector

trait DoubleFieldDesc[Name <: String, A] extends FieldDesc[Name, A, Double]:
    def fieldBase(value: Double): IndexableField =
        DoubleField(name, value, store)

    def exact(value: A): Query =
        DoubleField.newExactQuery(name, forward(value))

    def range(min: A, max: A): Query =
        DoubleField.newRangeQuery(name, forward(min), forward(max))

    def ascending: SortField =
        DoubleField.newSortField(name, false, SortedNumericSelector.Type.MIN)

    def descending: SortField =
        DoubleField.newSortField(name, true, SortedNumericSelector.Type.MAX)

trait TransientDoubleFieldDesc[Name <: String, A] extends DoubleFieldDesc[Name, A]:
    def contramap[B](f: B => A): TransientDoubleFieldDesc[Name, B] =
        new TransientDoubleFieldDesc[Name, B]:
            def name: String         = TransientDoubleFieldDesc.this.name
            def forward: B => Double = b => TransientDoubleFieldDesc.this.forward(f(b))

trait PersistentDoubleFieldDesc[Name <: String, A]
    extends DoubleFieldDesc[Name, A],
      PersistentFieldDesc[Name, A, Double]:

    def readBase(doc: Document): Either[Throwable, Double] =
        Right(doc.getField(name).storedValue().getDoubleValue())

    def iemap[B](f: A => Either[ReadError, B], g: B => A): PersistentDoubleFieldDesc[Name, B] =
        new PersistentDoubleFieldDesc[Name, B]:
            def name: String = PersistentDoubleFieldDesc.this.name

            def forward: B => Double =
                b => PersistentDoubleFieldDesc.this.forward(g(b))

            def backward: Double => Either[ReadError, B] =
                d => PersistentDoubleFieldDesc.this.backward(d).flatMap(f)
