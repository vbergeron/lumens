package lumens.fields

import org.apache.lucene.index.IndexableField
import org.apache.lucene.document.KeywordField
import org.apache.lucene.search.Query
import org.apache.lucene.search.SortField
import org.apache.lucene.search.SortedSetSelector
import org.apache.lucene.document.Document

trait KeywordFieldDesc[Name <: String, A] extends FieldDesc[Name, A, String]:
    def fieldBase(value: String): IndexableField =
        KeywordField(name, value, store)

    def exact(value: A): Query =
        KeywordField.newExactQuery(name, forward(value))

    def ascending: SortField =
        KeywordField.newSortField(name, false, SortedSetSelector.Type.MIN)

    def descending: SortField =
        KeywordField.newSortField(name, true, SortedSetSelector.Type.MAX)

trait TransientKeywordFieldDesc[Name <: String, A] extends KeywordFieldDesc[Name, A]:
    def contramap[B](f: B => A): TransientKeywordFieldDesc[Name, B] =
        new TransientKeywordFieldDesc[Name, B]:
            def name: String         = TransientKeywordFieldDesc.this.name
            def forward: B => String = b => TransientKeywordFieldDesc.this.forward(f(b))

trait PersistentKeywordFieldDesc[Name <: String, A]
    extends KeywordFieldDesc[Name, A],
      PersistentFieldDesc[Name, A, String]:

    def readBase(doc: Document): Either[Throwable, String] =
        Right(doc.getField(name).storedValue().getStringValue()) // TODO beurk

    def iemap[B](f: A => Either[ReadError, B], g: B => A): PersistentKeywordFieldDesc[Name, B] =
        new PersistentKeywordFieldDesc[Name, B]:
            def name: String = PersistentKeywordFieldDesc.this.name

            def forward: B => String =
                b => PersistentKeywordFieldDesc.this.forward(g(b))

            def backward: String => Either[ReadError, B] =
                s => PersistentKeywordFieldDesc.this.backward(s).flatMap(f)
