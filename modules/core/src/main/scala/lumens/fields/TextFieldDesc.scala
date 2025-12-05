package lumens.fields

import org.apache.lucene.index.IndexableField
import org.apache.lucene.document.TextField
import org.apache.lucene.search.Query
import org.apache.lucene.search.TermQuery
import org.apache.lucene.search.PhraseQuery
import org.apache.lucene.document.Document
import org.apache.lucene.index.Term

trait TextFieldDesc[Name <: String, A] extends FieldDesc[Name, A, String]:
    def fieldBase(value: String): IndexableField =
        TextField(name, value, store)

    def term(termValue: String): Query =
        TermQuery(Term(name, termValue))

    def phrase(terms: String*): Query =
        val builder = PhraseQuery.Builder()
        terms.foreach(term => builder.add(Term(name, term)))
        builder.build()

    // Note: TextField is not sortable - would need a separate KeywordField for sorting
    // Note: No exact() or range() methods - TextField is tokenized, not for exact matching

trait TransientTextFieldDesc[Name <: String, A] extends TextFieldDesc[Name, A]:
    def contramap[B](f: B => A): TransientTextFieldDesc[Name, B] =
        new TransientTextFieldDesc[Name, B]:
            def name: String         = TransientTextFieldDesc.this.name
            def forward: B => String = b => TransientTextFieldDesc.this.forward(f(b))

trait PersistentTextFieldDesc[Name <: String, A] extends TextFieldDesc[Name, A], PersistentFieldDesc[Name, A, String]:

    def readBase(doc: Document): Either[Throwable, String] =
        Right(doc.getField(name).storedValue().getStringValue())

    def iemap[B](f: A => Either[ReadError, B], g: B => A): PersistentTextFieldDesc[Name, B] =
        new PersistentTextFieldDesc[Name, B]:
            def name: String = PersistentTextFieldDesc.this.name

            def forward: B => String =
                b => PersistentTextFieldDesc.this.forward(g(b))

            def backward: String => Either[ReadError, B] =
                s => PersistentTextFieldDesc.this.backward(s).flatMap(f)
