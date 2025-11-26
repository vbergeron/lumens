package lumens.fields

import org.apache.lucene.util.BytesRef
import org.apache.lucene.document.Document
import org.apache.lucene.document.StoredValue
import org.apache.lucene.document.StoredField

trait StoredFieldWriter[A]:
    def write(name: String, value: A): StoredField

    def contramap[B](f: B => A): StoredFieldWriter[B] =
        new StoredFieldWriter[B]:
            def write(name: String, value: B): StoredField =
                StoredFieldWriter.this.write(name, f(value))

object StoredFieldWriter:
    def apply[A](using ev: StoredFieldWriter[A]): StoredFieldWriter[A] = ev

    given StoredFieldWriter[String] with
        def write(name: String, value: String): StoredField =
            StoredField(name, value)

    given StoredFieldWriter[Int] with
        def write(name: String, value: Int): StoredField =
            StoredField(name, value)

    given StoredFieldWriter[Long] with
        def write(name: String, value: Long): StoredField =
            StoredField(name, value)

    given StoredFieldWriter[Float] with
        def write(name: String, value: Float): StoredField =
            StoredField(name, value)

    given StoredFieldWriter[Double] with
        def write(name: String, value: Double): StoredField =
            StoredField(name, value)

    given StoredFieldWriter[BytesRef] with
        def write(name: String, value: BytesRef): StoredField =
            StoredField(name, value)
