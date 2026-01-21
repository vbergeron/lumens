package lumens.extract

import lumens.fields.PersistentFieldDesc
import lumens.internal.Zippable
import org.apache.lucene.document.Document
import scala.util.Either

trait ExtractOutput[A]:
    val fields: List[PersistentFieldDesc[?, ?, ?]]

    /** Decodes a Lucene Document into the output type A. */
    def decode(doc: Document): Either[Throwable, A]

    def and[B](other: ExtractOutput[B])(using z: Zippable[A, B]): ExtractOutput[z.Out] =
        val self = this
        new ExtractOutput[z.Out]:
            override val fields: List[PersistentFieldDesc[?, ?, ?]] = self.fields ++ other.fields

            override def decode(doc: Document): Either[Throwable, z.Out] =
                for
                    a <- self.decode(doc)
                    b <- other.decode(doc)
                yield z.zip(a, b)

def out[A](field: PersistentFieldDesc[?, A, ?]): ExtractOutput[A] =
    new ExtractOutput[A]:
        override val fields: List[PersistentFieldDesc[?, ?, ?]] = List(field)

        override def decode(doc: Document): Either[Throwable, A] =
            field.read(doc)
