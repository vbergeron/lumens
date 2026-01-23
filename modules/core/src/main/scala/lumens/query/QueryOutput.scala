package lumens.query

import lumens.fields.PersistentFieldDesc
import lumens.internal.Zippable
import org.apache.lucene.document.Document
import scala.util.Either

/** A query output describes the expected output type of a query. */
sealed trait QueryOutput[A]:
    /** The fields that are used to decode the results. */
    val fields: List[PersistentFieldDesc[?, ?, ?]]

    /** Decodes a Lucene Document into the output type A. */
    def decode(doc: Document): Either[Throwable, A]

    /** Combines two query outputs into a single query output by zipping the results. */
    def and[B](other: QueryOutput[B])(using z: Zippable[A, B]): QueryOutput[z.Out] =
        val self = this
        new QueryOutput[z.Out]:
            override val fields: List[PersistentFieldDesc[?, ?, ?]] = self.fields ++ other.fields

            override def decode(doc: Document): Either[Throwable, z.Out] =
                for
                    a <- self.decode(doc)
                    b <- other.decode(doc)
                yield z.zip(a, b)

object QueryOutput:
    def apply[A](field: PersistentFieldDesc[?, A, ?]): QueryOutput[A] =
        new QueryOutput[A]:
            override val fields: List[PersistentFieldDesc[?, ?, ?]] = List(field)

            override def decode(doc: Document): Either[Throwable, A] =
                field.read(doc)
