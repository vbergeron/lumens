package lumens.query

import scala.collection.{Factory, mutable}
import org.apache.lucene.search.IndexSearcher
import org.apache.lucene.search.Sort
import org.apache.lucene.search.TopFieldDocs

import scala.collection.mutable
import org.apache.lucene.document.Document

import scala.util.Try
import lumens.query.QueryContext
import org.apache.lucene.search.Query as LQuery

/** A query intent describes how the query will be executed and how the results will be transformed into the output
  * type.
  */
sealed trait QueryIntent[C[_], A](context: QueryContext[A])(using factory: Factory[A, C[A]]):
    type Output = C[A]

    /** Given a searcher, returns the results of the query. */
    def getResults: IndexSearcher => Either[Throwable, C[A]] =
        searcher =>
            Try:
                val topDocs: TopFieldDocs             = getTopDocs(searcher)
                val builder: mutable.Builder[A, C[A]] = factory.newBuilder

                topDocs.scoreDocs.foreach: hit =>
                    val doc: Document = searcher.storedFields().document(hit.doc)
                    context.output.decode(doc) match
                        case Right(value) => builder += value
                        case Left(error)  => throw error
                builder.result()
            .toEither

    /** Builds the sort for the query. */
    private def buildSort: Sort =
        if context.sorts.isEmpty then Sort.RELEVANCE
        else new Sort(context.sorts*)

    /** Builds the Lucene query for the query. */
    private def buildQuery: LQuery =
        context.query.translate

    /** Given a searcher, returns the top documents. */
    private def getTopDocs: IndexSearcher => TopFieldDocs =
        searcher => searcher.search(buildQuery, context.fetchSize, buildSort)

object QueryIntent:
    def apply[C[_], A](context: QueryContext[A])(using factory: Factory[A, C[A]]): QueryIntent[C, A] =
        new QueryIntent[C, A](context) {}
