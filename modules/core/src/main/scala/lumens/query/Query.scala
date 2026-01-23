package lumens.query

import org.apache.lucene.search.{BooleanQuery as LBooleanQuery, BooleanClause, Query as LQuery}
import lumens.fields.PersistentFieldDesc
import org.apache.lucene.search.SortField
import scala.collection.Factory
import lumens.internal.MonadError
import org.apache.lucene.search.IndexSearcher
import org.apache.lucene.search.Sort
import org.apache.lucene.search.TopFieldDocs
import scala.collection.mutable.Builder
import org.apache.lucene.document.Document
import scala.util.Try

/** A query is a Lucene query. It can be a standalone query or a clause. */
sealed trait Query:
    /** Translates the query to a Lucene query. */
    def translate: LQuery

    /** Creates a query context with the given query output. */
    def out[A](output: QueryOutput[A]): QueryContext[A] =
        QueryContext[A](output, this)

object Query:
    /** A standalone query is a single Lucene query. */
    final class Standolone(query: LQuery) extends Query:
        def translate: LQuery = query

    /** A clause is a Lucene BooleanQuery. */
    final case class Clauses(private val builder: LBooleanQuery.Builder) extends Query:
        def must(queries: Query*): Clauses      = copy(builder = aggQueries(queries, BooleanClause.Occur.MUST))
        def should(queries: Query*): Clauses    = copy(builder = aggQueries(queries, BooleanClause.Occur.SHOULD))
        def filter(queries: Query*): Clauses    = copy(builder = aggQueries(queries, BooleanClause.Occur.FILTER))
        def mustNot(queries: Query*): Clauses   = copy(builder = aggQueries(queries, BooleanClause.Occur.MUST_NOT))
        def minimumShouldMatch(n: Int): Clauses = copy(builder = builder.setMinimumNumberShouldMatch(n))
        def translate: LQuery                   = builder.build()

        private def aggQueries(queries: Seq[Query], occur: BooleanClause.Occur): LBooleanQuery.Builder =
            queries.foldLeft(builder)((b, query) => b.add(query.translate, occur))

    /** Creates a new clauses query. */
    def clauses: Clauses = Clauses(new LBooleanQuery.Builder())

    /** Creates a new standalone query. */
    def of(query: LQuery): Query = Standolone(query)
