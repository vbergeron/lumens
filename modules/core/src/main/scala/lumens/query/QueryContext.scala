package lumens.query

import lumens.query.Query
import lumens.query.QueryOutput
import scala.collection.Factory
import lumens.query.QueryIntent
import org.apache.lucene.search.SortField

/** A query context contains all the settings for a query such as output, the query itself, the sorts, and the fetch
  * size.
  */
final case class QueryContext[A](output: QueryOutput[A], query: Query, sorts: List[SortField], fetchSize: Int):

    /** Adds the given sorts to the query context. */
    def sort(s: SortField*): QueryContext[A] = copy(sorts = sorts ++ s.toList)

    /** Sets the fetch size for the query context. */
    def withFetchSize(fetchSize: Int): QueryContext[A] = copy(fetchSize = fetchSize)

    /** Converts the query context to a query intent. */
    def into[C[_]](using Factory[A, C[A]]): QueryIntent[C, A] = QueryIntent(this)

object QueryContext:
    private val defaultFetchSize: Int = 100

    /** Creates a default query context with the given query output and query. Sorts are empty and the fetch size is the
      * default.
      */
    def apply[A](output: QueryOutput[A], query: Query): QueryContext[A] =
        QueryContext(output, query, List.empty, defaultFetchSize)
