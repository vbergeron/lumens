package lumens.query

import org.apache.lucene.search.{BooleanQuery as LBooleanQuery, BooleanClause, Query as LQuery}

object Query:
    def bool: BooleanQueryBuilder = BooleanQueryBuilder(new LBooleanQuery.Builder())

final case class BooleanQueryBuilder(private val builder: LBooleanQuery.Builder):

    /** Adds a must clause to the query. */
    def must(queries: LQuery*): BooleanQueryBuilder =
        copy(builder = queries.foldLeft(builder)((b, query) => b.add(query, BooleanClause.Occur.MUST)))

    /** Adds a should clause to the query. */
    def should(queries: LQuery*): BooleanQueryBuilder =
        copy(builder = queries.foldLeft(builder)((b, query) => b.add(query, BooleanClause.Occur.SHOULD)))

    /** Adds a filter clause to the query. */
    def filter(queries: LQuery*): BooleanQueryBuilder =
        copy(builder = queries.foldLeft(builder)((b, query) => b.add(query, BooleanClause.Occur.FILTER)))

    /** Adds a must not clause to the query. */
    def mustNot(queries: LQuery*): BooleanQueryBuilder =
        copy(builder = queries.foldLeft(builder)((b, query) => b.add(query, BooleanClause.Occur.MUST_NOT)))

    /** Sets the minimum number of should clauses that must be satisfied for the query to be true. */
    def minimumShouldMatch(n: Int): BooleanQueryBuilder = copy(builder = builder.setMinimumNumberShouldMatch(n))

    def &&(query: LQuery): BooleanQueryBuilder = must(query)

    def ||(query: LQuery): BooleanQueryBuilder = should(query)

    /** Builds the query. */
    def build(): LQuery = builder.build()
