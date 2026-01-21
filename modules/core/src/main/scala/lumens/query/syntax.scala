package lumens.query

import org.apache.lucene.search.Query as LQuery
import org.apache.lucene.search.BooleanQuery
import org.apache.lucene.search.BooleanClause
import lumens.query.Query.bool

extension (query: LQuery)
    def &&(other: LQuery): BooleanQueryBuilder =
        bool.must(query, other)

    def ||(other: LQuery): BooleanQueryBuilder =
        bool.should(query, other)
