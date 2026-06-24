# print.schema: basic functionality

    Code
      print(schema)
    Output
      <Schema>
      
      Type: list
      Required: field1, field2
      
      Fields:
      ├─ field1 [string]
      └─ field2 [int]

# print.schema: parameter works

    Code
      print(schema, max_depth = 0)
    Output
      <Schema>
      
      Type: list (null.ok, names="unique")
      Required: header, data
      
      Fields:
      ├─ header [list] (required: status, message)
      │    ... (2 more fields)
      └─ data [data_frame (null.ok, col.names="unique")] (subset of 3 fields)

---

    Code
      print(schema, max_depth = 1)
    Output
      <Schema>
      
      Type: list (null.ok, names="unique")
      Required: header, data
      
      Fields:
      ├─ header [list] (required: status, message)
      │  ├─ status [int]
      │  └─ message [string]
      └─ data [data_frame (null.ok, col.names="unique")] (subset of 3 fields)

---

    Code
      print(schema, max_depth = 2)
    Output
      <Schema>
      
      Type: list (null.ok, names="unique")
      Required: header, data
      
      Fields:
      ├─ header [list] (required: status, message)
      │  ├─ status [int]
      │  └─ message [string]
      └─ data [data_frame (null.ok, col.names="unique")] (subset of 3 fields)

---

    Code
      print(schema, show_constraints = TRUE)
    Output
      <Schema>
      
      Type: list (null.ok, names="unique")
      Required: header, data
      
      Fields:
      ├─ header [list] (required: status, message)
      │  ├─ status [int]
      │  └─ message [string]
      └─ data [data_frame (null.ok, col.names="unique")] (subset of 3 fields)

---

    Code
      print(schema, show_constraints = FALSE)
    Output
      <Schema>
      
      Type: list
      Required: header, data
      
      Fields:
      ├─ header [list]
      │  ├─ status [int]
      │  └─ message [string]
      └─ data [data_frame]

---

    Code
      print(schema, compact = FALSE)
    Output
      <Schema>
      
      Type: list
      Required: a, b, c, d, e, f, g, h, i, j

---

    Code
      print(schema, compact = TRUE)
    Output
      <Schema>
      
      Type: list
      Required: a, b, c, d, e ... (5 more)

# summary.schema

    Code
      result <- summary(schema)
    Output
      <Schema Summary>
      
      Root type:      list
      Total fields:   2
      Max depth:      1
      Required fields:2
      
      Required field names:
        field1, field2

---

    Code
      stats <- summary(schema)
    Output
      <Schema Summary>
      
      Root type:      list
      Total fields:   4
      Max depth:      2
      Required fields:2
      
      Required field names:
        header, data

# str.schema

    Code
      str(schema)
    Output
      <Schema>
      
      Type: list
      Required: field1, field2
      
      Fields:
      ├─ field1 [string]
      └─ field2 [int]

# print.schema

    Code
      print(schema)
    Output
      <Schema>
      
      Type: list
      
      Fields:
      └─ <dynamic fields> [string]

---

    Code
      print(schema)
    Output
      <Schema>
      
      Type: list
      Subset of: field1, field2, field3

---

    Code
      print(schema)
    Output
      <Schema>
      
      Type: list

# SCHEMA_RESPONSE

    Code
      print(SCHEMA_RESPONSE)
    Output
      <Schema>
      
      Type: list (null.ok, names="unique")
      Required: responseHeader, response, facet_counts, timestamp
      
      Fields:
      ├─ responseHeader [list (null.ok, names="unique")] (required: status, QTime, params)
      │  ├─ status [int]
      │  ├─ QTime [int]
      │  └─ params [list] (required: facet.field, df, q.alt, indent, echoParams, fl, start, fq, rows, q, tie, facet.limit, qf, facet.method, facet.mincount, facet, wt, facet.sort, shards, fields)
      │     ├─ facet.field [list (any.missing=FALSE)]
      │     ├─ df [string]
      │     ├─ q.alt [string]
      │     ├─ indent [choice("true", "false")]
      │     ├─ echoParams [string]
      │     ├─ fl [string]
      │     ├─ start [string]
      │     ├─ fq [character]
      │     ├─ rows [string]
      │     ├─ q [string]
      │     ├─ tie [string]
      │     ├─ facet.limit [string]
      │     ├─ qf [string]
      │     ├─ facet.method [string]
      │     ├─ facet.mincount [string]
      │     ├─ facet [choice("true", "false") (null.ok)]
      │     ├─ wt [choice("json", "xml")]
      │     ├─ facet.sort [choice("lex")]
      │     ├─ shards [string]
      │     └─ fields [character (any.missing=FALSE)]
      ├─ response [list] (required: numFound, start, maxScore, docs)
      │  ├─ numFound [int]
      │  ├─ start [int]
      │  ├─ maxScore [number]
      │  └─ docs [data_frame (null.ok, col.names="unique")] (subset of 111 fields)
      ├─ facet_counts [list (null.ok)]
      │  ├─ facet_fields [list (any.missing=FALSE)]
      │  └─ <dynamic fields> [list (rules=c("S1", "I1"))]
      └─ timestamp [string]

---

    Code
      stats <- summary(SCHEMA_RESPONSE)
    Output
      <Schema Summary>
      
      Root type:      list
      Total fields:   33
      Max depth:      3
      Required fields:4
      
      Required field names:
        responseHeader, response, facet_counts, timestamp

# SCHEMA_QUERY

    Code
      print(SCHEMA_QUERY)
    Output
      <Schema>
      
      Type: list
      Required: index_node, parameter
      
      Fields:
      ├─ index_node [string]
      └─ parameter [list (names="unique")] (required: project, activity_id, source_id, variable_id, variant_label, nominal_resolution, data_node, facets, fields, shards, replica, latest, distrib, limit, offset, format)
         ├─ project [list (null.ok, names="unique")] (required: value, negate)
         │  ├─ value [character (null.ok, any.missing=FALSE, unique)]
         │  └─ negate [flag]
         ├─ activity_id [list (null.ok, names="unique")] (required: value, negate)
         │  ├─ value [character (null.ok, any.missing=FALSE, unique)]
         │  └─ negate [flag]
         ├─ source_id [list (null.ok, names="unique")] (required: value, negate)
         │  ├─ value [character (null.ok, any.missing=FALSE, unique)]
         │  └─ negate [flag]
         ├─ variable_id [list (null.ok, names="unique")] (required: value, negate)
         │  ├─ value [character (null.ok, any.missing=FALSE, unique)]
         │  └─ negate [flag]
         ├─ variant_label [list (null.ok, names="unique")] (required: value, negate)
         │  ├─ value [character (null.ok, any.missing=FALSE, unique)]
         │  └─ negate [flag]
         ├─ nominal_resolution [list (null.ok, names="unique")] (required: value, negate)
         │  ├─ value [character (null.ok, any.missing=FALSE, unique)]
         │  └─ negate [flag]
         ├─ data_node [list (null.ok, names="unique")] (required: value, negate)
         │  ├─ value [character (null.ok, any.missing=FALSE, unique)]
         │  └─ negate [flag]
         ├─ facets [list (null.ok, names="unique")] (required: value, negate)
         │  ├─ value [character (null.ok, any.missing=FALSE, unique)]
         │  └─ negate [flag]
         ├─ shards [list (null.ok, names="unique")] (required: value, negate)
         │  ├─ value [character (null.ok, any.missing=FALSE, unique)]
         │  └─ negate [flag]
         ├─ dataset_id [list (null.ok, names="unique")] (required: value, negate)
         │  ├─ value [character (null.ok, any.missing=FALSE, unique)]
         │  └─ negate [flag]
         ├─ replica [list (null.ok, names="unique")] (required: value, negate)
         │  ├─ value [flag (null.ok)]
         │  └─ negate [flag]
         ├─ latest [list (null.ok, names="unique")] (required: value, negate)
         │  ├─ value [flag]
         │  └─ negate [flag]
         ├─ distrib [list (null.ok, names="unique")] (required: value, negate)
         │  ├─ value [flag]
         │  └─ negate [flag]
         ├─ limit [list (null.ok, names="unique")] (required: value, negate)
         │  ├─ value [count]
         │  └─ negate [flag]
         ├─ offset [list (null.ok, names="unique")] (required: value, negate)
         │  ├─ value [count]
         │  └─ negate [flag]
         ├─ type [list (null.ok, names="unique")] (required: value, negate)
         │  ├─ value [choice("Dataset")]
         │  └─ negate [flag]
         └─ format [list (null.ok, names="unique")] (required: value, negate)
            ├─ value [choice("application/solr+json")]
            └─ negate [flag]

---

    Code
      stats <- summary(SCHEMA_QUERY)
    Output
      <Schema Summary>
      
      Root type:      list
      Total fields:   53
      Max depth:      3
      Required fields:2
      
      Required field names:
        index_node, parameter

# SCHEMA_RESULT_DATASET

    Code
      print(SCHEMA_RESULT_DATASET)
    Output
      <Schema>
      
      Type: list
      Required: index_node, parameter, response
      
      Fields:
      ├─ index_node [string]
      ├─ parameter [list (names="unique")] (required: project, activity_id, source_id, variable_id, variant_label, nominal_resolution, data_node, facets, fields, shards, replica, latest, distrib, limit, offset, format)
      │  ├─ project [list (null.ok, names="unique")] (required: value, negate)
      │  │  ├─ value [character (null.ok, any.missing=FALSE, unique)]
      │  │  └─ negate [flag]
      │  ├─ activity_id [list (null.ok, names="unique")] (required: value, negate)
      │  │  ├─ value [character (null.ok, any.missing=FALSE, unique)]
      │  │  └─ negate [flag]
      │  ├─ source_id [list (null.ok, names="unique")] (required: value, negate)
      │  │  ├─ value [character (null.ok, any.missing=FALSE, unique)]
      │  │  └─ negate [flag]
      │  ├─ variable_id [list (null.ok, names="unique")] (required: value, negate)
      │  │  ├─ value [character (null.ok, any.missing=FALSE, unique)]
      │  │  └─ negate [flag]
      │  ├─ variant_label [list (null.ok, names="unique")] (required: value, negate)
      │  │  ├─ value [character (null.ok, any.missing=FALSE, unique)]
      │  │  └─ negate [flag]
      │  ├─ nominal_resolution [list (null.ok, names="unique")] (required: value, negate)
      │  │  ├─ value [character (null.ok, any.missing=FALSE, unique)]
      │  │  └─ negate [flag]
      │  ├─ data_node [list (null.ok, names="unique")] (required: value, negate)
      │  │  ├─ value [character (null.ok, any.missing=FALSE, unique)]
      │  │  └─ negate [flag]
      │  ├─ facets [list (null.ok, names="unique")] (required: value, negate)
      │  │  ├─ value [character (null.ok, any.missing=FALSE, unique)]
      │  │  └─ negate [flag]
      │  ├─ shards [list (null.ok, names="unique")] (required: value, negate)
      │  │  ├─ value [character (null.ok, any.missing=FALSE, unique)]
      │  │  └─ negate [flag]
      │  ├─ dataset_id [list (null.ok, names="unique")] (required: value, negate)
      │  │  ├─ value [character (null.ok, any.missing=FALSE, unique)]
      │  │  └─ negate [flag]
      │  ├─ replica [list (null.ok, names="unique")] (required: value, negate)
      │  │  ├─ value [flag (null.ok)]
      │  │  └─ negate [flag]
      │  ├─ latest [list (null.ok, names="unique")] (required: value, negate)
      │  │  ├─ value [flag]
      │  │  └─ negate [flag]
      │  ├─ distrib [list (null.ok, names="unique")] (required: value, negate)
      │  │  ├─ value [flag]
      │  │  └─ negate [flag]
      │  ├─ limit [list (null.ok, names="unique")] (required: value, negate)
      │  │  ├─ value [count]
      │  │  └─ negate [flag]
      │  ├─ offset [list (null.ok, names="unique")] (required: value, negate)
      │  │  ├─ value [count]
      │  │  └─ negate [flag]
      │  ├─ type [list (null.ok, names="unique")] (required: value, negate)
      │  │  ├─ value [choice("Dataset")]
      │  │  └─ negate [flag]
      │  └─ format [list (null.ok, names="unique")] (required: value, negate)
      │     ├─ value [choice("application/solr+json")]
      │     └─ negate [flag]
      └─ response [list (null.ok, names="unique")] (required: responseHeader, response, facet_counts, timestamp)
         ├─ responseHeader [list (null.ok, names="unique")] (required: status, QTime, params)
         │  ├─ status [int]
         │  ├─ QTime [int]
         │  └─ params [list] (required: df, q.alt, indent, echoParams, fl, start, fq, rows, q, tie, facet.limit, qf, facet.method, facet.mincount, wt, facet.sort, shards)
         │     ├─ df [string]
         │     ├─ q.alt [string]
         │     ├─ indent [choice("true", "false")]
         │     ├─ echoParams [string]
         │     ├─ fl [string]
         │     ├─ start [string]
         │     ├─ fq [character]
         │     ├─ rows [string]
         │     ├─ q [string]
         │     ├─ tie [string]
         │     ├─ facet.limit [string]
         │     ├─ qf [string]
         │     ├─ facet.method [string]
         │     ├─ facet.mincount [string]
         │     ├─ wt [choice("json", "xml")]
         │     ├─ facet.sort [choice("lex")]
         │     └─ shards [string]
         ├─ response [list] (required: numFound, start, maxScore, docs)
         │  ├─ numFound [int]
         │  ├─ start [int]
         │  ├─ maxScore [number]
         │  └─ docs [data_frame (null.ok, col.names="unique")] (subset of 111 fields)
         ├─ facet_counts [list (null.ok)]
         │  ├─ facet_fields [list (any.missing=FALSE)]
         │  └─ <dynamic fields> [list (rules=c("S1", "I1"))]
         └─ timestamp [string]

---

    Code
      stats <- summary(SCHEMA_RESULT_DATASET)
    Output
      <Schema Summary>
      
      Root type:      list
      Total fields:   84
      Max depth:      4
      Required fields:3
      
      Required field names:
        index_node, parameter, response

